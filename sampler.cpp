#include "sampler.h"
#include "memory.h"
#include "config.h"

#include <math.h>
#include <stdlib.h>
#include <memory.h>

FixedAllocator sampler_allocator;

#define SIN_TABLE

#ifdef SIN_TABLE
#define TABLE_SIZE 1024
float *sin_table;
#define SIN(x) sin_table[((int)((x * TABLE_SIZE) / (2*M_PI))) % TABLE_SIZE]
#else
#define SIN(x) sinf(x)
#endif

void sampler_init() {
#ifdef SIN_TABLE
  int ii;
  sin_table = (float*)malloc(sizeof(float) * TABLE_SIZE);

  for(ii = 0; ii < TABLE_SIZE; ++ii) {
    float factor = (float)ii/TABLE_SIZE;
    sin_table[ii] = sin(2 * M_PI * factor);
  }
#endif

  size_t max_sampler_size
    = MAX(sizeof(struct SinSampler_),
          MAX(sizeof(struct SawSampler_),
              sizeof(struct Filter_)));

  sampler_allocator = fixed_allocator_make(max_sampler_size,
                                           MAX_NUM_SAMPLERS,
                                           "sampler_allocator");
}

void sampler_free(void* obj) {
  fixed_allocator_free(sampler_allocator, obj);
}

long sampler_offset_(Sampler sampler, long sample) {
  return sample - START(sampler);
}

#define sampler_offset(sampler, sample) \
  sampler_offset_((Sampler)sampler, sample)

int16_t sin_sample(SinSampler sampler, long sample) {
  sample = sampler_offset(sampler, sample);
  return sampler->amp *
    SIN(sampler->phase + sampler->radians_per_sample * sample);
}

/** all phases are normalized phase values ranging from [0, 1)
 */

Sampler sinsampler_make(long start, long duration,
                        float freq, float amp, float phase) {
  SinSampler sampler = (SinSampler)fixed_allocator_alloc(sampler_allocator);
  sampler->sampler.function = (SamplerFunction)sin_sample;
  sampler->sampler.release = sampler_free;
  sampler->sampler.start_sample = start;
  sampler->sampler.duration_samples = duration;

  float cycles_per_sample = freq / SAMPLE_FREQ;
  sampler->radians_per_sample = cycles_per_sample * 2 * M_PI;
  sampler->amp = amp;
  sampler->phase = phase * 2 * M_PI;

  return (Sampler)sampler;
}

int16_t saw_sample(SawSampler sampler, long sample) {
  sample = sampler_offset(sampler, sample);
  long pos = (sampler->phase_samples + sample) % sampler->samples_per_period;
  return -sampler->amp + pos * sampler->slope;
}

Sampler sawsampler_make(long start, long duration,
                        float freq, float amp, float phase) {
  SawSampler sampler = (SawSampler)fixed_allocator_alloc(sampler_allocator);
  sampler->sampler.function = (SamplerFunction)saw_sample;
  sampler->sampler.release = sampler_free;
  sampler->sampler.start_sample = start;
  sampler->sampler.duration_samples = duration;

  long samples_per_period = SAMPLE_FREQ / freq;
  float slope = 2 * amp / samples_per_period;

  sampler->samples_per_period = samples_per_period;
  sampler->slope = slope;
  sampler->phase_samples = phase * samples_per_period;
  sampler->amp = amp;

  return (Sampler)sampler;
}

void oggsampler_release(void* _sampler) {
  OggSampler sampler = (OggSampler)_sampler;
  ov_clear(&sampler->vf);
  free(sampler);
}

void oggsampler_fillbuffer(OggSampler sampler) {
  int section;
  int read = 0;
  while(read < sizeof(sampler->buffer)) {
    int ret = ov_read(&sampler->vf, &sampler->buffer[read], sizeof(sampler->buffer) - read, &section);
    if(ret == 0) {
      // done
      memset(&sampler->buffer[read], 0, sizeof(sampler->buffer) - read);
      read = sizeof(sampler->buffer);
    } else if(ret < 0) {
      if(ret == OV_EBADLINK) {
        fprintf(stderr, "corrupt bitsream!\n");
        exit(1);
      }
    }
    read += ret;
  }
}

int16_t oggsampler_sample(void* _sampler, long sample) {
  OggSampler sampler = (OggSampler)_sampler;
  const int bufsz = sizeof(sampler->buffer) / (sizeof(int16_t) * sampler->channels);

  sample = sampler_offset(sampler, sample);
  while(sample - sampler->samples_past >= bufsz) {
    oggsampler_fillbuffer(sampler);
    sampler->samples_past += bufsz;
  }
  int16_t* buffer = (int16_t*)sampler->buffer;
  sample -= sampler->samples_past;

  int16_t value = buffer[sample * sampler->channels];
  return value * sampler->volume;
}

Sampler oggsampler_make(const char* filename, long start, float volume) {
  OggSampler sampler = (OggSampler)malloc(sizeof(struct OggSampler_));
  sampler->f = fopen(filename, "rb");
  if(ov_open(sampler->f, &sampler->vf, NULL, 0) < 0) {
    fprintf(stderr, "error opening vorbis file %s\n", filename);
    exit(1);
  }

  vorbis_info *vi = ov_info(&sampler->vf, -1);
  sampler->start_sample = start;
  sampler->duration_samples = ov_pcm_total(&sampler->vf, -1);
  sampler->function = oggsampler_sample;
  sampler->release = oggsampler_release;
  sampler->samples_past = 0;
  sampler->channels = vi->channels;
  sampler->sample_rate = vi->rate;
  sampler->volume = volume;
  oggsampler_fillbuffer(sampler);

  return sampler;
}

int16_t buffersampler_sample(void* _sampler, long sample) {
  BufferSampler sampler = (BufferSampler)_sampler;
  sample = sampler_offset(sampler, sample);
  if(sample < sampler->duration_samples) {
    return sampler->buffer[sample];
  } else {
    return 0;
  }
}

void buffersampler_release(void* sampler) {
  free(sampler);
}

Sampler buffersampler_make(int16_t* buffer, long start, long nsamples) {
  BufferSampler sampler = (BufferSampler)malloc(sizeof(struct BufferSampler_));
  sampler->start_sample = start;
  sampler->duration_samples = nsamples;
  sampler->function = buffersampler_sample;
  sampler->release = buffersampler_release;
  sampler->buffer = buffer;
  return sampler;
}

int16_t filter_value(Filter filter, int16_t value) {
  int ii;
  int xi = filter->xi;
  int yi = filter->yi;

  filter->xi = (xi + 1) % filter->na;
  filter->yi = (yi + 1) % filter->nb;

  filter->xs[xi] = value;

  int16_t result = 0;
  for(ii = 0; ii < filter->na; ++ii) {
    int idx = (xi + ii + 1) % filter->na;
    result += filter->xs[idx] * filter->as[filter->na - ii - 1];
  }

  for(ii = 0; ii < filter->nb; ++ii) {
    int idx = (yi + ii + 1) % filter->nb;
    result += filter->ys[idx] * filter->bs[filter->nb - ii - 1];
  }

  filter->ys[yi] = result;
  return result;
}

void filter_release(Filter filter) {
  free(filter->as);
  free(filter->bs);
  free(filter->xs);
  free(filter->ys);

  sampler_free(filter);
}

Filter filter_make(float* as, int na, float* bs, int nb) {
  Filter filter = (Filter)fixed_allocator_alloc(sampler_allocator);
  filter->na = na;
  filter->nb = nb;
  filter->xi = 0;
  filter->yi = 0;
  filter->as = (float*)malloc(na * sizeof(float));
  filter->xs = (int16_t*)malloc(na * sizeof(int16_t));
  filter->bs = (float*)malloc(nb * sizeof(float));
  filter->ys = (int16_t*)malloc(nb * sizeof(int16_t));

  memcpy(filter->as, as, na * sizeof(float));
  memcpy(filter->bs, bs, nb * sizeof(float));
  memset(filter->xs, 0, na * sizeof(int16_t));
  memset(filter->ys, 0, nb * sizeof(int16_t));
  return filter;
}

Filter lowpass_make(float cutoff, float sample_freq) {
  /*
  float omega_c = tan(cutoff / sample_freq * M_PI);
  float a = (1 - omega_c) / 2;
  float as[] = {a, a};
  float bs[] = {omega_c};
  */
  float as[] = {0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1};
  float bs[] = {0.0};
  return filter_make(as, array_size(as), bs, array_size(bs));
}
