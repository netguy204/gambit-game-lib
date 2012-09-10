#include "sampler.h"
#include "memory.h"

#include <math.h>
#include <stdlib.h>
#include <memory.h>

#define MAX(x,y) ((x)>(y) ? (x) : (y))

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
  sin_table = malloc(sizeof(float) * TABLE_SIZE);

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
                                           NUM_SAMPLERS,
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
  filter->as = malloc(na * sizeof(float));
  filter->xs = malloc(na * sizeof(int16_t));
  filter->bs = malloc(nb * sizeof(float));
  filter->ys = malloc(nb * sizeof(int16_t));

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
