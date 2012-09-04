#include "sampler.h"

#include <math.h>
#include <stdlib.h>
#include <memory.h>

int16_t sin_sample(SinSampler sampler, long sample) {
  return sampler->amp *
    sinf(sampler->phase + sampler->radians_per_sample * sample);
}

/** all phases are normalized phase values ranging from [0, 1)
 */

SinSampler sinsampler_make(float freq, float amp, float phase) {
  SinSampler sampler = malloc(sizeof(struct SinSampler_));
  sampler->sampler.function = (SamplerFunction)sin_sample;
  sampler->sampler.release = free;

  float cycles_per_sample = freq / SAMPLE_FREQ;
  sampler->radians_per_sample = cycles_per_sample * 2 * M_PI;
  sampler->amp = amp;
  sampler->phase = phase * 2 * M_PI;

  return sampler;
}

int16_t saw_sample(SawSampler sampler, long sample) {
  long pos = (sampler->phase_samples + sample) % sampler->samples_per_period;
  return -sampler->amp + pos * sampler->slope;
}

SawSampler sawsampler_make(float freq, float amp, float phase) {
  SawSampler sampler = malloc(sizeof(struct SawSampler_));
  sampler->sampler.function = (SamplerFunction)saw_sample;
  sampler->sampler.release = free;

  long samples_per_period = SAMPLE_FREQ / freq;
  float slope = 2 * amp / samples_per_period;
  
  sampler->samples_per_period = samples_per_period;
  sampler->slope = slope;
  sampler->phase_samples = phase * samples_per_period;
  sampler->amp = amp;

  return sampler;
}

void stepsampler_release(StepSampler sampler) {
  RELEASE_SAMPLER(sampler->nested_sampler);
  free(sampler);
}

int16_t stepsampler_sample(StepSampler sampler, long sample) {
  if(sample < sampler->start_sample) return 0;
  sample -= sampler->start_sample;

  if(sample >= sampler->duration_samples) return 0;

  return SAMPLE(sampler->nested_sampler, sample);
}

long stepsampler_start(StepSampler sampler) {
  return sampler->start_sample;
}

long stepsampler_duration(StepSampler sampler) {
  return sampler->duration_samples;
}

StepSampler stepsampler_make(Sampler nested_sampler,
                             long start_sample,
                             long duration_samples) {
  StepSampler sampler = malloc(sizeof(struct StepSampler_));

  sampler->sampler.sampler.function = (SamplerFunction)stepsampler_sample;
  sampler->sampler.sampler.release = (ReleaseSampler)stepsampler_release;
  sampler->sampler.duration = (SamplerDuration)stepsampler_duration;
  sampler->sampler.start = (SamplerStart)stepsampler_start;

  sampler->nested_sampler = nested_sampler;
  sampler->start_sample = start_sample;
  sampler->duration_samples = duration_samples;

  return sampler;
}

void finitesequence_release(FiniteSequence seq) {
  int ii;
  for(ii = 0; ii < seq->nsamplers; ++ii) {
    RELEASE_SAMPLER(seq->samplers[ii]);
  }

  free(seq->samplers);
  free(seq);
}

int16_t finitesequence_sample(FiniteSequence seq, long sample) {
  int ii = 0;
  int16_t result = 0;

  for(ii = 0; ii < seq->nsamplers; ++ii) {
    result += SAMPLE(seq->samplers[ii], sample);
  }

  return result;
}

long finitesequence_start(FiniteSequence seq) {
  if(seq->nsamplers == 0) return 0;
  return START(seq->samplers[0]);
}

long finitesequence_duration(FiniteSequence seq) {
  if(seq->nsamplers == 0) return 0;
  return END(seq->samplers[seq->nsamplers - 1]) - START(seq->samplers[0]);
}

FiniteSequence finitesequence_make(FiniteSampler* samplers, int nsamplers) {
  FiniteSequence seq = malloc(sizeof(struct FiniteSequence_));

  seq->sampler.sampler.function = (SamplerFunction)finitesequence_sample;
  seq->sampler.sampler.release = (ReleaseSampler)finitesequence_release;
  seq->sampler.start = (SamplerStart)finitesequence_start;
  seq->sampler.duration = (SamplerDuration)finitesequence_duration;

  seq->nsamplers = nsamplers;
  seq->samplers = samplers;
  return seq;
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

  if(filter->nested_sampler) RELEASE_SAMPLER(filter->nested_sampler);
}

int16_t filter_sample(Filter filter, long sample) {
  return filter_value(filter, SAMPLE(filter->nested_sampler, sample));
}


Filter filter_make(Sampler nested_sampler,
                   float* as, int na, float* bs, int nb) {
  Filter filter = malloc(sizeof(struct Filter_));
  filter->sampler.function = (SamplerFunction)filter_sample;
  filter->sampler.release = (ReleaseSampler)filter_release;

  filter->nested_sampler = nested_sampler;

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

Filter lowpass_make(Sampler nested_sampler, float cutoff, float sample_freq) {
  /*
  float omega_c = tan(cutoff / sample_freq * M_PI);
  float a = (1 - omega_c) / 2;
  float as[] = {a, a};
  float bs[] = {omega_c};
  */
  float as[] = {0.20};
  float bs[] = {0.20, 0.20, 0.20, 0.20};
  return filter_make(nested_sampler, as, array_size(as), bs, array_size(bs));
}

FiniteSequence make_sequence(float* freqs, int nfreqs, float amp,
                             float duration) {
  int ii = 0;
  FiniteSampler* samplers = malloc(nfreqs * sizeof(StepSampler));
  long next_start_sample = 0;
  long duration_samples = duration * SAMPLE_FREQ;
  float next_phase = 0;
  for(ii = 0; ii < nfreqs; ++ii) {
    SawSampler sin_sampler = sawsampler_make(freqs[ii], amp, next_phase);
    float added_phase = duration * freqs[ii];
    next_phase = fmodf(next_phase + added_phase, 1.0);

    samplers[ii] = (FiniteSampler)stepsampler_make((Sampler)sin_sampler,
                                                   next_start_sample,
                                                   duration_samples);
    next_start_sample = next_start_sample + duration_samples;
  }
  return finitesequence_make(samplers, nfreqs);
}
