#ifndef SAMPLER_H
#define SAMPLER

#include <stdint.h>
#include "config.h"

#define N_(n,b) (n*b)

#define C_(n) N_(n, 261.6)
#define D_(n) N_(n, 293.7)
#define E_(n) N_(n, 329.6)
#define F_(n) N_(n, 349.2)
#define G_(n) N_(n, 392.0)
#define A_(n) N_(n, 440.0)
#define B_(n) N_(n, 493.9)

#define SAMPLE(f, x) (((Sampler)(f))->function(f, x))
#define RELEASE_SAMPLER(f) (((Sampler)(f))->release(f))

typedef int16_t (*SamplerFunction)(void*, long);
typedef void (*ReleaseSampler)(void*);

void sampler_init();

typedef struct Sampler_ {
  SamplerFunction function;
  ReleaseSampler release;
  long start_sample;
  long duration_samples;
} *Sampler;

typedef struct SinSampler_ {
  struct Sampler_ sampler;
  float phase; /* radians */
  float radians_per_sample;
  float amp;
} *SinSampler;

Sampler sinsampler_make(long start, long duration,
                        float freq, float amp, float phase);

typedef struct SawSampler_ {
  struct Sampler_ sampler;
  long samples_per_period;
  long phase_samples;
  float slope;
  float amp;
} *SawSampler;

Sampler sawsampler_make(long start, long duration,
                        float freq, float amp, float phase);

#define DURATION(f) (((Sampler)f)->duration_samples)
#define START(f) (((Sampler)f)->start_sample)
#define END(f) (START(f) + DURATION(f))

typedef struct Filter_ {
  int na;
  int nb;
  int xi;
  float yi;

  float *as;
  float *bs;
  int16_t *xs;
  int16_t *ys;
} *Filter;

Filter filter_make(float* as, int na, float* bs, int nb);

int16_t filter_value(Filter filter, int16_t value);

Filter lowpass_make(float cutoff, float sample_freq);

#endif
