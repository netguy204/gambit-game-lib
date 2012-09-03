#ifndef SAMPLER_H
#define SAMPLER

#include <stdint.h>

#define N_(n,b) (n*b)

#define C_(n) N_(n, 261.6)
#define D_(n) N_(n, 293.7)
#define E_(n) N_(n, 329.6)
#define F_(n) N_(n, 349.2)
#define G_(n) N_(n, 392.0)
#define A_(n) N_(n, 440.0)
#define B_(n) N_(n, 493.9)

#define SAMPLE_FREQ 22050

#define SAMPLE(f, x) (((Sampler)(f))->function(f, x))

#define array_size(a) (sizeof(a)/sizeof(a[0]))

typedef int16_t (*SamplerFunction)(void*, long);

typedef struct Sampler_ {
  SamplerFunction function;
} *Sampler;

typedef struct SinSampler_ {
  struct Sampler_ sampler;
  float phase; /* radians */
  float radians_per_sample;
  float amp;
} *SinSampler;

SinSampler sinsampler_make(float freq, float amp, float phase);

typedef struct SawSampler_ {
  struct Sampler_ sampler;
  long samples_per_period;
  long phase_samples;
  float slope;
  float amp;
} *SawSampler;

SawSampler sawsampler_make(float freq, float amp, float phase);

typedef struct FiniteSampler_ {
  struct Sampler_ sampler;
  Sampler nested_sampler;
  long start_sample;
  long duration_samples;
} *FiniteSampler;

FiniteSampler finitesampler_make(Sampler nested_sampler,
                                 long start_sample,
                                 long duration_samples);

typedef struct FiniteSequence_ {
  struct Sampler_ sampler;
  int nsamplers;
  FiniteSampler* samplers;
} *FiniteSequence;

FiniteSequence finitesequence_make(FiniteSampler* samplers, int nsamplers);

typedef struct Filter_ {
  struct Sampler_ sampler;
  Sampler nested_sampler;
  int na;
  int nb;
  int xi;
  float yi;

  float *as;
  float *bs;
  int16_t *xs;
  int16_t *ys;
} *Filter;

Filter filter_make(Sampler nested_sampler,
                   float* as, int na, float* bs, int nb);

Filter lowpass_make(Sampler nested_sampler, float cutoff, float sample_freq);

FiniteSequence make_sequence(float* freqs, int nfreqs, float amp,
                             float duration);

#endif
