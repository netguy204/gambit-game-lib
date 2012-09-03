#include <SDL/SDL_audio.h>
#include <math.h>

#define N_(n,b) (n*b)

#define C_(n) N_(n, 261.6)
#define D_(n) N_(n, 293.7)
#define E_(n) N_(n, 329.6)
#define F_(n) N_(n, 349.2)
#define G_(n) N_(n, 392.0)
#define A_(n) N_(n, 440.0)
#define B_(n) N_(n, 493.9)

long last_count = 0;
float gfreq = 5000;
long gsample_freq = 44100 / 2; //22050;
long gamp = 32767;

typedef struct Filter_ {
  int na;
  int nb;
  int xi;
  float yi;

  float *as;
  float *bs;
  Sint16 *xs;
  Sint16 *ys;
} *Filter;

typedef Sint16 (*Sampler)(float, float, float, long);

typedef struct Sample_ {
  float freq;
  float duration;
  float start_in_sequence;
  float phase;
  Sampler sampler;
} *Sample;


typedef struct Sequence_ {
  int nsamples;
  float length;
  Sample samples;
} *Sequence;

Filter filter_make(float* as, int na, float* bs, int nb) {
  Filter filter = malloc(sizeof(struct Filter_));
  filter->na = na;
  filter->nb = nb;
  filter->xi = 0;
  filter->yi = 0;
  filter->as = malloc(na * sizeof(float));
  filter->xs = malloc(na * sizeof(Sint16));
  filter->bs = malloc(nb * sizeof(float));
  filter->ys = malloc(nb * sizeof(Sint16));

  memcpy(filter->as, as, na * sizeof(float));
  memcpy(filter->bs, bs, nb * sizeof(float));
  memset(filter->xs, 0, na * sizeof(Sint16));
  memset(filter->ys, 0, nb * sizeof(Sint16));
  return filter;
}

Sint16 filter_value(Filter filter, Sint16 value) {
  int ii;
  int xi = filter->xi;
  int yi = filter->yi;

  filter->xi = (xi + 1) % filter->na;
  filter->yi = (yi + 1) % filter->nb;

  filter->xs[xi] = value;

  Sint16 result = 0;
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

#define array_size(a) (sizeof(a)/sizeof(a[0]))

Filter lowpass_make(float cutoff, float sample_freq) {
  /*
  float omega_c = tan(cutoff / sample_freq * M_PI);
  float a = (1 - omega_c) / 2;
  float as[] = {a, a};
  float bs[] = {omega_c};
  */
  float as[] = {0.20};
  float bs[] = {0.20, 0.20, 0.20, 0.20};
  return filter_make(as, array_size(as), bs, array_size(bs));
}

Sint16 sin_sample(float freq, float sample_freq, float amp, long sample) {
  float cycles_per_sample = freq / sample_freq;
  return amp * sinf(2 * M_PI * sample * cycles_per_sample);
}

Sint16 saw_sample(float freq, float sample_freq, float amp, long sample) {
  long samples_per_period = sample_freq / freq;
  long pos = sample % samples_per_period;
  float slope = 2 * amp / samples_per_period;
  return -amp + pos * slope;
}

void sequence_update_times(Sequence sequence) {
  int ii = 0;
  float length = 0;
  Sample samples = sequence->samples;
  float last_phase_offset = 0;
  for(ii = 0; ii < sequence->nsamples; ++ii) {
    samples[ii].phase = last_phase_offset;
    samples[ii].start_in_sequence = length;
    length += samples[ii].duration;

    float total_phase = last_phase_offset +
      samples[ii].duration * samples[ii].freq * 2 * M_PI;
    last_phase_offset = fmodf(total_phase, 2*M_PI);
  }
  sequence->length = length;
}

// time is modded into the sequence
Sample sequence_find_sample(Sequence sequence, float time) {
  int ii = 0;
  Sample samples = sequence->samples;
  for(ii = 0; ii < sequence->nsamples; ++ii) {
    float end = samples[ii].start_in_sequence + samples[ii].duration;
    if(end > time) return &samples[ii];
  }
  return NULL;
}

Sint16 seq_sample(Sequence seq, float sample_freq, float amp, long sampleno) {
  float time = sampleno / sample_freq;
  Sample sample = sequence_find_sample(seq, time);
  if(sample == NULL) return 0;

  float phase_time = sample->phase / (sample->freq * 2 * M_PI);
  float sample_time = time - sample->start_in_sequence + phase_time;
  long sample_sample = sample_time * sample_freq;
  Sint16 value = sample->sampler(sample->freq, sample_freq, amp, sample_sample);
  //return filter_value(seq->filter, value);
  return value;
}

Sequence make_sequence(float* freqs, int nfreqs, float duration) {
  int ii = 0;
  Sequence seq = malloc(sizeof(struct Sequence_));
  seq->nsamples = nfreqs;
  seq->length = nfreqs * duration;
  Sample samples = malloc(nfreqs * sizeof(struct Sample_));
  seq->samples = samples;
  for(ii = 0; ii < nfreqs; ++ii) {
    samples[ii].freq = freqs[ii];
    samples[ii].duration = duration;
    samples[ii].sampler = sin_sample;
  }
  sequence_update_times(seq);
  return seq;
}

Sequence gsequence;
Filter gfilter;

/* The audio function callback takes the following parameters:
   stream:  A pointer to the audio buffer to be filled
   len:     The length (in bytes) of the audio buffer
*/
void fill_audio(void *udata, Uint8 *stream, int len)
{
  // Only play if we have data left 
  //if ( audio_len == 0 )
  //  return;
  
  // Mix as much data as possible 
  //len = ( len > audio_len ? audio_len : len );
  //SDL_MixAudio(stream, audio_pos, len, SDL_MIX_MAXVOLUME);
  //audio_pos += len;
  //audio_len -= len;

  int nwords = len / 2;
  int ii;
  Sint16 *words = (Sint16*)stream;
  //printf("asking for %d words\n", nwords);

  for(ii = 0; ii < nwords; ++ii) {
    //words[ii] = sin_sample(gfreq, gsample_freq, gamp, ii + last_count);
    //words[ii] += saw_sample(gfreq, gsample_freq, gamp, ii + last_count + 5);
    //words[ii] += saw_sample(gfreq, gsample_freq, gamp, ii + last_count);
    Sint16 value = seq_sample(gsequence, gsample_freq, gamp, ii + last_count);
    words[ii] = filter_value(gfilter, value);
  }

  last_count += nwords;
}

void sample_to_file(Sequence seq, float sample_freq, FILE* out) {
  long samples = seq->length * sample_freq;
  int ii = 0;
  Filter filter = lowpass_make(2000, sample_freq);
  for (ii = 0; ii < samples; ++ii) {
    float t = ii / sample_freq;
    Sint16 value = seq_sample(seq, sample_freq, gamp, ii);
    fprintf(out, "%f, %d, %d\n", t,
            value,
            filter_value(filter, value));
  }
}

int main(int argc, char ** argv) {
  float cutoff = 2000;

  if(argc > 1) {
    cutoff = atoi(argv[1]);
  }

  if(argc > 2) {
    gamp = atoi(argv[2]);
  }

  float freqs[] = {C_(1), D_(1), E_(1), E_(1),
                   C_(1), D_(1), E_(1),
                   C_(1), D_(1), E_(1), D_(1), C_(1), D_(1), C_(1)};

  gsequence = make_sequence(freqs, sizeof(freqs)/sizeof(float), 0.25);
  gfilter = lowpass_make(cutoff, gsample_freq);

  FILE * seqfile = fopen("seq.csv", "w");
  sample_to_file(gsequence, gsample_freq, seqfile);
  fclose(seqfile);

  SDL_AudioSpec wanted;
  
  // Set the audio format 
  wanted.freq = gsample_freq;
  wanted.format = AUDIO_S16SYS;
  wanted.channels = 1;    // 1 = mono, 2 = stereo 
  wanted.samples = 1024;  // Good low-latency value for callback 
  //wanted.size = 4096;
  wanted.padding = 0;
  wanted.callback = fill_audio;
  wanted.userdata = NULL;
  
  // Open the audio device, forcing the desired format 
  if ( SDL_OpenAudio(&wanted, NULL) < 0 ) {
    fprintf(stderr, "Couldn't open audio: %s\n", SDL_GetError());
    return(-1);
  }

  SDL_PauseAudio(0);
  SDL_Delay(4500);
  SDL_CloseAudio();
  return 0;
}
