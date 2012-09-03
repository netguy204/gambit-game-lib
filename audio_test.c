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

typedef struct Sample_ {
  float freq;
  float amp;
  float duration;
  float start_in_sequence;
  float phase;
} *Sample;


typedef struct Sequence_ {
  int nsamples;
  float length;
  Sample samples;
} *Sequence;

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
  float seq_time = fmodf(time, sequence->length);
  Sample samples = sequence->samples;
  for(ii = 0; ii < sequence->nsamples; ++ii) {
    float end = samples[ii].start_in_sequence + samples[ii].duration;
    if(end > seq_time) return &samples[ii];
  }
  return NULL;
}

Sint16 seq_sample(Sequence seq, float sample_freq, float amp_scale, long sampleno) {
  float time = sampleno / sample_freq;
  Sample sample = sequence_find_sample(seq, time);
  float phase_time = sample->phase / (sample->freq * 2 * M_PI);
  float sample_time = time - sample->start_in_sequence + phase_time;
  long sample_sample = sample_time * sample_freq;
  return sin_sample(sample->freq, sample_freq, sample->amp * amp_scale,
                    sample_sample);
}

Sequence make_sequence(float* freqs, float amp, int nfreqs, float duration) {
  int ii = 0;
  Sequence seq = malloc(sizeof(struct Sequence_));
  seq->nsamples = nfreqs;
  seq->length = nfreqs * duration;
  Sample samples = malloc(nfreqs * sizeof(struct Sample_));
  seq->samples = samples;
  for(ii = 0; ii < nfreqs; ++ii) {
    samples[ii].freq = freqs[ii];
    samples[ii].amp = amp;
    samples[ii].duration = duration;
  }
  sequence_update_times(seq);
  return seq;
}

Sequence gsequence;

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
    words[ii] = seq_sample(gsequence, gsample_freq, 1.0, ii + last_count);
  }

  last_count += nwords;
}

void sample_to_file(Sequence seq, float sample_freq, FILE* out) {
  long samples = seq->length * sample_freq;
  int ii = 0;
  for (ii = 0; ii < samples; ++ii) {
    float t = ii / sample_freq;
    fprintf(out, "%f, %d\n", t,
            seq_sample(seq, sample_freq, 1.0, ii));
  }
}

int main(int argc, char ** argv) {
  if(argc > 1) {
    gamp = atoi(argv[1]);
  }

  float freqs[] = {C_(1), D_(1), E_(1), E_(1),
                   C_(1), D_(1), E_(1),
                   C_(1), D_(1), E_(1), D_(1), C_(1), D_(1), C_(1), C_(1)};

  gsequence = make_sequence(freqs, gamp, sizeof(freqs)/sizeof(float), 0.25);

  /*
  FILE * seqfile = fopen("seq.csv", "w");
  sample_to_file(gsequence, gsample_freq, seqfile);
  fclose(seqfile);
  */

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
  SDL_Delay(4000);
  SDL_CloseAudio();
  return 0;
}
