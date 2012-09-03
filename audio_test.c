#include <SDL/SDL_audio.h>
#include "sampler.h"

long last_count = 0;
float gfreq = 5000;
//long gsample_freq = 44100 / 2; //22050;
long gamp = 32767;


FiniteSequence gsequence;
Filter gfilter;

/* The audio function callback takes the following parameters:
   stream:  A pointer to the audio buffer to be filled
   len:     The length (in bytes) of the audio buffer
*/
void fill_audio(void *udata, Uint8 *stream, int len)
{
  int nwords = len / 2;
  int ii;
  Sint16 *words = (Sint16*)stream;

  for(ii = 0; ii < nwords; ++ii) {
    words[ii] = SAMPLE(gfilter, ii + last_count);
  }

  last_count += nwords;
}

void sample_to_file(Sampler sampler, float sample_freq, float max, FILE* out) {
  int ii = 0;
  long samples = max * SAMPLE_FREQ;
  Filter filter = lowpass_make(sampler, 2000, sample_freq);
  for (ii = 0; ii < samples; ++ii) {
    float t = (float)ii / SAMPLE_FREQ;
    fprintf(out, "%f, %d, %d\n", t,
            SAMPLE(sampler, ii),
            SAMPLE(filter, ii));
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

  gsequence = make_sequence(freqs, array_size(freqs), gamp, 0.13);
  gfilter = lowpass_make((Sampler)gsequence, cutoff, SAMPLE_FREQ);

  FILE * seqfile = fopen("seq.csv", "w");
  sample_to_file((Sampler)gsequence, SAMPLE_FREQ, 1.0, seqfile);
  fclose(seqfile);

  SDL_AudioSpec wanted;
  
  // Set the audio format 
  wanted.freq = SAMPLE_FREQ;
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
