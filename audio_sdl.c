#include <SDL/SDL_audio.h>
#include "audio.h"

void fill_audio(void *udata, Uint8 *stream, int len) {
  int nwords = len / 2;
  audio_fill_buffer((int16_t*)stream, nwords);
}

void native_audio_init() {
  SDL_AudioSpec wanted;
  
  // Set the audio format 
  wanted.freq = SAMPLE_FREQ;
  wanted.format = AUDIO_S16SYS;
  wanted.channels = 2;    // 1 = mono, 2 = stereo 
  wanted.samples = 1024;  // Good low-latency value for callback 
  //wanted.size = 4096;
  wanted.padding = 0;
  wanted.callback = fill_audio;
  wanted.userdata = NULL;

  // Open the audio device, forcing the desired format 
  if ( SDL_OpenAudio(&wanted, NULL) < 0 ) {
    fprintf(stderr, "Couldn't open audio: %s\n", SDL_GetError());
    exit(-1);
  }

  SDL_PauseAudio(0);
}
