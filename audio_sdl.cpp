#include <SDL/SDL_audio.h>
#include <unistd.h>
#include <assert.h>

#include "audio.h"
#include "memlib.h"

#define NUM_SAMPLES 2048

char * audio_pre_buffer;
CircularBuffer audio_buffer;
pthread_t audio_thread;
pthread_mutex_t audio_mutex;

int buffer_samples_can_write() {
  int result;
  pthread_mutex_lock(&audio_mutex);
  result = circularbuffer_bytes_writable(audio_buffer);
  pthread_mutex_unlock(&audio_mutex);
  return result / 2;
}

void* audio_exec(void* udata) {
  float buffer_time_us = (float)(1e6 * NUM_SAMPLES) / SAMPLE_FREQ;
  int sample_thresh = NUM_SAMPLES / 2;
  while(1) {
    // wait till we can write a good chunk
    int nsamples;
    while((nsamples = buffer_samples_can_write()) < sample_thresh) {
      usleep(buffer_time_us / 4);
    }

    // compute the audio that we know we need
    audio_fill_buffer((int16_t*)audio_pre_buffer, nsamples);

    // now lock the circular buffer and copy
    pthread_mutex_lock(&audio_mutex);

    int s1, s2;
    char *b1, *b2;
    int nbytes = nsamples * 2;
    circularbuffer_write_buffers(audio_buffer, &b1, &s1, &b2, &s2,
                                 nbytes);

    int to_write = MIN(nbytes, s1);
    memcpy(b1, audio_pre_buffer, to_write);
    nbytes -= to_write;

    if(nbytes > 0) {
      to_write = MIN(nbytes, s2);
      memcpy(b2, &audio_pre_buffer[s1], to_write);
    }

    pthread_mutex_unlock(&audio_mutex);

    // don't immediately bang on the lock
    usleep(buffer_time_us / 4);
  }
}

void fill_audio(void *udata, Uint8 *stream, int len) {
  int s1, s2;
  char *b1, *b2;

  pthread_mutex_lock(&audio_mutex);
  circularbuffer_read_buffers(audio_buffer, &b1, &s1, &b2, &s2,
                              len);

  int tocopy = MIN(len, s1);
  memcpy(stream, b1, tocopy);
  len -= tocopy;

  if(len > 0) {
    tocopy = MIN(len, s2);
    memcpy(stream, b2, tocopy);
  }

  pthread_mutex_unlock(&audio_mutex);
}

/*
void fill_audio(void *udata, Uint8 *stream, int len) {
  audio_fill_buffer((int16_t*)stream, len / 2);
}
*/

void native_audio_init() {
  // twice the number of samples in the sdl buffer (2 bytes per
  // channel per sample)
  int buffer_size = NUM_SAMPLES * 2 * 2 * 2;
  audio_buffer = circularbuffer_make(buffer_size);
  audio_pre_buffer = (char*)malloc(buffer_size);

  pthread_mutex_init(&audio_mutex, NULL);
  pthread_create(&audio_thread, NULL, audio_exec, NULL);

  SDL_AudioSpec wanted;

  // Set the audio format
  wanted.freq = SAMPLE_FREQ;
  wanted.format = AUDIO_S16SYS;
  wanted.channels = 2;    // 1 = mono, 2 = stereo
  wanted.samples = NUM_SAMPLES;  // Good low-latency value for callback
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
