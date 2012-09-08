#ifndef AUDIO_H
#define AUDIO_H

#include "sampler.h"
#include "listlib.h"

typedef struct PlayListSample_ {
  struct DLLNode_ node;
  Sampler sampler;
} *PlayListSample;

typedef struct PlayList_ {
  PlayListSample head;
  long next_sample;
} *PlayList;

PlayListSample playlistsample_make(Sampler sampler);
void playlistsample_free(PlayListSample pls);
void playlist_insert_sample(PlayList list, PlayListSample sample);
void playlist_fill_buffer(PlayList list, int16_t* buffer, int nsamples);

/* high level api */
void audio_init();
void audio_enqueue(Sampler sampler);
long audio_current_sample();

void audio_fill_buffer(int16_t* buffer, int nsamples);

/* provided by the system specific library */
void native_audio_init();

#endif
