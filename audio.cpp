#include "threadlib.h"
#include "audio.h"
#include "memlib.h"

#include <limits>

const int int16_min = std::numeric_limits<int16_t>::min();
const int int16_max = std::numeric_limits<int16_t>::max();

typedef Queue<PlayListSample_, offsetof(PlayListSample_, node)> PlayListSampleQueue;

PlayList playlist;
PlayListSampleQueue* audio_queue;
Filter global_filter;
FixedAllocator pls_allocator;

PlayListSample playlistsample_make(Sampler sampler) {
  PlayListSample pl = (PlayListSample)fixed_allocator_alloc(pls_allocator);
  pl->sampler = sampler;
  return pl;
}

void playlistsample_free(PlayListSample pls) {
  RELEASE_SAMPLER(pls->sampler);
  fixed_allocator_free(pls_allocator, pls);
}

PlayList playlist_make() {
  PlayList pl = new PlayList_();
  pl->next_sample = 0;
  return pl;
}

void playlist_insert_sampler(PlayList list, PlayListSample sample) {
  list->samples.insert_before_when(sample, [=](PlayListSample other) {
      return START(sample->sampler) < START(other->sampler);
    });
}

void playlist_fill_buffer(PlayList list, int16_t* buffer, int nsamples) {
  int ii;
  long next_sample = list->next_sample;
  list->next_sample += nsamples / 2;

  for(ii = 0; ii < nsamples; ii+=2) {
    long sample = next_sample + ii / 2;
    PlayListSample node;

    int32_t value = 0;
    for(node = (PlayListSample)list->samples.head; node != NULL;
        node = (PlayListSample)node->node.next) {
      if(START(node->sampler) > sample) break;
      value +=  SAMPLE(node->sampler, sample);
    }

    if(value > int16_max) {
      buffer[ii] = int16_max;
    } else if(value < int16_min) {
      buffer[ii] = int16_min;
    } else {
      buffer[ii] = value;
    }

    buffer[ii+1] = buffer[ii];
  }

  /* remove any nodes that are no longer playable */
  PlayListSample node = (PlayListSample)list->samples.head;
  while(node) {
    PlayListSample next = (PlayListSample)node->node.next;
    if(END(((PlayListSample)node)->sampler) < list->next_sample) {
      list->samples.remove(node);
      playlistsample_free(node);
    }
    node = next;
  }
}

void audio_init() {
  sampler_init();
  pls_allocator = fixed_allocator_make(sizeof(struct PlayListSample_),
                                       MAX_NUM_SAMPLERS,
                                       "pls_allocator");

  playlist = playlist_make();
  audio_queue = new PlayListSampleQueue();
  global_filter = lowpass_make(0, 0);

  native_audio_init();
}

void audio_enqueue(Sampler sampler) {
  audio_queue->enqueue(playlistsample_make(sampler));
}

long audio_current_sample() {
  /* race condition but we don't care */
  return playlist->next_sample;
}

void audio_fill_buffer(int16_t* buffer, int nsamples) {
  PlayListSample sample;
  while((sample = audio_queue->dequeue_noblock()) != NULL) {
    playlist_insert_sampler(playlist, sample);
  }

  playlist_fill_buffer(playlist, buffer, nsamples);
}
