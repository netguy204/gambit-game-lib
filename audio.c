#include "threadlib.h"
#include "audio.h"

PlayListSample playlistsample_make(FiniteSampler sampler) {
  PlayListSample pl = malloc(sizeof(struct PlayListSample_));
  pl->sampler = sampler;
  return pl;
}

void playlistsample_free(PlayListSample pls) {
  RELEASE_SAMPLER(pls->sampler);
  free(pls);
}

PlayList playlist_make() {
  PlayList pl = malloc(sizeof(struct PlayList_));
  pl->head = NULL;
  pl->next_sample = 0;
  return pl;
}

void playlist_insert_sampler(PlayList list, PlayListSample sample) {
  PlayListSample last_node = NULL;
  PlayListSample current_node = list->head;
  while(current_node != NULL) {
    if(START(sample->sampler) < START(current_node->sampler)) {
      // insert before this node
      if(last_node == NULL) {
        // new head
        sample->node.next = (DLLNode)current_node;
        list->head = sample;
      } else {
        sample->node.next = (DLLNode)current_node;
        last_node->node.next = (DLLNode)sample;
      }
      break; // done
    }

    last_node = current_node;
    current_node = (PlayListSample)current_node->node.next;
  }
}

void playlist_fill_buffer(PlayList list, int16_t* buffer, int nsamples) {
  int ii;

  for(ii = 0; ii < nsamples; ++ii) {
    long sample = list->next_sample + ii;

    buffer[ii] = 0;
    LL_FOREACH(PlayListSample, node, list->head) {
      if(END(node->sampler) < sample) break;
      buffer[ii] += SAMPLE(node->sampler, sample);
    }
  }

  list->next_sample += nsamples;

  /* remove any nodes that are no longer playable */
  while(list->head != NULL &&
        END(list->head->sampler) < list->next_sample) {
    PlayListSample node = list->head;
    list->head = (PlayListSample)node->node.next;
    playlistsample_free(node);
  }
}

PlayList playlist;
Queue audio_queue;

void audio_init() {
  playlist = playlist_make();
  audio_queue = queue_make();
  native_audio_init();
}

void audio_enqueue(FiniteSampler sampler) {
  enqueue(audio_queue, (DLLNode)playlistsample_make(sampler));
}

long audio_current_sample() {
  /* race condition but we don't care */
  return playlist->next_sample;
}

void audio_fill_buffer(int16_t* buffer, int nsamples) {
  PlayListSample sample;
  while((sample = (PlayListSample)dequeue_noblock(audio_queue)) != NULL) {
    playlist_insert_sampler(playlist, sample);
  }

  playlist_fill_buffer(playlist, buffer, nsamples);
}
