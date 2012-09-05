#include "threadlib.h"
#include "audio.h"

PlayList playlist;
Queue audio_queue;
Filter global_filter;

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
  if(list->head == NULL) {
    list->head = sample;
    sample->node.next = NULL;
    return;
  }

  if(START(sample->sampler) < START(list->head->sampler)) {
    sample->node.next = (DLLNode)list->head;
    list->head = sample;
    return;
  }

  PlayListSample last_node = list->head;
  PlayListSample current_node = (PlayListSample)list->head->node.next;

  while(current_node != NULL) {
    if(START(sample->sampler) < START(current_node->sampler)) {
      // insert before this node
      sample->node.next = (DLLNode)current_node;
      last_node->node.next = (DLLNode)sample;
      return; // done
    }

    last_node = current_node;
    current_node = (PlayListSample)current_node->node.next;
  }

  // must be after the end
  sample->node.next = NULL;
  last_node->node.next = (DLLNode)sample;
}

void playlist_fill_buffer(PlayList list, int16_t* buffer, int nsamples) {
  int ii;
  long next_sample = list->next_sample;
  list->next_sample += nsamples;

  for(ii = 0; ii < nsamples; ii+=2) {
    long sample = next_sample + ii;
    PlayListSample node;

    buffer[ii] = 0;
    for(node = list->head; node != NULL;
	node = (PlayListSample)node->node.next) {
      if(START(node->sampler) > sample) break;
      buffer[ii] += SAMPLE(node->sampler, sample);
    }

    buffer[ii] = filter_value(global_filter, buffer[ii]);
    buffer[ii+1] = buffer[ii];
  }

  /* remove any nodes that are no longer playable */
  while(list->head != NULL &&
        END(list->head->sampler) < list->next_sample) {
    PlayListSample node = list->head;
    list->head = (PlayListSample)node->node.next;
    playlistsample_free(node);
  }
}

void audio_init() {
  playlist = playlist_make();
  audio_queue = queue_make();
  global_filter = lowpass_make(NULL, 0, 0);

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
