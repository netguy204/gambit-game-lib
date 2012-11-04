#ifndef THREADLIB_H
#define THREADLIB_H

#include <stdlib.h>
#include <pthread.h>
#include "listlib.h"

typedef struct Queue_ {
  struct DLL_ list;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
} *Queue;

Queue queue_make();
void queue_free(Queue queue);

void enqueue(Queue queue, DLLNode item);
DLLNode dequeue(Queue queue);
DLLNode dequeue_noblock(Queue queue);

typedef struct ThreadBarrier_ {
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int nthreads;
  int threads_waiting;
  int seq_no; // number of times we've waited at this barrier, overflow ok
} *ThreadBarrier;

ThreadBarrier threadbarrier_make();
void threadbarrier_free(ThreadBarrier barrier);

void threadbarrier_wait(ThreadBarrier barrier);

#endif
