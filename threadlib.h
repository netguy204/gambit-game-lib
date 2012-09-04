#ifndef THREADLIB_H
#define THREADLIB_H

#include <stdlib.h>
#include <pthread.h>
#include "listlib.h"

typedef struct Queue_ {
  DLLNode head;
  DLLNode tail;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
} *Queue;

Queue queue_make();
void queue_free(Queue queue);

void enqueue(Queue queue, DLLNode item);
DLLNode dequeue(Queue queue);

typedef struct ThreadBarrier_ {
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int nthreads;
  int threads_waiting;
} *ThreadBarrier;

ThreadBarrier threadbarrier_make();
void threadbarrier_free(ThreadBarrier barrier);

void threadbarrier_wait(ThreadBarrier barrier);

#endif
