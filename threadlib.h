#ifndef THREADLIB_H
#define THREADLIB_H

#include <stdlib.h>
#include <pthread.h>
#include "listlib.h"

template<typename E, int OFFSET>
class Queue {
 public:
  DLL<E,OFFSET> list;
  pthread_mutex_t mutex;
  pthread_cond_t cond;

  Queue() {
    pthread_mutex_init(&this->mutex, NULL);
    pthread_cond_init(&this->cond, NULL);
  }

  ~Queue() {
    pthread_cond_destroy(&this->cond);
    pthread_mutex_destroy(&this->mutex);
  }

  void enqueue(E* element) {
    pthread_mutex_lock(&this->mutex);
    this->list.add_head(element);
    pthread_mutex_unlock(&this->mutex);

    pthread_cond_signal(&this->cond);
  }

  E* dequeue() {
    pthread_mutex_lock(&this->mutex);
    while(1) {
      if(this->list.tail) {
        E* result = this->list.remove_tail();
        pthread_mutex_unlock(&this->mutex);
        return result;
      } else {
        /* need to wait for something to be put in the queue */
        pthread_cond_wait(&this->cond, &this->mutex);
      }
    }
  }

  E* dequeue_noblock() {
    pthread_mutex_lock(&this->mutex);
    E* result = NULL;
    if(this->list.tail) {
      result = this->list.remove_tail();
    }
    pthread_mutex_unlock(&this->mutex);
    return result;
  }
};

typedef struct ThreadBarrier_ {
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int nthreads;
  int threads_waiting;
  int seq_no; // number of times we've waited at this barrier, overflow ok
} *ThreadBarrier;

ThreadBarrier threadbarrier_make(int nthreads);
void threadbarrier_free(ThreadBarrier barrier);

void threadbarrier_wait(ThreadBarrier barrier);

#endif
