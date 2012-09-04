#include "threadlib.h"

Queue queue_make() {
  Queue queue = (Queue)malloc(sizeof(struct Queue_));
  queue->head = NULL;
  queue->tail = NULL;
  pthread_mutex_init(&queue->mutex, NULL);
  pthread_cond_init(&queue->cond, NULL);
  return queue;
}

void queue_free(Queue queue) {
  free(queue);
}

void enqueue(Queue queue, DLLNode item) {
  pthread_mutex_lock(&queue->mutex);
  if(queue->head == NULL) {
    queue->head = item;
    queue->tail = item;
  } else {
    INSERT_BEFORE(queue->head, item);
    queue->head = item;
  }
  pthread_mutex_unlock(&queue->mutex);
  pthread_cond_signal(&queue->cond);
}

DLLNode dequeue(Queue queue) {
  pthread_mutex_lock(&queue->mutex);
  while(1) {
    if(queue->tail) {
      DLLNode result = queue->tail;
      DLLNode before = result->prev;
      if(before) {
        before->next = NULL;
        queue->tail = before;
      } else {
        queue->head = NULL;
        queue->tail = NULL;
      }
      pthread_mutex_unlock(&queue->mutex);
      return result;
    } else {
      /* need to wait for something to be put in the queue */
      pthread_cond_wait(&queue->cond, &queue->mutex);
    }
  }
}

ThreadBarrier threadbarrier_make(int nthreads) {
  ThreadBarrier barrier = (ThreadBarrier)malloc(sizeof(struct ThreadBarrier_));
  pthread_mutex_init(&barrier->mutex, NULL);
  pthread_cond_init(&barrier->cond, NULL);
  barrier->nthreads = nthreads;
  barrier->threads_waiting = 0;
  return barrier;
}

void threadbarrier_free(ThreadBarrier barrier) {
  free(barrier);
}

void threadbarrier_wait(ThreadBarrier barrier) {
  pthread_mutex_lock(&barrier->mutex);
  barrier->threads_waiting += 1;

  if(barrier->threads_waiting == barrier->nthreads) {
    barrier->threads_waiting = 0;
    pthread_cond_broadcast(&barrier->cond);
  } else {
    while(barrier->threads_waiting != 0) {
      pthread_cond_wait(&barrier->cond, &barrier->mutex);
    }
  }

  pthread_mutex_unlock(&barrier->mutex);
}
