#include "threadlib.h"

Queue queue_make() {
  Queue queue = (Queue)malloc(sizeof(struct Queue_));
  queue->list.head = NULL;
  queue->list.tail = NULL;
  pthread_mutex_init(&queue->mutex, NULL);
  pthread_cond_init(&queue->cond, NULL);
  return queue;
}

void queue_free(Queue queue) {
  free(queue);
}

void enqueue(Queue queue, DLLNode item) {
  pthread_mutex_lock(&queue->mutex);
  dll_add_head((DLL)queue, item);
  pthread_mutex_unlock(&queue->mutex);
  pthread_cond_signal(&queue->cond);
}

DLLNode dequeue(Queue queue) {
  pthread_mutex_lock(&queue->mutex);
  while(1) {
    if(queue->list.tail) {
      DLLNode result = dll_remove_tail((DLL)queue);
      pthread_mutex_unlock(&queue->mutex);
      return result;
    } else {
      /* need to wait for something to be put in the queue */
      pthread_cond_wait(&queue->cond, &queue->mutex);
    }
  }
}

DLLNode dequeue_noblock(Queue queue) {
  pthread_mutex_lock(&queue->mutex);
  DLLNode result = NULL;
  if(queue->list.tail) {
    result = dll_remove_tail((DLL)queue);
  }
  pthread_mutex_unlock(&queue->mutex);
 return result;
}

ThreadBarrier threadbarrier_make(int nthreads) {
  ThreadBarrier barrier = (ThreadBarrier)malloc(sizeof(struct ThreadBarrier_));
  pthread_mutex_init(&barrier->mutex, NULL);
  pthread_cond_init(&barrier->cond, NULL);
  barrier->nthreads = nthreads;
  barrier->threads_waiting = 0;
  barrier->seq_no = 0;
  return barrier;
}

void threadbarrier_free(ThreadBarrier barrier) {
  pthread_cond_destroy(&barrier->cond);
  pthread_mutex_destroy(&barrier->mutex);
  free(barrier);
}

void threadbarrier_wait(ThreadBarrier barrier) {
  pthread_mutex_lock(&barrier->mutex);
  barrier->threads_waiting += 1;

  if(barrier->threads_waiting == barrier->nthreads) {
    barrier->threads_waiting = 0;
    barrier->seq_no++;
    pthread_cond_broadcast(&barrier->cond);
  } else {
    int last_seq_no = barrier->seq_no;
    while(barrier->seq_no == last_seq_no) {
      pthread_cond_wait(&barrier->cond, &barrier->mutex);
    }
  }

  pthread_mutex_unlock(&barrier->mutex);
}
