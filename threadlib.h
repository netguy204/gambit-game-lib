#ifndef THREADLIB_H
#define THREADLIB_H

#include <stdlib.h>
#include <pthread.h>
typedef struct DLLNode_ *DLLNode;

struct DLLNode_ {
  DLLNode next;
  DLLNode prev;
};

void llnode_insert_after(DLLNode target, DLLNode addition);
void llnode_insert_before(DLLNode target, DLLNode addition);
void llnode_remove(DLLNode node);

#define INSERT_AFTER(target, addition) \
  llnode_insert_after((DLLNode)target, (DLLNode)addition)

#define INSERT_BEFORE(target, addition) \
  llnode_insert_before((DLLNode)target, (DLLNode)addition)

#define REMOVE(node) \
  llnode_remove((DLLNode)node)

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
