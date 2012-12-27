#ifndef MEMORY_H
#define MEMORY_H

#include "config.h"

#include <stdlib.h>
#include <pthread.h>

typedef struct FixedAllocator_ {
#ifdef DEBUG_MEMORY
  const char* name;
  long inflight;
  long max_inflight;
#endif
  pthread_mutex_t mutex;
  size_t allocation_size;
  void* first_free;
} *FixedAllocator;

typedef struct StackAllocator_ {
#ifdef DEBUG_MEMORY
  const char* name;
  long max_alloced;
#endif

  void* stack_top;
  void* stack_bottom;
  void* stack_max;
} *StackAllocator;

FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n,
                                    const char* name);
void* fixed_allocator_alloc(FixedAllocator allocator);
void fixed_allocator_free(FixedAllocator allocator, void *obj);

StackAllocator stack_allocator_make(size_t stack_size,
                                    const char* name);
void* stack_allocator_alloc(StackAllocator allocator, size_t size);
void stack_allocator_freeall(StackAllocator allocator);
void stack_allocator_release(StackAllocator allocator);

typedef struct CircularBuffer_ {
  int read_index;
  int write_index;
  int size;
  int filled;
  char* data;
} *CircularBuffer;

CircularBuffer circularbuffer_make(size_t bytes);
void circularbuffer_free(CircularBuffer buffer);
int circularbuffer_bytes_writable(CircularBuffer buffer);
int circularbuffer_bytes_readable(CircularBuffer buffer);

void circularbuffer_read_buffers(CircularBuffer buffer,
                                 char ** buffer1, int * size1,
                                 char ** buffer2, int * size2,
                                 int bytes_to_read);

void circularbuffer_write_buffers(CircularBuffer buffer,
                                  char ** buffer1, int * size1,
                                  char ** buffer2, int * size2,
                                  int bytes_to_write);

int circularbuffer_insert(CircularBuffer buffer, char * bytes, int length);

int circularbuffer_read(CircularBuffer buffer, char * target, int length);

#endif
