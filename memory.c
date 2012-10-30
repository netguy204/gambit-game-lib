#include "memory.h"
#include "config.h"

#include <stdarg.h>
#include <stdio.h>
#include <memory.h>
#include <assert.h>

void* fail_exit(const char * message, ...) {
  fprintf(stderr, "FAIL_EXIT: ");

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  fflush(stderr);
  exit(1);
  return NULL;
}

/**
 * FixedAllocator's are used to quickly allocate and free objects of
 * fixed size. They operate in constant time but cannot allocate more
 * objects than they were initially designed to hold. This makes them
 * appropriate for holding things like resource handles (since the
 * number of resources in the system is finite), timelines, and other
 * finite arity and long duration objects.
 */
FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n,
                                    const char* name) {
  int ii;

  /* next 8 byte aligned size */
  obj_size = NEXT_ALIGNED_SIZE(obj_size);

  FixedAllocator allocator = (FixedAllocator)malloc(obj_size * n + 2*sizeof(struct FixedAllocator_));

#ifdef DEBUG_MEMORY
  allocator->name = name;
  allocator->inflight = 0;
  allocator->max_inflight = 0;
#endif

  pthread_mutex_init(&allocator->mutex, NULL);
  allocator->allocation_size = obj_size;

  void* mem = &allocator[1];
  allocator->first_free = NULL;
  for(ii = 0; ii < n; ++ii) {
    *(void**)mem = allocator->first_free;
    allocator->first_free = mem;
    mem += obj_size;
  }

  return allocator;
}

void* fixed_allocator_alloc(FixedAllocator allocator) {
  SAFETY(if(!allocator->first_free) return fail_exit("fixed_allocator %s failed", allocator->name));

  pthread_mutex_lock(&allocator->mutex);
  void * mem = allocator->first_free;
  allocator->first_free = *(void**)allocator->first_free;

#ifdef DEBUG_MEMORY
  allocator->inflight += 1;
  if(allocator->inflight > allocator->max_inflight) {
    allocator->max_inflight = allocator->inflight;
  }
#endif
  pthread_mutex_unlock(&allocator->mutex);

  return mem;
}

void fixed_allocator_free(FixedAllocator allocator, void *obj) {
  pthread_mutex_lock(&allocator->mutex);
  *(void**)obj = allocator->first_free;
  allocator->first_free = obj;

#ifdef DEBUG_MEMORY
  allocator->inflight -= 1;
#endif
  pthread_mutex_unlock(&allocator->mutex);
}

StackAllocator stack_allocator_make(size_t stack_size, const char* name) {
  size_t size = sizeof(struct StackAllocator_) * 2 + stack_size;
  size = NEXT_ALIGNED_SIZE(size);

  StackAllocator allocator = (StackAllocator)malloc(size);
#ifdef DEBUG_MEMORY
  allocator->name = name;
#endif
  pthread_mutex_init(&allocator->mutex, NULL);
  allocator->stack_bottom = &allocator[1];
  allocator->stack_top = allocator->stack_bottom;
  allocator->stack_max = (char*)allocator->stack_top + stack_size;
  return allocator;
}

void* stack_allocator_alloc(StackAllocator allocator, size_t size) {
  pthread_mutex_lock(&allocator->mutex);
  size = NEXT_ALIGNED_SIZE(size);
  SAFETY(if((char*)allocator->stack_top + size > (char*)allocator->stack_max) return fail_exit("stack_allocator %s failed", allocator->name));
  void* mem = allocator->stack_top;
  allocator->stack_top = (char*)allocator->stack_top + size;
  pthread_mutex_unlock(&allocator->mutex);

  return mem;
}

void stack_allocator_freeall(StackAllocator allocator) {
#ifdef DEBUG_MEMORY
  allocator->max_alloced = (char*)allocator->stack_top - (char*)allocator->stack_bottom;
#endif
  allocator->stack_top = allocator->stack_bottom;
}

void stack_allocator_release(StackAllocator allocator) {
  free(allocator);
}

CircularBuffer circularbuffer_make(size_t bytes) {
  CircularBuffer buffer = malloc(sizeof(struct CircularBuffer_));
  buffer->read_index = 0;
  buffer->write_index = 0;
  buffer->size = bytes;
  buffer->filled = 0;
  buffer->data = malloc(bytes);
  return buffer;
}

void circularbuffer_free(CircularBuffer buffer) {
  free(buffer->data);
  free(buffer);
}

int circularbuffer_distance(CircularBuffer buffer, int i1, int i2) {
  if(i2 > i1) {
    return i2 - i1;
  } else {
    return i2 + buffer->size - i1;
  }
}

int circularbuffer_bytes_writable(CircularBuffer buffer) {
  if(!buffer->filled && (buffer->write_index == buffer->read_index)) {
    return buffer->size;
  } else if(buffer->filled) {
    return 0;
  } else {
    return circularbuffer_distance(buffer, buffer->write_index,
                                   buffer->read_index);
  }
}

int circularbuffer_bytes_readable(CircularBuffer buffer) {
  if(buffer->filled) {
    return buffer->size;
  } else if(buffer->read_index == buffer->write_index) {
    return 0;
  } else {
    return circularbuffer_distance(buffer, buffer->read_index,
                                   buffer->write_index);
  }
}

int circularbuffer_add(CircularBuffer buffer, int a, int b) {
  return (a + b) % buffer->size;
}

void circularbuffer_read_buffers(CircularBuffer buffer,
                                 char ** buffer1, int * size1,
                                 char ** buffer2, int * size2,
                                 int bytes_to_read) {
  // easy case, 1 buffer
  if(buffer->read_index < buffer->write_index) {
    *buffer1 = &buffer->data[buffer->read_index];
    *size1 = buffer->write_index - buffer->read_index;
    *buffer2 = NULL;
    *size2 = 0;
  } else if(!buffer->filled && buffer->read_index == buffer->write_index) {
    *buffer1 = NULL;
    *size1 = 0;
    *buffer2 = NULL;
    *size2 = 0;
  } else {
    // 2 buffers
    *buffer1 = &buffer->data[buffer->read_index];
    *size1 = buffer->size - buffer->read_index;
    *buffer2 = buffer->data;
    *size2 = buffer->write_index;
  }

  bytes_to_read = MIN(bytes_to_read, *size1 + *size2);
  buffer->read_index = circularbuffer_add(buffer, buffer->read_index,
                                          bytes_to_read);

  if(bytes_to_read > 0) buffer->filled = 0;
}

void circularbuffer_write_buffers(CircularBuffer buffer,
                                  char ** buffer1, int * size1,
                                  char ** buffer2, int * size2,
                                  int bytes_to_write) {
  // 1 buffer
  if(buffer->write_index < buffer->read_index) {
    *buffer1 = &buffer->data[buffer->write_index];
    *size1 = buffer->read_index - buffer->write_index;
    *buffer2 = NULL;
    *size2 = 0;
  } else if (buffer->filled) {
    *buffer1 = NULL;
    *size1 = 0;
    *buffer2 = NULL;
    *size2 = 0;
  } else {
    // the corner case and the 2 buffer case are the same for writing
    *buffer1 = &buffer->data[buffer->write_index];
    *size1 = buffer->size - buffer->write_index;
    *buffer2 = buffer->data;
    *size2 = buffer->read_index;
  }

  bytes_to_write = MIN(bytes_to_write, *size1 + *size2);
  buffer->write_index = circularbuffer_add(buffer, buffer->write_index,
                                           bytes_to_write);

  if(bytes_to_write > 0 && buffer->read_index == buffer->write_index) {
    buffer->filled = 1;
  }
}

int circularbuffer_insert(CircularBuffer buffer, char * bytes, int length) {
  assert(bytes);

  int s1, s2;
  char *b1, *b2;
  circularbuffer_write_buffers(buffer, &b1, &s1, &b2, &s2, length);

  int to_write = MIN(length, s1);

  assert(b1);
  memcpy(b1, bytes, to_write);
  length -= to_write;

  if(length > 0) {
    to_write = MIN(length, s2);
    memcpy(b2, &bytes[s1], to_write);
    length -= to_write;
  }

  return length == 0;
}

int circularbuffer_read(CircularBuffer buffer, char * target, int length) {
  assert(target);

  int s1, s2;
  char *b1, *b2;
  circularbuffer_read_buffers(buffer, &b1, &s1, &b2, &s2, length);

  int to_read = MIN(length, s1);
  int read = 0;

  assert(b1);
  memcpy(target, b1, to_read);
  length -= to_read;

  if(length > 0) {
    to_read = MIN(length, s2);
    memcpy(&target[s1], b2, to_read);
    length -= to_read;
  }

  return length == 0;
}
