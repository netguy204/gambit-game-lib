#ifndef MEMORY_H
#define MEMORY_H

#include <stdlib.h>

#define DEBUG_MEMORY

typedef struct FixedAllocator_ {
#ifdef DEBUG_MEMORY
  const char* name;
  long inflight;
  long max_inflight;
#endif
  size_t allocation_size;
  void* first_free;
} *FixedAllocator;

typedef struct StackAllocator_ {
#ifdef DEBUG_MEMORY
  const char* name;
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


#endif
