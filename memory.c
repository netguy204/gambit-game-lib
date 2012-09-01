#include "memory.h"

#include <stdarg.h>
#include <stdio.h>

#define SAFETY(x) x
#define OFFSET(idx, obj_size, ptr) ((void*)(((char*)ptr) + (idx * obj_size)))
#define NEXT_ALIGNED_SIZE(x) ((x + 8 - 1) & ~(8 - 1))

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
 * fixed size. They operrate in constant time but cannot allocate more
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

  void * mem = allocator->first_free;
  allocator->first_free = *(void**)allocator->first_free;

#ifdef DEBUG_MEMORY
  allocator->inflight += 1;
  if(allocator->inflight > allocator->max_inflight) {
    allocator->max_inflight = allocator->inflight;
  }
#endif

  return mem;
}

void fixed_allocator_free(FixedAllocator allocator, void *obj) {
  *(void**)obj = allocator->first_free;
  allocator->first_free = obj;

#ifdef DEBUG_MEMORY
  allocator->inflight -= 1;
#endif
}

StackAllocator stack_allocator_make(size_t stack_size, const char* name) {
  size_t size = sizeof(struct StackAllocator_) * 2 + stack_size;
  size = NEXT_ALIGNED_SIZE(size);

  StackAllocator allocator = (StackAllocator)malloc(size);
#ifdef DEBUG_MEMORY
  allocator->name = name;
#endif
  allocator->stack_bottom = &allocator[1];
  allocator->stack_top = allocator->stack_bottom;
  allocator->stack_max = (char*)allocator->stack_top + stack_size;
  return allocator;
}

void* stack_allocator_alloc(StackAllocator allocator, size_t size) {
  size = NEXT_ALIGNED_SIZE(size);
  SAFETY(if((char*)allocator->stack_top + size > (char*)allocator->stack_max) return fail_exit("stack_allocator %s failed", allocator->name));
  void* mem = allocator->stack_top;
  allocator->stack_top = (char*)allocator->stack_top + size;
  return mem;
}

void stack_allocator_freeall(StackAllocator allocator) {
  allocator->stack_top = allocator->stack_bottom;
}

