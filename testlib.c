#include <math.h>

#include "testlib.h"

#define OFFSET(idx, obj_size, ptr) ((void*)(((char*)ptr) + (idx * obj_size)))
#define NEXT_ALIGNED_SIZE(x) ((x + 8 - 1) & ~(8 - 1))
#define SAFETY(x) x

/**
 * FixedAllocator's are used to quickly allocate and free objects of
 * fixed size. They opporate in constant time but cannot allocate more
 * objects than they were initially designed to hold. This makes them
 * appropriate for holding things like resource handles (since the
 * number of resources in the system is finite), timelines, and other
 * finite arity and long duration objects.
 */
FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n) {
  int ii;

  /* next 8 byte aligned size */
  obj_size = NEXT_ALIGNED_SIZE(obj_size);

  FixedAllocator allocator = (FixedAllocator)malloc(obj_size * n + 2*sizeof(struct FixedAllocator_));
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
  SAFETY(if(!allocator->first_free) return NULL);

  void * mem = allocator->first_free;
  allocator->first_free = *(void**)allocator->first_free;
  return mem;
}

void fixed_allocator_free(FixedAllocator allocator, void *obj) {
  *(void**)obj = allocator->first_free;
  allocator->first_free = obj;
}

StackAllocator stack_allocator_make(size_t stack_size) {
  size_t size = sizeof(struct StackAllocator_) * 2 + stack_size;
  size = NEXT_ALIGNED_SIZE(size);

  StackAllocator allocator = (StackAllocator)malloc(size);
  allocator->stack_bottom = &allocator[1];
  allocator->stack_top = allocator->stack_bottom;
  allocator->stack_max = (char*)allocator->stack_top + stack_size;
  return allocator;
}

void* stack_allocator_alloc(StackAllocator allocator, size_t size) {
  size = NEXT_ALIGNED_SIZE(size);
  SAFETY(if((char*)allocator->stack_top + size > (char*)allocator->stack_max) return NULL);
  void* mem = allocator->stack_top;
  allocator->stack_top = (char*)allocator->stack_top + size;
  return mem;
}

void stack_allocator_freeall(StackAllocator allocator) {
  allocator->stack_top = allocator->stack_bottom;
}

static FixedAllocator clock_allocator;

void clock_init() {
  clock_allocator = fixed_allocator_make(sizeof(struct Clock_), MAX_NUM_CLOCKS);
}

Clock clock_make() {
  Clock clock = (Clock)fixed_allocator_alloc(clock_allocator);
  clock->cycles = 0;
  clock->time_scale = 1.0f;
  clock->paused = 0;
  return clock;
}

void clock_free(Clock clock) {
  fixed_allocator_free(clock_allocator, clock);
}

void clock_update(Clock clock, float delta) {
  if(!clock->paused) {
    clock->cycles += clock_seconds_to_cycles(delta * clock->time_scale);
  }
}

long clock_get_time(Clock clock) {
  return clock->cycles;
}

float clock_cycles_to_seconds(long cycles) {
  return cycles / 1000.0f;
}

long clock_seconds_to_cycles(float seconds) {
  return roundf(seconds * 1000.0f);
}

SDL_Surface* load_image(char * file) {
  SDL_Surface *image;
  image = IMG_Load(file);
  if(image == NULL) {
    fprintf(stderr, "failed to load %s\n", file);
    return NULL;
  }

  return SDL_DisplayFormatAlpha(image);
}

void blit_image(SDL_Surface* target, SDL_Surface* src, int x, int y) {
  SDL_Rect dest;
  dest.x = x;
  dest.y = y;
  dest.w = src->w;
  dest.h = src->h;
  SDL_BlitSurface(src, NULL, target, &dest);
  SDL_UpdateRects(target, 1, &dest);
}
