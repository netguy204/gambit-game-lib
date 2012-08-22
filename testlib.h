#ifndef TEST_H
#define TEST_H

#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

/* exported by test.scm */
void set_screen(void*);
void step(int);
void terminate();

/* exported by testlib.c */
typedef struct FixedAllocator_ {
  size_t allocation_size;
  void* first_free;
} *FixedAllocator;

typedef struct StackAllocator_ {
  void* stack_top;
  void* stack_bottom;
  void* stack_max;
} *StackAllocator;

SDL_Surface* load_image(char * file);
void blit_image(SDL_Surface* target, SDL_Surface* src, int x, int y);

FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n);
void* fixed_allocator_alloc(FixedAllocator allocator);
void fixed_allocator_free(FixedAllocator allocator, void *obj);

StackAllocator stack_allocator_make(size_t stack_size);
void* stack_allocator_alloc(StackAllocator allocator, size_t size);
void stack_allocator_freeall(StackAllocator allocator);

#define MAX_NUM_CLOCKS 20

typedef struct Clock_ {
  long cycles; /* msecs */
  float time_scale;
  int paused;
} *Clock;

void clock_init();
Clock clock_make();
void clock_free(Clock clock);
void clock_update(Clock clock, float delta); /* time in seconds */
long clock_get_time(Clock clock); /* time in cycles */

float clock_cycles_to_seconds(long cycles);
long clock_seconds_to_cycles(float seconds);

#endif
