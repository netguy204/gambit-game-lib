#include "testlib.h"
#include "testcase.h"

int main(int argc, char ** argv) {
  int ii;

  FixedAllocator fa = fixed_allocator_make(sizeof(long), 100);
  void* last;
  for(ii=0; ii < 100; ++ii) {
    ASSERT((last = fixed_allocator_alloc(fa)) != NULL);
  }

  ASSERT(fixed_allocator_alloc(fa) == NULL);
  fixed_allocator_free(fa, last);
  ASSERT(fixed_allocator_alloc(fa) != NULL);

  StackAllocator sa = stack_allocator_make(sizeof(long) * 100);
  for(ii=0; ii < 100; ++ii) {
    ASSERT((last = stack_allocator_alloc(sa, sizeof(long))) != NULL);
  }

  ASSERT(stack_allocator_alloc(sa, sizeof(long)) == NULL);
  stack_allocator_freeall(sa);
  ASSERT(stack_allocator_alloc(sa, sizeof(long) * 5) != NULL);

  clock_init();

  Clock clock = clock_make();
  ASSERT(clock_get_time(clock) == 0);
  clock_update(clock, 1.0);
  ASSERT(clock_get_time(clock) == 1000);
  clock->paused = 1;
  clock_update(clock, 1.0);
  ASSERT(clock_get_time(clock) == 1000);
  clock->paused = 0;
  clock->time_scale = -1.0f;
  clock_update(clock, 0.5);
  ASSERT(clock_get_time(clock) == 500);
  clock_free(clock);

  END_MAIN();
}
