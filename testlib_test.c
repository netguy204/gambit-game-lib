#include "memory.h"
#include "testcase.h"

int main(int argc, char ** argv) {
  int ii;

  FixedAllocator fa = fixed_allocator_make(sizeof(long), 100, "fa");
  void* last;
  for(ii=0; ii < 100; ++ii) {
    ASSERT((last = fixed_allocator_alloc(fa)) != NULL);
  }

  //ASSERT(fixed_allocator_alloc(fa) == NULL);
  fixed_allocator_free(fa, last);
  ASSERT(fixed_allocator_alloc(fa) != NULL);

  StackAllocator sa = stack_allocator_make(sizeof(long) * 100, "sa");
  for(ii=0; ii < 100; ++ii) {
    ASSERT((last = stack_allocator_alloc(sa, sizeof(long))) != NULL);
  }

  //ASSERT(stack_allocator_alloc(sa, sizeof(long)) == NULL);
  stack_allocator_freeall(sa);
  ASSERT(stack_allocator_alloc(sa, sizeof(long) * 5) != NULL);

  CircularBuffer buffer = circularbuffer_make(100);
  ASSERT(circularbuffer_bytes_writable(buffer) == 100);
  ASSERT(circularbuffer_bytes_readable(buffer) == 0);

  char bytes[100];
  for(ii = 0; ii < 100; ++ii) {
    bytes[ii] = ii;
  }

  circularbuffer_insert(buffer, bytes, 100);
  ASSERT(circularbuffer_bytes_writable(buffer) == 0);
  ASSERT(circularbuffer_bytes_readable(buffer) == 100);

  char morebytes[100];
  circularbuffer_read(buffer, morebytes, 50);
  ASSERT(circularbuffer_bytes_readable(buffer) == 50);
  ASSERT(circularbuffer_bytes_writable(buffer) == 50);
  for(int ii = 0; ii < 50; ++ii) {
    ASSERT(morebytes[ii] == ii);
  }

  circularbuffer_insert(buffer, bytes, 50);
  ASSERT(circularbuffer_bytes_writable(buffer) == 0);
  ASSERT(circularbuffer_bytes_readable(buffer) == 100);

  circularbuffer_read(buffer, morebytes, 100);
  ASSERT(circularbuffer_bytes_writable(buffer) == 100);
  ASSERT(circularbuffer_bytes_readable(buffer) == 0);
  for(int ii = 0; ii < 100; ++ii) {
    if(ii < 50) {
      ASSERT(morebytes[ii] == ii + 50);
    } else {
      ASSERT(morebytes[ii] == ii - 50);
    }
  }

  END_MAIN();
}
