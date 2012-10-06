#include "random.h"

int rand_in_range(Random random, int lower, int upper) {
  int range = upper - lower;
  uint32_t rval = random_next_uint32(random);

  return lower + (rval % range);
}
