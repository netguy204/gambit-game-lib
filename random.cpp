#include "random.h"

#include <math.h>

int rand_in_range(Random random, int lower, int upper) {
  int range = upper - lower;
  uint32_t rval = random_next_uint32(random);

  return lower + (rval % range);
}

void random_init(Random random, uint32_t seed) {
  sfmt_init_gen_rand((sfmt_t*)random, seed);
  random->phase = 0;
}

// Abramowitz and Stegun: http://c-faq.com/lib/gaussian.html
float random_next_gaussian(Random random) {
  float z;

  if(random->phase == 0) {
    random->u = (random_next_uint32(random) + 1.0f) / (UINT32_MAX + 2.0f);
    random->v = random_next_uint32(random) / (UINT32_MAX + 1.0f);
    z = sqrt(-2.0f * logf(random->u)) * sinf(2.0f * M_PI * random->v);
  } else {
    z = sqrt(-2.0f * logf(random->u)) * cosf(2.0f * M_PI * random->v);
  }

  random->phase = 1 - random->phase;

  return z;
}

void random_shuffle_bytes(Random random, unsigned char* bytes, int nbytes) {
  int ii;
  for(ii = 0; ii < nbytes - 1; ++ii) {
    int swap_tgt = rand_in_range(random, ii + 1, nbytes);
    unsigned char temp = bytes[ii];
    bytes[ii] = bytes[swap_tgt];
    bytes[swap_tgt] = temp;
  }
}
