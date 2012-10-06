#ifndef RANDOM_H
#define RANDOM_H

#include "SFMT.h"

typedef struct Random_ {
  sfmt_t sfmt;
  uint32_t phase;
  float u, v;
} *Random;

#define random_next_uint32(random) sfmt_genrand_uint32((sfmt_t*)random)

void random_init(Random random, uint32_t seed);
int rand_in_range(Random random, int lower, int upper);
float random_next_gaussian(Random random);

#endif
