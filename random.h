#ifndef RANDOM_H
#define RANDOM_H

#include "SFMT.h"

typedef struct Random_ {
  sfmt_t sfmt;

} *Random;

#define random_init(random, seed) sfmt_init_gen_rand((sfmt_t*)random, seed)
#define random_next_uint32(random) sfmt_genrand_uint32((sfmt_t*)random)

int rand_in_range(Random random, int lower, int upper);


#endif
