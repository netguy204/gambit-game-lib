#ifndef PERLIN_H
#define PERLIN_H

#include "random.h"
#include "vector.h"

typedef struct Perlin_ {
  struct Vector_ offset, scale;
  float z;
  unsigned char state[512];
} *Perlin;

void perlin_init(Perlin perlin, Random rng, Vector offset, Vector scale);

float perlin_sample(Perlin perlin, Vector point);

#endif
