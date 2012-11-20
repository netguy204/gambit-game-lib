#include "perlin.h"

#include "config.h"

#include <math.h>

// based on http://mrl.nyu.edu/~perlin/noise/

void perlin_init(Perlin perlin, Random rng, Vector offset, Vector scale) {
  perlin->offset = *offset;
  perlin->scale = *scale;
  perlin->z = 0.0f;

  unsigned int ii = 0;
  for(ii = 0; ii < 256; ++ii) {
    perlin->state[ii] = ii;
  }

  random_shuffle_bytes(rng, perlin->state, 256);

  // duplicate those bytes into the next 256 slots
  for(ii = 0; ii < 256; ++ii) {
    perlin->state[ii + 256] = perlin->state[ii];
  }
}

static float fade(float t) {
  return t * t * t * (t * (t* 6 - 15) + 10);
}

static float lerp(float t, float a, float b) {
  return a + t * (b - a);
}

static float grad(int hash, float x, float y, float z) {
  int h = hash & 15;
  float u = h < 8 ? x : y;
  float v = h < 4 ? y : h==12 || h == 14 ? x : z;
  return ((h & 1) == 0 ? u : -u) + ((h & 2) == 0 ? v : -v);
}

float perlin_sample(Perlin perlin, Vector point) {
  // the paper describes 3d noise. we assume that z = 0

  struct Vector_ spoint;
  vector_sub(&spoint, point, &perlin->offset);
  spoint.x *= perlin->scale.x;
  spoint.y *= perlin->scale.y;

  // find the unit square that contains point
  float fx = floorf(spoint.x);
  float fy = floorf(spoint.y);
  float fz = floorf(perlin->z);

  int X = (int)fx & 255;
  int Y = (int)fy & 255;
  int Z = (int)fz & 256;

  // make spoint relative to square coords
  float x = spoint.x - fx;
  float y = spoint.y - fy;
  float z = perlin->z - fz;

  // compute fade
  float u = fade(x);
  float v = fade(y);
  float w = fade(z);

  unsigned char* p = perlin->state;

  int A = p[X] + Y;
  int AA = p[A] + Z;
  int AB = p[A+1] + Z;
  int B = p[X+1] + Y;
  int BA = p[B] + Z;
  int BB = p[B+1] + Z;

  return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),
                              grad(p[BA  ], x-1, y  , z   )),
                      lerp(u, grad(p[AB  ], x  , y-1, z   ),
                           grad(p[BB  ], x-1, y-1, z   ))),
              lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),
                           grad(p[BA+1], x-1, y  , z-1 )),
                   lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
                        grad(p[BB+1], x-1, y-1, z-1 ))));
}
