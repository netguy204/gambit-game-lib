#include "vector.h"
#include <math.h>

void vector_add(Vector dst, Vector a, Vector b) {
  dst->x = a->x + b->x;
  dst->y = a->y + b->y;
}

void vector_sub(Vector dst, Vector a, Vector b) {
  dst->x = a->x - b->x;
  dst->y = a->y - b->y;
}

void vector_scale(Vector dst, Vector a, float s) {
  dst->x = a->x * s;
  dst->y = a->y * s;
}

float vector_dot(Vector a, Vector b) {
  float x = a->x * b->x;
  float y = a->y * b->y;
  return x + y;
}

float vector_mag(Vector a) {
  return sqrt(vector_dot(a, a));
}

void vector_norm(Vector dst, Vector src) {
  vector_scale(dst, src, 1.0f / vector_mag(src));
}

float vector_angle(Vector v) {
  float angle = atan2(v->y, v->x);
  return angle * 180.0f / M_PI;
}

int vector_direction_scaled(Vector dst, Vector a, Vector b, float s) {
  struct Vector_ ba;
  vector_sub(&ba, a, b);

  float mag = vector_mag(&ba);
  if(mag < 0.01) return 0;

  vector_scale(dst, &ba, s / mag);
  return 1;
}

void vector_integrate(Vector dst, Vector r, Vector dr, float dt) {
  dst->x = r->x + (dr->x * dt);
  dst->y = r->y + (dr->y * dt);
}

void vector_clamp(Vector dst, Vector src, float max) {
  float mag = vector_mag(src);
  if(mag > max) {
    vector_scale(dst, src, max / mag);
  } else {
    *dst = *src;
  }
}
