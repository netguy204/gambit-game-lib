#include "vector.h"

void vector_add(Vector* dst, Vector* a, Vector* b) {
  dst->x = a->x + b->x;
  dst->y = a->y + b->y;
}

void vector_sub(Vector* dst, Vector* a, Vector* b) {
  dst->x = a->x - b->x;
  dst->y = a->y - b->y;
}

void vector_scale(Vector* dst, Vector* a, float s) {
  dst->x = a->x * s;
  dst->y = a->y * s;
}

void vector_integrate(Vector* dst, Vector* r, Vector* dr, float dt) {
  dst->x = r->x + (dr->x * dt);
  dst->y = r->y + (dr->y * dt);
}
