#ifndef VECTOR_H
#define VECTOR_H

typedef struct {
  float x;
  float y;
} Vector;

void vector_add(Vector* dst, Vector* a, Vector* b);
void vector_sub(Vector* dst, Vector* a, Vector* b);
void vector_scale(Vector* dst, Vector* a, float s);

void vector_integrate(Vector* dst, Vector* r, Vector* dr, float dt);

#endif
