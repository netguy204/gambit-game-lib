#ifndef VECTOR_H
#define VECTOR_H

typedef struct Vector_ {
  float x;
  float y;
} *Vector;

void vector_add(Vector dst, Vector a, Vector b);
void vector_sub(Vector dst, Vector a, Vector b);
void vector_scale(Vector dst, Vector a, float s);
float vector_dot(Vector a, Vector b);
float vector_mag(Vector a);
void vector_zero(Vector a);
void vector_norm(Vector dst, Vector src);
float vector_angle(Vector v);
int vector_direction_scaled(Vector dst, Vector a, Vector b, float s);
void vector_integrate(Vector dst, Vector r, Vector dr, float dt);
void vector_clamp(Vector dst, Vector src, float max);
float vector_scalarproject(Vector src, Vector onto);
float vector_project2(Vector dst, Vector src, Vector normonto);
float vector_dist2(Vector a, Vector b);
float vector_dist(Vector a, Vector b);

#endif
