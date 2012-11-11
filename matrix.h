#ifndef MATRIX_H
#define MATRIX_H

typedef struct Matrix44_ {
  float data[16];
} *Matrix44;

void matrix_identity(Matrix44 matrix);

void matrix_orthographic_proj(Matrix44 matrix, float xmin, float xmax,
                              float ymin, float ymax, float zmin, float zmax);


#endif
