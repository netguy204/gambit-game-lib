#include "matrix.h"

void matrix_identity(Matrix44 matrix) {
  matrix->data[0] = 1.0f;
  matrix->data[1] = 0.0f;
  matrix->data[2] = 0.0f;
  matrix->data[3] = 0.0f;

  matrix->data[4] = 0.0f;
  matrix->data[5] = 1.0f;
  matrix->data[6] = 0.0f;
  matrix->data[7] = 0.0f;

  matrix->data[8] = 0.0f;
  matrix->data[9] = 0.0f;
  matrix->data[10] = 1.0f;
  matrix->data[11] = 0.0f;

  matrix->data[12] = 0.0f;
  matrix->data[13] = 0.0f;
  matrix->data[14] = 0.0f;
  matrix->data[15] = 1.0f;
}

void matrix_orthographic_proj(Matrix44 matrix, float xmin, float xmax,
                              float ymin, float ymax, float zmin, float zmax) {
  matrix_identity(matrix);
  matrix->data[0] = 2.0f / (xmax - xmin);
  matrix->data[5] = 2.0f / (ymax - ymin);
  matrix->data[10] = -2.0f / (zmax - zmin);
  matrix->data[12] = -((xmax + xmin)/(xmax - xmin));
  matrix->data[13] = -((ymax + ymin)/(ymax - ymin));
  matrix->data[14] = -((zmax + zmin)/(zmax - zmin));
  matrix->data[15] = 1.0f;
}
