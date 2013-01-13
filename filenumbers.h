#ifndef FILENUMBERS_H
#define FILENUMBERS_H

#include <stdio.h>

void read_ushort(FILE* fh, unsigned short* value);

// a normalized fixed number works best for values between 0 and 1
void read_norm_fixed(FILE* fh, float* value);

// a non-normalized fixed number works for floats that are +/- about 250
void read_fixed(FILE* fh, float* value);

#endif
