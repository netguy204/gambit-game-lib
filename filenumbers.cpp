#include "filenumbers.h"

#ifdef __ANDROID__
#include <sys/endian.h>
#else
#include <arpa/inet.h>
#endif

#define NORM_COORD_SCALE (1 << 15)
#define COORD_SCALE (1 << 15)

void read_ushort(FILE* fh, unsigned short* value) {
  fread(value, sizeof(unsigned short), 1, fh);
  *value = ntohs(*value);
}

void read_short(FILE* fh, short* value) {
  read_ushort(fh, (unsigned short*)value);
}

void read_int(FILE* fh, int* value) {
  fread(value, sizeof(int), 1, fh);
  *value = ntohl(*value);
}

void read_norm_fixed(FILE* fh, float* value) {
  short fixed_value;
  read_short(fh, &fixed_value);
  *value = ((double)fixed_value) / NORM_COORD_SCALE;
}

void read_fixed(FILE* fh, float* value) {
  int fixed_value;
  read_int(fh, &fixed_value);
  *value = ((double)fixed_value) / COORD_SCALE;
}
