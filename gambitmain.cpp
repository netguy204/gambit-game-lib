#include <stdlib.h>

extern int real_main(int, char**);

// required by things that read files
FILE* nativeOpen(const char* fname) {
  return fopen(fname, "rb");
}

int main(int argc, char ** argv) {
  int result = real_main(argc, argv);
  return result;
}
