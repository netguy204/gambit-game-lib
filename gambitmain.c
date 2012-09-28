#include <stdlib.h>

extern int real_main(int, char**);

int main(int argc, char ** argv) {
  int result = real_main(argc, argv);
  return result;
}
