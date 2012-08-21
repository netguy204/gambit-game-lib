#include <stdio.h>

int any_fail = 0;

#define ASSERT(x) do {                          \
  if(!(x)) {                                    \
    fprintf(stderr, "FAILED: "#x"\n");          \
    any_fail = 1;                               \
  }                                             \
  } while(0);

#define END_MAIN() return any_fail


