#include "testlib.h"

int fib(int n) {
  int temp;
  int a = 1;
  int b = 1;

  if(n == 0 || n == 1) return 1;

  while(n-- > 2) {
    temp = a + b;
    a = b;
    b = temp;
    enum_object(b);
  }

  return b;
}
