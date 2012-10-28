#include "utils.h"

#include <stdlib.h>

void timer_start(Timer timer) {
  gettimeofday((struct timeval*)timer, NULL);
}

long timer_elapsed_msecs(Timer timer) {
  struct timeval now;
  gettimeofday(&now, NULL);

  long dsecs = now.tv_sec - timer->val.tv_sec;
  long dusecs = now.tv_usec - timer->val.tv_usec;

  return dsecs * 1000 + (dusecs / 1000);
}
