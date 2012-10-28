#ifndef UTILS_H
#define UTILS_H

#include <sys/time.h>

typedef struct Timer_ {
  struct timeval val;
} *Timer;

void timer_start(Timer timer);
long timer_elapsed_msecs(Timer timer);

#define PROFILE_START(timer, message) do {      \
    printf(message);                            \
    printf(": ");                               \
    fflush(stdout);                             \
    timer_start(timer);                         \
  } while(0)

#define PROFILE_END(timer) do {                 \
    long msecs = timer_elapsed_msecs(timer);    \
    printf("%ld\n", msecs);                     \
  } while(0)

#endif
