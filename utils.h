#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <sys/time.h>

void* fail_exit(const char * message, ...);

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

long filename_size(const char* filename);
char* filename_slurp(const char* filename);

// provided by system specific lib
FILE* nativeOpen(const char* fname);


#ifdef __ANDROID__
#include <android/log.h>

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))

#else

#define LOGI(...) fprintf(stderr, "INFO: " __VA_ARGS__)
#define LOGW(...) fprintf(stderr, "WARNING: " __VA_ARGS__)

#endif

#endif
