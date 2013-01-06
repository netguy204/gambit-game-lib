/* EGL implementation of testlib targeting the raspberry pi */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>
#include <sys/time.h>

#include "testlib.h"
#include "testlib_gl.h"
#include "testlib_internal.h"
#include "gl_headers.h"
#include "utils.h"

#include <android/native_window.h>
#include <android/looper.h>

EGLDisplay display;
EGLSurface surface;
EGLContext context;

void egl_assert_(int test, const char * string) {
  if(!test) {
    LOGW("egl_assert: %s -> %s\n", string,
         eglQueryString(display, eglGetError()));
    exit(-1);
  }
}

#define egl_assert(test) egl_assert_(test, "" #test)

struct timeval start_time;
struct InputState_ input_state;

void native_init() {
  gettimeofday(&start_time, NULL);
}

long time_millis() {
  struct timeval now_time;
  gettimeofday(&now_time, NULL);

  long delta_secs = now_time.tv_sec - start_time.tv_sec;
  long delta_usecs = now_time.tv_usec - start_time.tv_usec;
  return (delta_secs * 1000) + (delta_usecs / 1000);
}

void sleep_millis(long millis) {
  usleep(millis * 1000);
}

int sign(int val) {
  if(val > 0) return 1;
  if(val < 0) return -1;
  return 0;
}

void inputstate_latest(InputState req_state) {
  memcpy(req_state, &input_state, sizeof(InputState_));
}

void renderer_init(void*) {
  // not used
  LOGI("renderer_init");
}

void renderer_shutdown(void* empty) {
  LOGI("renderer_shutdown");
  renderer_gl_shutdown();
}

void at_exit() {
}
