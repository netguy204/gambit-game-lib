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

#include <android_native_app_glue.h>

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

android_app* android_state;

void inputstate_latest(InputState req_state) {
  int events;
  int ident;
  struct android_poll_source* source;
  while ((ident=ALooper_pollAll(0, NULL, &events, (void**)&source)) >= 0) {

    // Process this event.
    if (source != NULL) {
      source->process(android_state, source);
    }

    // Check if we are exiting.
    if (android_state->destroyRequested != 0) {
      input_state.quit_requested = 1;
    }
  }

  memcpy(req_state, &input_state, sizeof(InputState_));
}

void renderer_init(void* empty) {
  int32_t success = 0;
  EGLBoolean result;
  EGLint num_config, format;

  static const EGLint attribute_list[] =
    {
      EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
      EGL_RED_SIZE, 8,
      EGL_GREEN_SIZE, 8,
      EGL_BLUE_SIZE, 8,
      EGL_ALPHA_SIZE, 8,
      EGL_NONE
    };

  EGLint major, minor;
  display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
  egl_assert(display!=EGL_NO_DISPLAY);

  result = eglInitialize(display, &major, &minor);
  egl_assert(EGL_FALSE != result);
  LOGI("EGL initialzed version %d %d\n", major, minor);

  // bind ES2
  result = eglBindAPI(EGL_OPENGL_ES_API);
  egl_assert(EGL_FALSE != result);

  static const EGLint context_attributes[] =
   {
      EGL_CONTEXT_CLIENT_VERSION, 2,
      EGL_NONE
   };

  EGLConfig config;
  result = eglChooseConfig(display, attribute_list, &config, 1, &num_config);
  egl_assert(EGL_FALSE != result);

  // find out what we format we got
  eglGetConfigAttrib(display, config, EGL_NATIVE_VISUAL_ID, &format);
  ANativeWindow_setBuffersGeometry(android_state->window, 0, 0, format);

  surface = eglCreateWindowSurface( display, config, android_state->window, NULL );
  egl_assert(surface != EGL_NO_SURFACE);

  context = eglCreateContext(display, config, EGL_NO_CONTEXT, context_attributes);
  egl_assert(context!=EGL_NO_CONTEXT);

  // connect the context to the surface
  result = eglMakeCurrent(display, surface, surface, context);
  egl_assert(EGL_FALSE != result);

  // get the sizes
  EGLint w, h;
  eglQuerySurface(display, surface, EGL_WIDTH, &w);
  eglQuerySurface(display, surface, EGL_HEIGHT, &h);
  screen_width = (int)w;
  screen_height = (int)h;

  renderer_gl_init();
}

void renderer_shutdown(void* empty) {
  renderer_gl_shutdown();

  // Release OpenGL resources
  eglMakeCurrent( display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT );
  eglDestroySurface( display, surface );
  eglDestroyContext( display, context );
  eglTerminate( display );
}

void at_exit() {
}

void signal_render_complete(void* _allocator) {
  StackAllocator allocator = (StackAllocator)_allocator;
  eglSwapBuffers(display, surface);
  gl_check_("endframe");
  render_reply_queue->enqueue(allocator);
}
