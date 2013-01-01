/* EGL implementation of testlib targeting the raspberry pi */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>

#include "testlib.h"
#include "testlib_internal.h"
#include "joystick.h"
#include "gl_headers.h"
#include "bcm_host.h"

EGLDisplay display;
EGLSurface surface;
EGLContext context;

void egl_assert_(int test, const char * string) {
  if(!test) {
    fprintf(stderr, "egl_assert: %s -> %s\n", string,
            eglQueryString(display, eglGetError()));
    exit(-1);
  }
}

#define egl_assert(test) egl_assert_(test, "" #test)

struct timeval start_time;
js_state joystick_state;

void native_init() {
  gettimeofday(&start_time, NULL);
  joystick_state = joystick_open("/dev/input/js0");
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

void inputstate_latest(InputState state) {
  memset(state, 0, sizeof(struct InputState_));

  joystick_update_state(joystick_state);

  state->leftright = ((float)joystick_state->values[1].value) / 32767.0;
  state->updown = -((float)joystick_state->values[3].value) / 32767.0;
  state->action1 = joystick_state->values[0].value;
  state->action2 = joystick_state->values[4].value;
//  state->action3 = joystick_state->values[5].value;
}

void renderer_init(void* empty) {
  int32_t success = 0;
  EGLBoolean result;
  EGLint num_config;

  static EGL_DISPMANX_WINDOW_T nativewindow;

  DISPMANX_ELEMENT_HANDLE_T dispman_element;
  DISPMANX_DISPLAY_HANDLE_T dispman_display;
  DISPMANX_UPDATE_HANDLE_T dispman_update;
  VC_RECT_T dst_rect;
  VC_RECT_T src_rect;

  static const EGLint attribute_list[] =
    {
      EGL_RED_SIZE, 8,
      EGL_GREEN_SIZE, 8,
      EGL_BLUE_SIZE, 8,
      EGL_ALPHA_SIZE, 8,
      EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
      EGL_NONE
    };

   static const EGLint context_attributes[] =
   {
      EGL_CONTEXT_CLIENT_VERSION, 2,
      EGL_NONE
   };

  EGLConfig config;

  bcm_host_init();

  // get an EGL display connection
  display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
  egl_assert(display!=EGL_NO_DISPLAY);

  // initialize the EGL display connection
  int major, minor;
  result = eglInitialize(display, &major, &minor);
  egl_assert(EGL_FALSE != result);
  fprintf(stderr, "EGL initialzed version %d %d\n", major, minor);

   // get an appropriate EGL frame buffer configuration
  result = eglChooseConfig(display, attribute_list, &config, 1, &num_config);
  egl_assert(EGL_FALSE != result);

  result = eglBindAPI(EGL_OPENGL_ES_API);
  egl_assert(EGL_FALSE != result);

  // create an EGL rendering context
  context = eglCreateContext(display, config, EGL_NO_CONTEXT, context_attributes);
  egl_assert(context!=EGL_NO_CONTEXT);

  // create an EGL window surface
  success = graphics_get_display_size(0 /* LCD */, &screen_width, &screen_height);
  fprintf(stderr, "success = %d, screen_width = %d, screen_height = %d\n", success, screen_width, screen_height);
  egl_assert( success >= 0 );

  dst_rect.x = 0;
  dst_rect.y = 0;
  dst_rect.width = screen_width;
  dst_rect.height = screen_height;

  src_rect.x = 0;
  src_rect.y = 0;
  src_rect.width = screen_width << 16;
  src_rect.height = screen_height << 16;

  dispman_display = vc_dispmanx_display_open( 0 /* LCD */);
  dispman_update = vc_dispmanx_update_start( 0 );
  VC_DISPMANX_ALPHA_T alpha;
  alpha.flags = DISPMANX_FLAGS_ALPHA_FROM_SOURCE;
  alpha.opacity = 255;
  dispman_element =
    vc_dispmanx_element_add ( dispman_update, dispman_display,
                              0/*layer*/, &dst_rect, 0/*src*/,
                              &src_rect, DISPMANX_PROTECTION_NONE, &alpha/*alpha*/, 0/*clamp*/, DISPMANX_NO_ROTATE/*transform*/);

  nativewindow.element = dispman_element;
  nativewindow.width = screen_width;
  nativewindow.height = screen_height;
  vc_dispmanx_update_submit_sync( dispman_update );

  surface = eglCreateWindowSurface( display, config, &nativewindow, NULL );
  egl_assert(surface != EGL_NO_SURFACE);

  // connect the context to the surface
  result = eglMakeCurrent(display, surface, surface, context);
  egl_assert(EGL_FALSE != result);

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
