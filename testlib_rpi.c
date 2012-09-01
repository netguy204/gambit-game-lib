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

#include "bcm_host.h"

#include "GLES/gl.h"
#include "EGL/egl.h"
#include "EGL/eglext.h"

EGLDisplay display;
EGLSurface surface;
EGLContext context;

extern StackAllocator frame_allocator;

// include common code that is dependant on the platform variable
// location/name of the opengl headers
#include "testlib_gl.c"

void egl_assert_(int test, const char * string) {
  if(!test) {
    fprintf(stderr, "egl_assert: %s -> %s\n", string,
            eglQueryString(display, eglGetError()));
    exit(-1);
  }
}

#define egl_assert(test) egl_assert_(test, "" #test)

/* GLES2
static GLbyte vShaderStr[] =
  "attribute vec4 vPosition; \n"
  "void main() {  \n"
  "  gl_Position = vPosition; \n"
  "}\n";

static GLbyte fShaderStr[] =
  "precision mediump float;\n"
  "void main() {  \n"
  "  gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0); \n"
  "}\n";

GLuint load_shader(GLenum type, const char * src) {
  GLuint shader;
  GLint compiled;

  shader = glCreateShader(type);
  if(shader == 0) return 0;

  glShaderSource(shader, 1, &src, NULL);
  glCompilerShader(shader);
  glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
  if(!compiled) {
    GLint info_len = 0;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &info_len);
    if(info_len > 0) {
      char* log = malloc(info_len);
      glGetShaderInfoLog(shader, info_len, NULL, log);
      fprintf(stderr, "Error compiling shader:\n%s\n", log);
      free(log);
    }

    glDeleteShader(shader);
    return 0;
  }
  return shader;
}
*/

struct timeval start_time;
js_state joystick_state;

void native_init() {
  gettimeofday(&start_time, NULL);
  joystick_state = joystick_open("/dev/input/js0");
}

long time_millis() {
  struct timeval now_time;
  gettimeofday(&now_time);

  long delta_secs = now_time.tv_sec - start_time.tv_sec;
  long delta_usecs = now_time.tv_usec - start_time.tv_usec;
  return (delta_secs * 1000) + (delta_usecs / 1000);
}

void sleep_millis(long millis) {
  usleep(millis * 1000);
}

InputState frame_inputstate() {
  InputState state = stack_allocator_alloc(frame_allocator, sizeof(struct InputState_));
  memset(state, 0, sizeof(struct InputState_));

  joystick_update_state(joystick_state);

  /*
  if(joystick_state->values[4].value != state->leftright
     || joystick_state->values[5].value != state->updown) {
    joystick_print_state(joystick_state);
  }
  joystick_print_state(joystick_state);
  */

  state->leftright = joystick_state->values[4].value;
  state->updown = -joystick_state->values[5].value;

  return state;
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
  
  // create an EGL rendering context
  context = eglCreateContext(display, config, EGL_NO_CONTEXT, NULL);
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
  
  dispman_element =
    vc_dispmanx_element_add ( dispman_update, dispman_display,
                              0/*layer*/, &dst_rect, 0/*src*/,
                              &src_rect, DISPMANX_PROTECTION_NONE, 0 /*alpha*/, 0/*clamp*/, 0/*transform*/);
  
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

void signal_render_complete(void* empty) {
  threadbarrier_wait(render_barrier);
  eglSwapBuffers(display, surface);
  gl_check_("endframe");
}

