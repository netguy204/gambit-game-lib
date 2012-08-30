/* SDL_Image/EGL implementation of testlib targeting the raspberry
   pi */

#include <SDL/SDL_image.h>

#include "bcm_host.h"

#include "GLES/gl.h"
#include "EGL/egl.h"
#include "EGL/eglext.h"

EGLDisplay display;
EGLSurface surface;
EGLContext context;
uint32_t screen_width;
uint32_t screen_height;

static const GLFloat quadCoords[4 * 3] = {
  0.0f, 0.0f, 0.0f,
  1.0f, 0.0f, 0.0f,
  1.0f, 1.0f, 0.0f,
  0.0f, 1.0f, 0.0f,
};

static const GLFloat texCoords[4 * 2] = {
  0.0f, 1.0f,
  1.0f, 1.0f,
  1.0f, 0.0f,
  0.0f, 0.0f,
};

void renderer_init(void* empty) {
  IMG_Init(IMG_INIT_PNG);
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
  
  // get an EGL display connection
  display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
  assert(display!=EGL_NO_DISPLAY);
  
  // initialize the EGL display connection
  result = eglInitialize(display, NULL, NULL);
  assert(EGL_FALSE != result);
  
   // get an appropriate EGL frame buffer configuration
  result = eglChooseConfig(display, attribute_list, &config, 1, &num_config);
  assert(EGL_FALSE != result);
  
  // create an EGL rendering context
  context = eglCreateContext(display, config, EGL_NO_CONTEXT, NULL);
  assert(context!=EGL_NO_CONTEXT);
  
  // create an EGL window surface
  success = graphics_get_display_size(0 /* LCD */, &screen_width, &screen_height);
  assert( success >= 0 );
  
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
  assert(surface != EGL_NO_SURFACE);

  // connect the context to the surface
  result = eglMakeCurrent(display, surface, surface, context);
  assert(EGL_FALSE != result);
  
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClearColor(0.8f, 0.8f, 0.8f, 0.0f);
  glViewport(0, 0, screen_width, screen_height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0.0f, screen_width, 0.0f, screen_height, -1.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  // Set background color and clear buffers
  //glClearColor(0.15f, 0.25f, 0.35f, 1.0f);
  //glClear( GL_COLOR_BUFFER_BIT );
  //glClear( GL_DEPTH_BUFFER_BIT );
  //glShadeModel(GL_FLAT);
  
  // Enable back face culling.
  //glEnable(GL_CULL_FACE);

  // set up for drawing just quads
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, quadCoords);

  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, 0, texCoords);
}

void renderer_shutdown(void* empty) {
  IMG_Quit();
  glClear(GL_COLOR_BUFFER_BIT);
  // Release OpenGL resources
  eglMakeCurrent( display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT );
  eglDestroySurface( display, surface );
  eglDestroyContext( display, context );
  eglTerminate( display );
}

void renderer_begin_frame(void* empty) {
  glClear(GL_COLOR_BUFFER_BIT);
}

void signal_render_complete(void* empty) {
  threadbarrier_wait(render_barrier);
  eglSwapBuffers(display, surface);
}

void renderer_finish_image_load(ImageResource resource) {
  GLuint texture;
  GLenum texture_format;
  GLint num_colors;

  num_colors = resource->channels;
  if(num_colors == 4) {
    texture_format = GL_RGBA;
  } else {
    texture_format = GL_RGB;
  }

  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, num_colors, resource->w, resource->h, 0,
               texture_format, GL_UNSIGNED_BYTE, resource->data);

  resource->texture = texture;

  free(resource->data);
}

void renderer_finish_image_free(void* texturep) {
  GLuint texture = *(GLuint*)texturep;
  glDeleteTextures(1, &texture);
}

void image_render_to_screen(ImageResource img, float angle,
                            float cx, float cy,
                            float x, float y) {
  glBindTexture(GL_TEXTURE_2D, img->texture);
  glPushMatrix();
  
  glTranslatef(x, y, 0.0f);
  glRotatef(angle, 0.0f, 0.0f, 1.0f);
  glTranslatef(-cx, -cy, 0.0f);
  glScalef(img->w, img->h, 1.0f);

  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

  glPopMatrix();
}
