/* SDL/OpenGL implementation of testlib suitable for desktops */

#include <SDL/SDL.h>
#include <SDL/SDL_opengl.h>

#include "testlib.h"
#include "testlib_internal.h"

extern StackAllocator frame_allocator;

InputState frame_inputstate() {
  SDL_Event event;
  InputState state = stack_allocator_alloc(frame_allocator, sizeof(struct InputState_));
  memset(state, 0, sizeof(struct InputState_));

  /* pump the events */
  while(SDL_PollEvent(&event)) {
    switch(event.type) {
    case SDL_QUIT:
      state->quit_requested = 1;
    }
  }

  return state;
}

long time_millis() {
  return SDL_GetTicks();
}

void sleep_millis(long millis) {
  SDL_Delay(millis);
}

void renderer_init(void* empty) {
  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
    exit(1);
  }

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_Surface* screen = SDL_SetVideoMode(640, 480, 16, SDL_OPENGL);
  if(screen == NULL) {
    fprintf(stderr, "Unable to set 640x480 video: %s\n", SDL_GetError());
    exit(1);
  }

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClearColor(0.8f, 0.8f, 0.8f, 0.0f);
  glViewport(0, 0, 640, 480);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0.0f, 640, 0.0f, 480.0f, -1.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

void renderer_shutdown(void* empty) {
  SDL_Quit();
  threadbarrier_wait(render_barrier);
}

void renderer_begin_frame(void* empty) {
  glClear(GL_COLOR_BUFFER_BIT);
}

void signal_render_complete(void* empty) {
  threadbarrier_wait(render_barrier);
  SDL_GL_SwapBuffers();
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

  glBegin(GL_QUADS);
  
  glTexCoord2i(0, 1);
  glVertex3f(0, 0, 0.0f);

  glTexCoord2i(1, 1);
  glVertex3f(img->w, 0, 0.0f);

  glTexCoord2i(1, 0);
  glVertex3f(img->w, img->h, 0.0f);

  glTexCoord2i(0, 0);
  glVertex3f(0, img->h, 0.0f);

  glEnd();

  glPopMatrix();
}

