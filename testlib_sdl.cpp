/* SDL/OpenGL implementation of testlib suitable for desktops */

#include <SDL/SDL.h>

#include "testlib.h"
#include "testlib_internal.h"
#include "gl_headers.h"

static struct InputState_ pstate;

void native_init() {
  memset(&pstate, 0, sizeof(struct InputState_));
  /* appears to be the ouya default resolution
  screen_width = 1920;
  screen_height = 1080;
  */
  screen_width = 1280;
  screen_height = 720;
}


void inputstate_latest(InputState state) {
  SDL_Event event;

  /* pump the events */
  while(SDL_PollEvent(&event)) {
    int keydown = 0;

    switch(event.type) {
    case SDL_QUIT:
      pstate.quit_requested = 1;
      break;
    case SDL_KEYDOWN:
      keydown = 1;
    }

    if(event.type == SDL_KEYDOWN ||
       event.type == SDL_KEYUP) {
      switch(event.key.keysym.sym) {
      case SDLK_LEFT:
        pstate.leftright = keydown * -1;
        break;
      case SDLK_RIGHT:
        pstate.leftright = keydown * 1;
        break;
      case SDLK_DOWN:
        pstate.updown = keydown * -1;
        break;
      case SDLK_UP:
        pstate.updown = keydown * 1;
        break;
      case SDLK_z:
        pstate.action1 = keydown * 1;
        break;
      case SDLK_x:
        pstate.action2 = keydown * 1;
        break;
      case SDLK_c:
        pstate.action3 = keydown * 1;
        break;
      default:
        break;
      }
    }
  }

  *state = pstate;
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
  SDL_Surface* screen = SDL_SetVideoMode(screen_width, screen_height,
                                         16, SDL_OPENGL);
  if(screen == NULL) {
    fprintf(stderr, "Unable to set %dx%d video: %s\n",
            screen_width, screen_height, SDL_GetError());
    exit(1);
  }

#ifndef __APPLE__
  GLenum err = glewInit();
  if(err != GLEW_OK) {
    fprintf(stderr, "Failed to initialize GLEW\n");
    exit(1);
  }
#endif

  renderer_gl_init(screen_width, screen_height);
}

void renderer_shutdown(void* empty) {
  renderer_gl_shutdown();
}

void at_exit() {
  SDL_Quit();
}

void signal_render_complete(void* _allocator) {
  StackAllocator allocator = (StackAllocator)_allocator;
  SDL_GL_SwapBuffers();
  render_reply_queue->enqueue(allocator);
}
