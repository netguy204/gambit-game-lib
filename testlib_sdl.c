/* SDL/OpenGL implementation of testlib suitable for desktops */

#include <SDL/SDL.h>
#include <SDL/SDL_opengl.h>

#include "testlib.h"
#include "testlib_internal.h"

// include common code that is dependant on the platform variable
// location/name of the opengl headers
#define glOrthof glOrtho
#include "testlib_gl.c"

extern StackAllocator frame_allocator;

static struct InputState_ pstate;

void native_init() {
  memset(&pstate, 0, sizeof(struct InputState_));
  screen_width = 1360;
  screen_height = 768;
}


InputState frame_inputstate() {
  SDL_Event event;
  InputState state = stack_allocator_alloc(frame_allocator, sizeof(struct InputState_));
  memset(state, 0, sizeof(struct InputState_));

  /* pump the events */
  while(SDL_PollEvent(&event)) {
    int keydown = 0;

    switch(event.type) {
    case SDL_QUIT:
      state->quit_requested = 1;
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
      default:
        break;
      }
    }
  }

  state->leftright = pstate.leftright;
  state->updown = pstate.updown;

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
  SDL_Surface* screen = SDL_SetVideoMode(screen_width, screen_height,
                                         16, SDL_OPENGL);
  if(screen == NULL) {
    fprintf(stderr, "Unable to set %dx%d video: %s\n",
            screen_width, screen_height, SDL_GetError());
    exit(1);
  }

  renderer_gl_init();
}

void renderer_shutdown(void* empty) {
  renderer_gl_shutdown();
}

void at_exit() {
  SDL_Quit();
}

void signal_render_complete(void* empty) {
  SDL_GL_SwapBuffers();
}

