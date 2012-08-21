#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>

#define ___VERSION 406006
#include <gambit.h>

#define SCHEME_LIBRARY_LINKER ____20_scmlib__
___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

extern int real_main(int argc, char ** argv, SDL_Surface* screen);

void shutdown() {
  IMG_Quit();
  SDL_Quit();
}

int main(int argc, char ** argv) {
  if ( SDL_Init(SDL_INIT_AUDIO|SDL_INIT_VIDEO) < 0 ) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
    exit(1);
  }
  IMG_Init(IMG_INIT_PNG);

  atexit(shutdown);

  SDL_Surface *screen;
  screen = SDL_SetVideoMode(640, 480, 16, SDL_HWSURFACE);
  if(screen == NULL) {
    fprintf(stderr, "Unable to set 640x480 video: %s\n", SDL_GetError());
    exit(1);
  }


  ___setup_params_struct setup_params;
  ___setup_params_reset(&setup_params);
  setup_params.version = ___VERSION;
  setup_params.linker = SCHEME_LIBRARY_LINKER;

  ___setup(&setup_params);

  int result = real_main(argc, argv, screen);

  ___cleanup();

  return result;
}
