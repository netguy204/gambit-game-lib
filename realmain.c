#include <SDL/SDL.h>
#include <stdio.h>
#include "testlib.h"

int real_main(int argc, char ** argv, SDL_Surface* screen) {
  set_screen(screen);

  SDL_Event event;
  while(1) {
    SDL_WaitEvent(&event);
    switch(event.type) {
    case SDL_QUIT:
      terminate();
      exit(0);
      break;
    default:
      step();
      break;
    }
  }
  return 0;
}
