#include <SDL/SDL.h>
#include <stdio.h>
#include "testlib.h"

int last_time;
int min_time = 18;

void loop_once() {
  SDL_Event event;
  int new_time;

  /* pump the events */
  while(SDL_PollEvent(&event)) {
    switch(event.type) {
    case SDL_QUIT:
      terminate();
      exit(0);
      break;
    }
  }

  /* check the time */
  new_time = SDL_GetTicks();
  if((new_time - last_time) < min_time) {
    SDL_Delay(min_time - (new_time - last_time));
    new_time = SDL_GetTicks();
  }

  step(new_time - last_time);
  last_time = new_time;
}

int real_main(int argc, char ** argv, SDL_Surface* screen) {
  int last_time = SDL_GetTicks();

  set_screen(screen);

  while(1) {
    loop_once();
  }

  return 0;
}
