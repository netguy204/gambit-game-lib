#include <SDL/SDL.h>
#include <stdio.h>
#include "testlib.h"

int last_time;
int min_time = 18;
int max_time = 100;

void loop_once() {
  SDL_Event event;
  int new_time;

  /* pump the events */
  while(SDL_PollEvent(&event)) {
    switch(event.type) {
    case SDL_QUIT:
      terminate();
      images_free();
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

  float delta = new_time - last_time;
  if(delta > max_time) {
    delta = max_time;
  }

  begin_frame();
  step(delta);
  end_frame();

  last_time = new_time;
}

int real_main(int argc, char ** argv) {
  int last_time = SDL_GetTicks();
  lib_init();

  while(1) {
    loop_once();
  }

  return 0;
}
