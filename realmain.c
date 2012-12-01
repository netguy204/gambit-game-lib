#include <stdio.h>
#include "testlib.h"
#include "game.h"

#define min_time 15
#define max_time 100

static int last_time;
static GameStep game_step_fn;

void set_game_step(GameStep fn) {
  game_step_fn = fn;
}

int loop_once() {
  int new_time;

  struct InputState_ state;
  inputstate_latest(&state);
  if(!game_step_fn || state.quit_requested) {
    lib_shutdown();
    return 0;
  }

  /* check the time */
  new_time = time_millis();
  long old_delta = new_time - last_time;
  if(old_delta < min_time) {
    sleep_millis(min_time - old_delta);
    new_time = time_millis();
  }

  long delta = new_time - last_time;
  if(delta > max_time) {
    delta = max_time;
  }

  begin_frame();
  game_step_fn(delta, &state);
  end_frame();

  last_time = new_time;

  return 1;
}

int real_main(int argc, char ** argv) {
  lib_init();
  game_init();

  while(loop_once()) {}

  game_shutdown();
  return 0;
}
