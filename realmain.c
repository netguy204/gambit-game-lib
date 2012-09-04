#include <stdio.h>
#include "testlib.h"

#define min_time 18
#define max_time 100

int last_time;

int loop_once() {
  int new_time;

  InputState state = frame_inputstate();
  if(!gambit_running || state->quit_requested) {
    if(gambit_running) terminate();
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
  if(gambit_running) step(delta, state);
  end_frame();

  last_time = new_time;

  return 1;
}

int real_main(int argc, char ** argv) {
  gambit_running = 1;
  int last_time = time_millis();
  lib_init();

  while(loop_once()) {}

  return 0;
}
