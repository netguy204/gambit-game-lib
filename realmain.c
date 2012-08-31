#include <stdio.h>
#include "testlib.h"

int last_time;
int min_time = 18;
int max_time = 100;

int loop_once() {
  int new_time;

  InputState state = frame_inputstate();
  if(state->quit_requested) {
    terminate();
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
  step(delta);
  end_frame();

  last_time = new_time;

  return 1;
}

int real_main(int argc, char ** argv) {
  int last_time = time_millis();
  lib_init();

  while(loop_once()) {}

  return 0;
}
