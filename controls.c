#include "controls.h"
#include "testlib.h"

int repeatinglatch_state(RepeatingLatch latch, Clock clock, int input_state) {
  if(latch->last_state != input_state) {
    latch->last_state = input_state;
    latch->last_time = clock_time(clock);
    return input_state;
  } else {
    // has the clock expired?
    long dt_ticks = clock_time(clock) - latch->last_time;
    float dt = clock_cycles_to_seconds(dt_ticks);
    if(dt >= latch->period) {
      latch->last_time = clock_time(clock);
      return input_state;
    } else {
      // return the default value
      return latch->latch_value;
    }
  }
}
