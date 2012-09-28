#ifndef CONTROLS_H
#define CONTROLS_H

typedef struct RepeatingLatch_ {
  long last_time;
  float period;
  int latch_value, last_state;
} *RepeatingLatch;

struct Clock_;
int repeatinglatch_state(RepeatingLatch latch, struct Clock_* clock,
                         int input_state);

#endif
