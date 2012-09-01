#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <memory.h>

#include "joystick.h"

const char* type2str(__u8 type) {
  type &= ~JS_EVENT_INIT;
  switch(type) {
  case JS_EVENT_BUTTON:
    return "JS_EVENT_BUTTON";
    break;
  case JS_EVENT_AXIS:
    return "JS_EVENT_AXIS";
    break;
  case JS_EVENT_INIT:
    return "JS_EVENT_INIT";
    break;
  default:
    return "UNKNOWN";
  }
}

void js_event_print(struct js_event* e) {
  printf("%s: time=%d value=%d number=%d\n", type2str(e->type),
	 e->time, e->value, e->number);
}

int joystick_maybe_read(int fileno, struct js_event* e) {
  if(read(fileno, e, sizeof(struct js_event)) <= 0) {
    if(errno != EAGAIN) {
      fprintf(stderr, "error reading from joystick\n");
      exit(-1);
    }
    return 0;
  } else {
    return 1;
  }
}

js_state joystick_open(const char* device) {
  int fd = open ("/dev/input/js0", O_RDONLY|O_NONBLOCK);

  int capacity = 16;
  js_state state = malloc(sizeof(struct js_state_));
  state->num_values = 0;
  state->capacity = capacity;
  state->fileno = fd;
  state->values = malloc(capacity * sizeof(struct js_event));
  memset(state->values, 0, capacity * sizeof(struct js_event));

  joystick_update_state(state);
  return state;
}

void joystick_close(js_state state) {
  close(state->fileno);
  free(state->values);
  free(state);
}

void joystick_update_state(js_state state) {
  struct js_event e;
  while(joystick_maybe_read(state->fileno, &e)) {
    if(e.number >= state->num_values) {
      if(e.number >= state->capacity) {
	int new_cap = e.number + 1;
	state->values = realloc(state->values, new_cap * sizeof(struct js_event));
	state->capacity = new_cap;
      }

      state->num_values = e.number + 1;
    }

    state->values[e.number] = e;
  }
}

void joystick_print_state(js_state state) {
  int ii;
  for(ii = 0; ii < state->num_values; ++ii) {
    js_event_print(state->values + ii);
  }
}

/*
int main(int argc, char** argv) {
  int ii;
  js_state state = joystick_open("/dev/input/js0");  

  joystick_print_state(state);

  joystick_close(state);

  return 1;
}
*/
