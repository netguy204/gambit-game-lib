#ifndef JOYSTICK_H
#define JOYSTICK_H

#include <asm/types.h>
#include "testlib.h"

#define JS_EVENT_BUTTON         0x01    /* button pressed/released */
#define JS_EVENT_AXIS           0x02    /* joystick moved */
#define JS_EVENT_INIT           0x80    /* initial state of device */

struct js_event {
  __u32 time;     /* event timestamp in milliseconds */
  __s16 value;    /* value */
  __u8 type;      /* event type */
  __u8 number;    /* axis/button number */
};

typedef struct js_state_ {
  int fileno;
  unsigned int num_values;
  unsigned int capacity;
  struct js_event* values;
} *js_state;

js_state joystick_open(const char* device);
void joystick_close(js_state state);
void joystick_update_state(js_state state);
void joystick_print_state(js_state state);

#endif
