#ifndef GAME_H
#define GAME_H

#include "particle.h"

typedef struct PlayerState_ {
  struct Particle_ particle;
  struct Vector_ fire_charge;
  float fire_timeout;
  int jumping;
  int fire_pressed;
  int charging;
} *PlayerState;

void game_init();
void game_shutdown();

#endif
