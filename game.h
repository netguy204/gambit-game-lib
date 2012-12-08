#ifndef GAME_H
#define GAME_H

#include "particle.h"

typedef struct PlayerState_ {
  struct Particle_ particle;
  struct Vector_ fire_charge;
  int jumping;
  int charging;
} *PlayerState;

void game_init();
void game_shutdown();

#endif
