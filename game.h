#ifndef GAME_H
#define GAME_H

#include "particle.h"

typedef struct Platform_ {
  struct Particle_ particle;
  struct ColoredRect_ rect;
} *Platform;

typedef struct PlayerState_ {
  struct Particle_ particle;
  struct Vector_ fire_charge;
  Platform parent;
  float fire_timeout;
  int jumping;
  int fire_pressed;
  int charging;
} *PlayerState;


void game_init();
void game_shutdown();

#endif
