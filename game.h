#ifndef GAME_H
#define GAME_H

#include "particle.h"

typedef struct Platform_ {
  struct Particle_ particle;
  struct ColoredRect_ rect;
} *Platform;

typedef struct Platformer_ {
  struct Particle_ particle;
  Platform parent;
  float w;
  float h;
  int falling;
} *Platformer;

typedef struct PlayerState_ {
  struct Platformer_ platformer;
  struct Vector_ fire_charge;
  float fire_timeout;
  int fire_pressed;
  int charging;
} *PlayerState;

typedef struct Bomb_ {
  struct Platformer_ _;
  float time_remaining;
} *Bomb;


void game_init();
void game_shutdown();

#endif
