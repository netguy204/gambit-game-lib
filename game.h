#ifndef GAME_H
#define GAME_H

#include "platform.h"

typedef struct PlayerState_ {
  struct Platformer_ platformer;
  struct Vector_ fire_charge;
  float fire_timeout;
  int fire_pressed;
  int charging;
  int facing;
} *PlayerState;

typedef struct Bomb_ {
  struct Platformer_ _;
  float time_remaining;
  int searched_neighbors;
} *Bomb;

typedef struct Enemy_ {
  struct Platformer_ _;
  struct Platform_ platform;
} *Enemy;

typedef struct Boss_ {
  struct Platform_ _;
  int hp;
} *Boss;

void game_init();
void game_shutdown();

#endif
