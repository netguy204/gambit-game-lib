#ifndef GAME_H
#define GAME_H

#include "platform.h"
#include "testlib.h"

typedef struct CInput_ {
  struct Component_ _;
  InputState state;
  int fire_pressed;
  int facing;
} *CInput;

const void* CInputObject();

typedef struct CLeftAndRight_ {
  struct Component_ _;
  float minx, maxx;
} *CLeftAndRight;

const void* CLeftAndRightObject();

typedef struct CTimer_ {
  struct Component_ _;
  void* expire_payload;
  float time_remaining;
} *CTimer;

const void* CTimerObject();

typedef struct CBombBehavior_ {
  struct Component_ _;
  int state;
} *CBombBehavior;

const void* CBombBehaviorObject();

const void* CTestDisplayObject();

void game_init();
void game_shutdown();

GO platform_make(float x, float y, float w, float h);
GO slidingplatform_make(float x, float y, float w, float h, float speed,
                        float minx, float maxx);
GO bomb_make(Vector pos, Vector vel);

#endif
