#ifndef GAME_H
#define GAME_H

#include "platform.h"
#include "testlib.h"

class CInput : public Component {
 public:
  OBJECT_PROTO(CInput);

  CInput();
  CInput(GO* go);

  virtual void update(float dt);

  InputState state;
  int fire_pressed;
  int facing;
};

class CLeftAndRight : public Component {
 public:
  OBJECT_PROTO(CLeftAndRight);

  CLeftAndRight();
  CLeftAndRight(GO* go, float minx, float maxx);

  virtual void update(float dt);

  float minx, maxx;
};

class CTimer : public Component {
 public:
  OBJECT_PROTO(CTimer);

  CTimer();
  CTimer(GO* go, float time_remaining, void* payload);

  virtual void update(float dt);

  void* expire_payload;
  float time_remaining;
};

class CBombBehavior : public Component {
 public:
  OBJECT_PROTO(CBombBehavior);

  CBombBehavior();
  CBombBehavior(GO* go);

  virtual void update(float dt);

  int state;
};

class CEnemyBehavior : public Component {
 public:
  OBJECT_PROTO(CEnemyBehavior);

  CEnemyBehavior();
  CEnemyBehavior(GO* go);

  virtual void update(float dt);

  int state;
};

class CTestDisplay : public Component {
 public:
  OBJECT_PROTO(CTestDisplay);

  CTestDisplay();
  CTestDisplay(GO* go, float r, float g, float b);

  virtual void update(float dt);

  float r, g, b;
};

// probably want all renders to happen after this has done its update
class CCameraFocus : public Component {
 public:
  OBJECT_PROTO(CCameraFocus);

  CCameraFocus();
  CCameraFocus(GO* go, GO* camera);

  virtual void update(float dt);

  GO* camera;
};

void game_init();
void game_shutdown();

GO* platform_make(float x, float y, float w, float h);
GO* slidingplatform_make(float x, float y, float w, float h, float speed,
                         float minx, float maxx);
GO* bomb_make(Vector pos, Vector vel);

#endif
