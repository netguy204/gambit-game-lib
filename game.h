#ifndef GAME_H
#define GAME_H

#include "platform.h"
#include "testlib.h"
#include "spriteatlas.h"

class CLeftAndRight : public Component {
 public:
  OBJECT_PROTO(CLeftAndRight);

  CLeftAndRight(void* go);

  virtual void update(float dt);

  float minx, maxx;
};

class CTimer : public Component {
 public:
  OBJECT_PROTO(CTimer);

  CTimer(void* go);

  virtual void update(float dt);

  Message* expire_message;
  float time_remaining;
};

class CTestDisplay : public Component {
 public:
  OBJECT_PROTO(CTestDisplay);

  CTestDisplay(void* go);

  virtual void update(float dt);

  float r, g, b;
};

class CStaticSprite : public Component {
 public:
  OBJECT_PROTO(CStaticSprite);

  CStaticSprite(void* go);

  virtual void update(float dt);

  Vector_ offset;
  SpriteAtlasEntry entry;
  int layer;
};

class CDrawHPatch : public Component {
 public:
  OBJECT_PROTO(CDrawHPatch);

  CDrawHPatch(void* go);

  virtual void update(float dt);

  Vector_ offset;
  SpriteAtlasEntry entry;
  float w;
  int layer;
};

class CDrawVPatch : public Component {
 public:
  OBJECT_PROTO(CDrawVPatch);

  CDrawVPatch(void* go);

  virtual void update(float dt);

  Vector_ offset;
  SpriteAtlasEntry entry;
  float h;
  int layer;
};

// probably want all renders to happen after this has done its update
class CCameraFocus : public Component {
 public:
  OBJECT_PROTO(CCameraFocus);

  CCameraFocus(void* go);

  virtual void update(float dt);

  GO* focus;
};

struct PEntry {
  Vector_ pos;
  Vector_ vel;
  float life;
};

class CParticleEmitter : public Component {
 public:
  OBJECT_PROTO(CParticleEmitter);

  CParticleEmitter(void* go);
  virtual ~CParticleEmitter();
  void init();

  virtual void update(float dt);
  void init_entry(PEntry* e, float life);

  Vector_ offset;
  SpriteAtlasEntry entry;
  int nmax;
  int active;
  int layer;
  float max_life;
  float max_speed;
  float max_offset;
  float grav_accel;
  float start_scale;
  float end_scale;
  PEntry* entries;
};

void game_init();
void game_shutdown();

GO* platform_make(float x, float y, float w, float h);
GO* slidingplatform_make(float x, float y, float w, float h, float speed,
                         float minx, float maxx);
GO* bomb_make(Vector pos, Vector vel);

#endif
