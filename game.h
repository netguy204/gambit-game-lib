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

  int kind;
  float time_remaining;
};

class CTestDisplay : public Component {
 public:
  OBJECT_PROTO(CTestDisplay);

  CTestDisplay(void* go);

  virtual void update(float dt);

  Vector_ offset;
  float r, g, b, a, w, h;
  int layer;
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

class CSpriterSprite : public Component {
 public:
  OBJECT_PROTO(CSpriterSprite);

  CSpriterSprite(void* go);

  virtual void update(float dt);

  Animation* animation;
  Vector_ offset;
  int layer;
  float current_time;
  float time_scale;
};

enum WallpaperStyle {
  WALLPAPER_SCALE,
  WALLPAPER_TILE,
};

class CDrawWallpaper : public Component {
 public:
  OBJECT_PROTO(CDrawWallpaper);

  CDrawWallpaper(void* go);

  virtual void update(float dt);

  SpriteAtlasEntry entry;
  Vector_ offset;
  float w, h;
  int style;
  int layer;
};

#include "tiles.h"

class CDrawTilemap : public Component {
 public:
  OBJECT_PROTO(CDrawTilemap);

  CDrawTilemap(void* go);
  virtual ~CDrawTilemap();

  virtual void update(float dt);

  void set_map(TileMap map);
  TileMap get_map();

  Vector_ offset;
  TileMap map;
  int w, h;
  int layer;
};

struct PEntry {
  Vector_ pos;
  Vector_ vel;
  float life;
  float angle;
  float dangle;
};

enum EmitterColoring {
  COLORING_BLACKBODY,
  COLORING_BW
};

class CParticleEmitter : public Component {
 public:
  OBJECT_PROTO(CParticleEmitter);

  CParticleEmitter(void* go);
  virtual ~CParticleEmitter();
  void init();

  virtual void update(float dt);
  void init_entry(PEntry* e, float life);

  SpriteAtlasEntry entry;
  Vector_ offset;
  int nmax;
  int active;
  int layer;
  int coloring;
  float max_life;
  float max_speed;
  float max_angular_speed;
  float max_offset;
  float grav_accel;
  float start_scale;
  float end_scale;
  float start_color;
  float end_color;
  float start_alpha;
  float end_alpha;
  PEntry* entries;
};

void game_init();
void game_shutdown();

GO* platform_make(float x, float y, float w, float h);
GO* slidingplatform_make(float x, float y, float w, float h, float speed,
                         float minx, float maxx);
GO* bomb_make(Vector pos, Vector vel);

#endif
