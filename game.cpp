#include "game.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"
#include "particle.h"
#include "rect.h"
#include "controls.h"
#include "steering.h"
#include "tiles.h"
#include "random.h"
#include "game_ui.h"
#include "color.h"

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

#define ATLAS "resources/images_default"

World* world;
struct Random_ rgen;

Clock main_clock;

void play_vorbis(const char* filename, float volume) {
  audio_enqueue(oggsampler_make(filename, audio_current_sample(), volume));
}

OBJECT_IMPL(CTimer, Component);
OBJECT_PROPERTY(CTimer, time_remaining);
OBJECT_PROPERTY(CTimer, kind);

CTimer::CTimer(void* go)
  : Component((GO*)go, PRIORITY_THINK), time_remaining(0), kind(MESSAGE_TIMER_EXPIRED) {
}

void CTimer::update(float dt) {
  this->time_remaining -= dt;

  if(this->time_remaining <= 0) {
    go->send_message(go->create_message(kind));
    delete_me = 1;
  }
}

OBJECT_IMPL(CLeftAndRight, Component);
OBJECT_PROPERTY(CLeftAndRight, minx);
OBJECT_PROPERTY(CLeftAndRight, maxx);

CLeftAndRight::CLeftAndRight(void* go)
  : Component((GO*)go, PRIORITY_ACT), minx(0), maxx(0) {
}

void CLeftAndRight::update(float dt) {
  Vector_ vel, pos;
  go->pos(&pos);
  go->vel(&vel);

  if(vel.x > 0) {
    if(pos.x > this->maxx) {
      pos.x = this->maxx;
      vel.x = -vel.x;
    }
  } else {
    if(pos.x < this->minx) {
      pos.x = this->minx;
      vel.x = -vel.x;
    }
  }
}

OBJECT_IMPL(CTestDisplay, Component);
OBJECT_PROPERTY(CTestDisplay, r);
OBJECT_PROPERTY(CTestDisplay, g);
OBJECT_PROPERTY(CTestDisplay, b);
OBJECT_PROPERTY(CTestDisplay, w);
OBJECT_PROPERTY(CTestDisplay, h);

CTestDisplay::CTestDisplay(void* go)
  : Component((GO*)go, PRIORITY_SHOW), r(1.0f), g(0.0f), b(1.0f), w(10), h(10) {
}

void CTestDisplay::update(float dt) {
  Vector_ pos;
  go->pos(&pos);

  struct ColoredRect_ rect;
  rect.minx = pos.x - w/2;
  rect.maxx = pos.x + w/2;
  rect.miny = pos.y - h/2;
  rect.maxy = pos.y + h/2;
  rect.color[0] = r;
  rect.color[1] = g;
  rect.color[2] = b;
  rect.color[3] = 1.0f;

  Vector_ cpos;
  camera()->pos(&cpos);

  float dy = floorf(cpos.y);
  rect.miny -= dy;
  rect.maxy -= dy;
  if(rect.miny > screen_height || rect.maxy < 0) return;

  float dx = floorf(cpos.x);
  rect.minx -= dx;
  rect.maxx -= dx;
  if(rect.minx > screen_width || rect.maxx < 0) return;

  filledrect_enqueue_for_screen(&rect);
}

OBJECT_IMPL(CStaticSprite, Component);
OBJECT_PROPERTY(CStaticSprite, entry);
OBJECT_PROPERTY(CStaticSprite, layer);
OBJECT_PROPERTY(CStaticSprite, offset);

CStaticSprite::CStaticSprite(void* go)
  : Component((GO*)go, PRIORITY_SHOW), entry(NULL), layer(LAYER_BACKGROUND) {
  vector_zero(&offset);
}

void CStaticSprite::update(float dt) {
  Vector_ pos;
  go->pos(&pos);
  vector_add(&pos, &pos, &offset);

  Sprite sprite = frame_make_sprite();
  sprite_fillfromentry(sprite, entry);
  sprite->displayX = pos.x;
  sprite->displayY = pos.y;
  sprite->originX = 0.5;
  sprite->originY = 0.5;

  scene()->addRelative(&scene()->layers[layer], sprite);
}

OBJECT_IMPL(CDrawHPatch, Component);
OBJECT_PROPERTY(CDrawHPatch, entry);
OBJECT_PROPERTY(CDrawHPatch, layer);
OBJECT_PROPERTY(CDrawHPatch, w);
OBJECT_PROPERTY(CDrawHPatch, offset);

CDrawHPatch::CDrawHPatch(void* go)
  : Component((GO*)go, PRIORITY_SHOW), entry(NULL), layer(LAYER_BACKGROUND) {
  vector_zero(&offset);
}

void CDrawHPatch::update(float dt) {
  Vector_ pos;
  go->pos(&pos);
  vector_add(&pos, &pos, &offset);

  Rect_ patch_rect;
  rect_centered(&patch_rect, &pos, w, entry->h);
  if(!rect_intersect(&scene()->camera_rect, &patch_rect)) return;

  int basex = pos.x;
  int basey = pos.y;
  int offset = -w / 2 + entry->w / 2;
  while(offset <= w / 2 - entry->w / 2) {
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, entry);
    sprite->displayX = basex + offset;
    sprite->displayY = basey;
    sprite->originX = 0.5;
    sprite->originY = 0.5;

    scene()->addRelative(&scene()->layers[layer], sprite);
    offset += entry->w;
  }
}


OBJECT_IMPL(CDrawVPatch, Component);
OBJECT_PROPERTY(CDrawVPatch, entry);
OBJECT_PROPERTY(CDrawVPatch, layer);
OBJECT_PROPERTY(CDrawVPatch, h);
OBJECT_PROPERTY(CDrawVPatch, offset);

CDrawVPatch::CDrawVPatch(void* go)
  : Component((GO*)go, PRIORITY_SHOW), entry(NULL), layer(LAYER_BACKGROUND) {
  vector_zero(&offset);
}

void CDrawVPatch::update(float dt) {
  Vector_ pos;
  go->pos(&pos);
  vector_add(&pos, &pos, &offset);

  Rect_ patch_rect;
  rect_centered(&patch_rect, &pos, entry->w, h);
  if(!rect_intersect(&scene()->camera_rect, &patch_rect)) return;

  int basex = pos.x;
  int basey = pos.y;
  int offset = -h / 2 + entry->h / 2;
  while(offset <= h / 2 - entry->h / 2) {
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, entry);
    sprite->displayX = basex;
    sprite->displayY = basey + offset;
    sprite->originX = 0.5;
    sprite->originY = 0.5;

    scene()->addRelative(&scene()->layers[layer], sprite);
    offset += entry->h;
  }
}


OBJECT_IMPL(CCameraFocus, Component);
OBJECT_PROPERTY(CCameraFocus, focus);

CCameraFocus::CCameraFocus(void* go)
  : Component((GO*)go, PRIORITY_THINK), focus(NULL)  {
}

void CCameraFocus::update(float dt) {
  if(!focus) return;
  Vector_ offset = {screen_width / 2.0f, screen_height / 2.0f};
  Vector_ desired;
  focus->pos(&desired);
  vector_sub(&desired, &desired, &offset);

  float max_v = 1600;
  const float max_dx = max_v * dt;

  Vector_ cpos;
  go->pos(&cpos);

  Vector_ delta;
  vector_sub(&delta, &desired, &cpos);
  float mag = vector_mag(&delta);
  if(mag < max_dx) {
    // snap
    go->set_pos(&desired);
    return;
  }

  vector_scale(&delta, &delta, max_dx / mag);
  vector_add(&cpos, &cpos, &delta);
  go->set_pos(&cpos);
}

OBJECT_IMPL(CParticleEmitter, Component);
OBJECT_PROPERTY(CParticleEmitter, entry);
OBJECT_PROPERTY(CParticleEmitter, active);
OBJECT_PROPERTY(CParticleEmitter, max_life);
OBJECT_PROPERTY(CParticleEmitter, max_speed);
OBJECT_PROPERTY(CParticleEmitter, max_offset);
OBJECT_PROPERTY(CParticleEmitter, grav_accel);
OBJECT_PROPERTY(CParticleEmitter, start_scale);
OBJECT_PROPERTY(CParticleEmitter, end_scale);
OBJECT_PROPERTY(CParticleEmitter, nmax);
OBJECT_PROPERTY(CParticleEmitter, offset);
OBJECT_PROPERTY(CParticleEmitter, layer);

void vector_random(Vector v, float scale) {
  float mag = 0;
  while(mag < 0.0001) {
    v->x = random_next_gaussian(&rgen);
    v->y = random_next_gaussian(&rgen);
    mag = vector_dot(v, v);
  }

  mag = sqrtf(mag);
  vector_scale(v, v, scale / mag);
}

CParticleEmitter::CParticleEmitter(void* go)
  : Component((GO*)go, PRIORITY_ACT), entry(NULL), nmax(0), entries(NULL),
    max_life(1), max_speed(100), max_offset(3), active(1), grav_accel(0),
    start_scale(1), end_scale(1), layer(LAYER_BACKGROUND) {
  vector_zero(&offset);
}

void CParticleEmitter::init() {
  if(entries) delete[] entries;

  entries = new PEntry[nmax];

  float dlife = max_life / nmax;

  for(int ii = 0; ii < nmax; ++ii) {
    PEntry* e = &entries[ii];
    init_entry(e, dlife * ii);
  }
}

void CParticleEmitter::init_entry(PEntry* e, float life) {
  Vector_ wpos;
  go->pos(&wpos);
  vector_add(&wpos, &wpos, &offset);
  vector_random(&e->vel, max_speed);
  vector_random(&e->pos, max_offset);
  vector_add(&e->pos, &e->pos, &wpos);
  e->life = life;
}

CParticleEmitter::~CParticleEmitter() {
  delete[] entries;
}

void CParticleEmitter::update(float dt) {
  const float max_temp = 6500;
  const float min_temp = 2000;
  const float temp_slope = (max_temp - min_temp) / max_life;
  const float scale_slope = (start_scale - end_scale) / max_life;

  for(int ii = 0; ii < nmax; ++ii) {
    PEntry* e = &entries[ii];
    if(active && e->life <= 0) {
      // re-init dead particles
      init_entry(e, max_life);
    } else if(!active) {
      // skip dead particles
      continue;
    }

    // integrate
    e->vel.y -= grav_accel * dt;
    vector_integrate(&e->pos, &e->pos, &e->vel, dt);
    e->life -= dt;

    const float scale = end_scale + e->life * scale_slope;

    // draw
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, entry);
    sprite->displayX = e->pos.x;
    sprite->displayY = e->pos.y;
    sprite->originX = 0.5;
    sprite->originY = 0.5;
    sprite->w *= scale;
    sprite->h *= scale;
    sprite->angle = vector_angle(&e->vel);

    const float temp = min_temp + e->life * temp_slope;
    color_for_temp(temp, sprite->color);
    sprite->color[3] = e->life / max_life;

    scene()->addRelative(&scene()->particles[layer], sprite);
  }
}

void game_step(long delta, InputState state);

int world_reset_requested = 0;
int reset_world(lua_State* L);

void init_world() {
  World* old_world = world;

  world = new World();

  lua_register(world->L, "reset_world", reset_world);
  world->load_level("resources/level1.lua");

  if(old_world) delete old_world;
  world_reset_requested = 0;
}

int reset_world(lua_State* L) {
  world_reset_requested = 1;
  return 0;
}

void game_support_init() {
  color_init();
  random_init(&rgen, 1234);
  main_clock = clock_make();
}

void game_init() {
  game_support_init();

  //play_vorbis("sounds/DST-2ndBallad.ogg", 0.7);
  //audio_enqueue(sinsampler_make(audio_current_sample(), SAMPLE_FREQ * 10, C_(1), 8000, 0.0));

  init_world();

  set_game_step(game_step);
}

const float enemy_period = 1;
float enemy_timer = 1;

typedef enum {
  STATE_START,
  STATE_PLAY,
  STATE_WIN,
  STATE_LOSE
} WinState;

void render_hud() {
  float patch_width = floorf(screen_width / 128) * 128;
  float patch_height = 128;

  Rect_ patch = {
    (screen_width - patch_width) / 2.0f,
    -32.0f,
    (screen_width + patch_width) / 2.0f,
    patch_height - 32.0f};
  SpriteList list = spritelist_from_8patch(NULL, world->atlas(ATLAS), &patch);
  spritelist_enqueue_for_screen(list);
}

void game_step(long delta, InputState state) {
  if(world_reset_requested) {
    init_world();
  }

  world->input_state = state;

  float dt = clock_update(main_clock, delta / 1000.0);

  world->update(dt);
  world->scene.enqueue();

  //render_hud();
}

void game_shutdown() {
}

void print_lstack() {
  lua_State* L = world->L;
  for(int ii = 0; ii < lua_gettop(L); ++ii) {
    printf("pos %d type %d\n", ii, lua_type(L, ii));
  }
}
