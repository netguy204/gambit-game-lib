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
#include "utils.h"

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

#define ATLAS "resources/images_default"

World* world;
struct Random_ rgen;

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
OBJECT_PROPERTY(CTestDisplay, a);
OBJECT_PROPERTY(CTestDisplay, w);
OBJECT_PROPERTY(CTestDisplay, h);
OBJECT_PROPERTY(CTestDisplay, layer);

CTestDisplay::CTestDisplay(void* go)
  : Component((GO*)go, PRIORITY_SHOW), r(1.0f), g(0.0f), b(1.0f), a(1.0f),
    w(10), h(10), layer(LAYER_PLAYER) {
}

void CTestDisplay::update(float dt) {
  Vector_ pos;
  go->pos(&pos);

  ColoredRect rect = (ColoredRect)frame_alloc(sizeof(ColoredRect_));
  rect->minx = pos.x - w/2;
  rect->maxx = pos.x + w/2;
  rect->miny = pos.y - h/2;
  rect->maxy = pos.y + h/2;
  rect->color[0] = r;
  rect->color[1] = g;
  rect->color[2] = b;
  rect->color[3] = a;

  scene()->addRelative(&scene()->testRects[layer], rect);
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

OBJECT_IMPL(CDrawWallpaper, Component);
OBJECT_PROPERTY(CDrawWallpaper, entry);
OBJECT_PROPERTY(CDrawWallpaper, offset);
OBJECT_PROPERTY(CDrawWallpaper, w);
OBJECT_PROPERTY(CDrawWallpaper, h);
OBJECT_PROPERTY(CDrawWallpaper, style);
OBJECT_PROPERTY(CDrawWallpaper, layer);

CDrawWallpaper::CDrawWallpaper(void* _go)
  : Component((GO*)_go, PRIORITY_SHOW), style(WALLPAPER_TILE), layer(LAYER_BACKDROP) {
  vector_zero(&offset);
}

void CDrawWallpaper::update(float dt) {
  Vector_ pos, cpos;
  go->pos(&pos);
  camera()->pos(&cpos);

  vector_add(&pos, &pos, &offset);

  float x_bl = (pos.x - w/2) - floorf(cpos.x);
  if(x_bl > screen_width) return;

  float y_bl = (pos.y - h/2) - floorf(cpos.y);
  if(y_bl > screen_height) return;

  float x_tr = (pos.x + w/2) - floorf(cpos.x);
  if(x_tr < 0) return;

  float y_tr = (pos.y + h/2) - floorf(cpos.y);
  if(y_tr < 0) return;

  // now we have some overlap, clamp the tr to the screen and figure
  // out the bl offset
  x_tr = MIN(x_tr, screen_width);
  y_tr = MIN(y_tr, screen_height);

  if(x_bl < 0) {
    x_bl += floorf(fabs(x_bl) / entry->w) * entry->w;
  }

  if(y_bl < 0) {
    y_bl += floorf(fabs(y_bl) / entry->h) * entry->h;
  }

  if(style != WALLPAPER_TILE) {
    fail_exit("i haven't bothered to implement the other wallpaper styles yet\n");
  }

  float y = y_bl;
  while(y < y_tr) {
    float x = x_bl;
    while(x < x_tr) {
      BaseSprite sprite = (BaseSprite)frame_alloc(sizeof(BaseSprite_));
      sprite_fillfromentry(sprite, entry);
      sprite->displayX = x;
      sprite->displayY = y;
      scene()->addAbsolute(&scene()->baseLayers[layer], sprite);
      x += entry->w;
    }
    y += entry->h;
  }
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
OBJECT_PROPERTY(CParticleEmitter, coloring);
OBJECT_PROPERTY(CParticleEmitter, start_color);
OBJECT_PROPERTY(CParticleEmitter, end_color);
OBJECT_PROPERTY(CParticleEmitter, start_alpha);
OBJECT_PROPERTY(CParticleEmitter, end_alpha);
OBJECT_PROPERTY(CParticleEmitter, max_angular_speed);

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
    start_scale(1), end_scale(1), layer(LAYER_BACKGROUND), start_alpha(1),
    end_alpha(0), start_color(6500), end_color(2000), coloring(COLORING_BLACKBODY),
    max_angular_speed(0) {
  vector_zero(&offset);
}

void CParticleEmitter::init() {
  if(entries) delete[] entries;

  entries = new PEntry[nmax];

  float dlife = max_life / nmax;

  for(int ii = 0; ii < nmax; ++ii) {
    PEntry* e = &entries[ii];
    init_entry(e, dlife * ii);
    // off in the nether so it can burn out
    e->pos.x = -10000;
    e->pos.y = -10000;
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
  if(max_angular_speed > 0) {
    e->angle = random_next_gaussian(&rgen) * 2 * M_PI;
    e->dangle = random_next_gaussian(&rgen) * max_angular_speed;
  } else {
    e->angle = 0;
    e->dangle = 0;
  }
}

CParticleEmitter::~CParticleEmitter() {
  delete[] entries;
}

void CParticleEmitter::update(float dt) {
  const float color_slope = (start_color - end_color) / max_life;
  const float scale_slope = (start_scale - end_scale) / max_life;
  const float alpha_slope = (start_alpha - end_alpha) / max_life;

  for(int ii = 0; ii < nmax; ++ii) {
    PEntry* e = &entries[ii];
    if(e->life <= 0) {
      if(active) {
        // re-init dead particles
        init_entry(e, max_life);
      } else {
        // skip dead particles
        continue;
      }
    }

    // integrate
    e->vel.y -= grav_accel * dt;
    e->angle += e->dangle * dt;

    vector_integrate(&e->pos, &e->pos, &e->vel, dt);
    e->life -= dt;

    const float scale = end_scale + e->life * scale_slope;
    const float color = end_color + e->life * color_slope;
    const float alpha = end_alpha + e->life * alpha_slope;

    // draw
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, entry);
    sprite->displayX = e->pos.x;
    sprite->displayY = e->pos.y;
    sprite->originX = 0.5;
    sprite->originY = 0.5;
    sprite->w *= scale;
    sprite->h *= scale;
    if(max_angular_speed > 0) {
      sprite->angle = e->angle;
    } else {
      sprite->angle = vector_angle(&e->vel);
    }

    if(coloring == COLORING_BLACKBODY) {
      color_for_temp(color, sprite->color);
    } else {
      sprite->color[0] = color;
      sprite->color[1] = color;
      sprite->color[2] = color;
    }

    sprite->color[3] = alpha;

    scene()->addRelative(&scene()->particles[layer], sprite);
  }
}

void game_step(long delta, InputState state);

int world_reset_requested = 0;

int Lreset_world(lua_State* L) {
  world_reset_requested = 1;
  return 0;
}

int Lrandom_gaussian(lua_State* L) {
  lua_pushnumber(L, random_next_gaussian(&rgen));
  return 1;
}

void init_world() {
  World* old_world = world;

  world = new World();

  lua_register(world->L, "reset_world", Lreset_world);
  lua_register(world->L, "random_gaussian", Lrandom_gaussian);

  world->load_level("resources/level1.lua");

  if(old_world) delete old_world;
  world_reset_requested = 0;
}

void game_support_init() {
  color_init();
  random_init(&rgen, 1234);
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
  world->update(delta);
  world->scene.enqueue();

  //render_hud();
  // get a clean exit for profiling
  /*
  if(clock_cycles_to_seconds(clock_time(world->clock)) > 120) {
    exit(0);
  }
  */
}

void game_shutdown() {
}

void print_lstack() {
  lua_State* L = world->L;
  for(int ii = 0; ii < lua_gettop(L); ++ii) {
    printf("pos %d type %d\n", ii, lua_type(L, ii));
  }
}
