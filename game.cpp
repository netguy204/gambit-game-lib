#include "game.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"
#include "particle.h"
#include "rect.h"
#include "controls.h"
#include "agent.h"
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

float bomb_delay = 6.0f;
float player_speed = 600;
float player_jump_speed = 1200;
float player_width = 28;
float player_height = 64;
float player_jump_height = 300;
float bomb_max_height = 400;
float bomb_dim = 48;
float player_gravity_accel;
float bomb_gravity_accel;
float throw_speed = 1200;
float ground_level = 100;
float charge_delay = 0.2;
float bomb_explode_start = 0.3;
float bomb_chain_factor = 2.0;
float enemy_speed = 100;
float enemy_dim = 36;

World* world;
struct Random_ rgen;

GO* player_go;
GO* camera;

TileMap background;
Clock main_clock;

int max_bombs = 50;
int current_n_bombs = 0;

int max_enemies = 20;
int current_n_enemies = 0;

void camera_relative_enqueue(ColoredRect rect) {
  float dy = floorf(camera->_pos.y);
  rect->miny -= dy;
  rect->maxy -= dy;
  if(rect->miny > screen_height || rect->maxy < 0) return;

  float dx = floorf(camera->_pos.x);
  rect->minx -= dx;
  rect->maxx -= dx;
  if(rect->minx > screen_width || rect->maxx < 0) return;

  filledrect_enqueue_for_screen(rect);
}

void play_vorbis(const char* filename, float volume) {
  audio_enqueue(oggsampler_make(filename, audio_current_sample(), volume));
}

// specialize PropertyTypeImpl for SpriteAtlasEntry
template<>
void PropertyTypeImpl<SpriteAtlasEntry>::LCpush_value(const PropertyInfo* info, Object* obj, lua_State* L) {
  SpriteAtlasEntry entry;
  get_value(info, obj, &entry);
  lua_pushlightuserdata(L, entry);
}

template<>
void PropertyTypeImpl<SpriteAtlasEntry>::LCset_value(const PropertyInfo* info, Object* obj, lua_State* L, int pos) {
  if(!lua_islightuserdata(L, pos)) {
    luaL_error(L, "position %d does not contain lightuserdata", pos);
  }

  SpriteAtlasEntry entry = (SpriteAtlasEntry)lua_touserdata(L, pos);
  set_value(info, obj, &entry);
}

OBJECT_IMPL(CTimer, Component);
OBJECT_PROPERTY(CTimer, time_remaining);
OBJECT_PROPERTY(CTimer, expire_message);

CTimer::CTimer(void* go)
  : Component((GO*)go, PRIORITY_THINK), time_remaining(0), expire_message(NULL) {
}

void CTimer::update(float dt) {
  this->time_remaining -= dt;

  if(this->time_remaining <= 0) {
    go->send_message(expire_message);
    delete_me = 1;
  }
}

enum BombStates {
  BOMB_IDLE,
  BOMB_EXPLODING,
  BOMB_DONE
};

OBJECT_IMPL(CBombBehavior, Component);

CBombBehavior::CBombBehavior(void* go)
  : Component((GO*)go, PRIORITY_ACT), state(BOMB_IDLE) {
  CTimer* timer = this->go->add_c<CTimer>();
  timer->time_remaining = bomb_delay - bomb_explode_start;
  timer->expire_message = this->go->create_message(MESSAGE_TIMER_EXPIRED);
  this->state = BOMB_IDLE;
}

void CBombBehavior::update(float dt) {
  GO* bomb = this->go;

  // if we're supported then zero our x so that we're sticky
  if(bomb->transform_parent) {
    bomb->_vel.x = 0;
  }

  // nothing to do if a timer hasn't gone off
  int timer_expired = 0;
  bomb->inbox.foreach([&](Message* message) -> int {
      // look for an expired timer message or an explosion
      if(message->kind == MESSAGE_TIMER_EXPIRED
         || message->kind == MESSAGE_EXPLOSION_NEARBY) {
        timer_expired = 1;
        return 1;
      }
      return 0;
    });

  if(!timer_expired) return;

  if(this->state == BOMB_IDLE) {
    // set the next timer and change state
    CTimer* timer = bomb->add_c<CTimer>();
    timer->time_remaining = bomb_explode_start;
    timer->expire_message = bomb->create_message(MESSAGE_TIMER_EXPIRED);

    this->state = BOMB_EXPLODING;

    // add explosion particle emitter
    CParticleEmitter* cp = go->add_c<CParticleEmitter>();
    cp->entry = go->world->atlas_entry(ATLAS, "expl1");
    cp->offset.x = 0;
    cp->offset.y = 0;
    cp->nmax = 10;
    cp->start_scale = 0.7;
    cp->max_life = bomb_explode_start;
    cp->grav_accel = -30;
    cp->init();

    // stop displaying the sprite
    CStaticSprite* spr = (CStaticSprite*)go->find_component(&CStaticSprite::Type);
    spr->delete_me = 1;

    // play the sound
    play_vorbis("sounds/Explosion6.ogg", 0.5);
  } else if(this->state == BOMB_EXPLODING) {
    // destroy the bomb
    this->state = BOMB_DONE;
    agent_send_terminate(bomb, bomb->world);

    // notify our neighbors
    Vector_ pos;
    bomb->pos(&pos);
    world_foreach(bomb->world, &pos, bomb_dim * bomb_chain_factor, [&](GO* item) -> int {
        item->send_message(bomb->create_message(MESSAGE_EXPLOSION_NEARBY));
        return 0;
      });
  }
}

OBJECT_IMPL(CLeftAndRight, Component);
OBJECT_PROPERTY(CLeftAndRight, minx);
OBJECT_PROPERTY(CLeftAndRight, maxx);

CLeftAndRight::CLeftAndRight(void* go)
  : Component((GO*)go, PRIORITY_ACT), minx(0), maxx(0) {
}

void CLeftAndRight::update(float dt) {
  if(go->_vel.x > 0) {
    if(go->_pos.x > this->maxx) {
      go->_pos.x = this->maxx;
      go->_vel.x = -go->_vel.x;
    }
  } else {
    if(go->_pos.x < this->minx) {
      go->_pos.x = this->minx;
      go->_vel.x = -go->_vel.x;
    }
  }
}

OBJECT_IMPL(CTestDisplay, Component);
OBJECT_PROPERTY(CTestDisplay, r);
OBJECT_PROPERTY(CTestDisplay, g);
OBJECT_PROPERTY(CTestDisplay, b);

CTestDisplay::CTestDisplay(void* go)
  : Component((GO*)go, PRIORITY_SHOW), r(1.0f), g(0.0f), b(1.0f) {
}

void CTestDisplay::update(float dt) {
  CCollidable* coll = (CCollidable*)go->find_component(&CCollidable::Type);

  assert(coll);

  struct ColoredRect_ rect;
  coll->rect(&rect);

  rect.color[0] = r;
  rect.color[1] = g;
  rect.color[2] = b;
  rect.color[3] = 1.0f;

  camera_relative_enqueue(&rect);
}

OBJECT_IMPL(CStaticSprite, Component);
OBJECT_PROPERTY(CStaticSprite, entry);

SpriteList CStaticSprite::list = NULL;

CStaticSprite::CStaticSprite(void* go)
  : Component((GO*)go, PRIORITY_SHOW), entry(NULL) {
}

void CStaticSprite::update(float dt) {
  Vector_ pos;
  go->pos(&pos);

  float dx = floorf(camera->_pos.x);
  float dy = floorf(camera->_pos.y);
  float px = pos.x - dx;
  float py = pos.y - dy;

  if(px + entry->w < 0 || px - entry->w > screen_width) return;
  if(py + entry->h < 0 || py - entry->h > screen_height) return;

  Sprite sprite = frame_make_sprite();
  sprite_fillfromentry(sprite, entry);
  sprite->displayX = px;
  sprite->displayY = py;
  sprite->originX = 0.5;
  sprite->originY = 0.5;

  CStaticSprite::list = frame_spritelist_append(CStaticSprite::list, sprite);
}

OBJECT_IMPL(CDrawPatch, Component);
OBJECT_PROPERTY(CDrawPatch, entry);

SpriteList CDrawPatch::list = NULL;

CDrawPatch::CDrawPatch(void* go)
  : Component((GO*)go, PRIORITY_SHOW), entry(NULL) {
}

void CDrawPatch::update(float dt) {
  CCollidable* coll = (CCollidable*)go->find_component(&CCollidable::Type);
  assert(coll);

  Vector_ pos;
  go->pos(&pos);

  float dx = floorf(camera->_pos.x);
  float dy = floorf(camera->_pos.y);
  int basex = pos.x - dx;
  int basey = pos.y - dy;
  int offset = -coll->w / 2 + entry->w / 2;
  while(offset < coll->w / 2 - entry->w / 2) {
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, entry);
    sprite->displayX = basex + offset;
    sprite->displayY = basey;
    sprite->originX = 0.5;
    sprite->originY = 0.5;

    CDrawPatch::list = frame_spritelist_append(CDrawPatch::list, sprite);
    offset += entry->w;
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

  Vector_ delta;
  vector_sub(&delta, &desired, &go->_pos);
  float mag = vector_mag(&delta);
  if(mag < max_dx) {
    // snap
    go->_pos = desired;
    return;
  }

  vector_scale(&delta, &delta, max_dx / mag);
  vector_add(&go->_pos, &go->_pos, &delta);
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

SpriteList CParticleEmitter::list = NULL;

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
    start_scale(1), end_scale(1) {
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

  float dx = floorf(camera->_pos.x);
  float dy = floorf(camera->_pos.y);

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
    sprite->displayX = e->pos.x - dx;
    sprite->displayY = e->pos.y - dy;
    sprite->originX = 0.5;
    sprite->originY = 0.5;
    sprite->w *= scale;
    sprite->h *= scale;
    sprite->angle = vector_angle(&e->vel);

    const float temp = min_temp + e->life * temp_slope;
    color_for_temp(temp, sprite->color);
    sprite->color[3] = e->life / max_life;

    CParticleEmitter::list = frame_spritelist_append(CParticleEmitter::list, sprite);
  }
}

GO* platform_make(float x, float y, float w, float h) {
  GO* go = world->create_go();
  go->_pos.x = x;
  go->_pos.y = y;

  CDrawPatch* patch = go->add_c<CDrawPatch>();
  patch->entry = world->atlas_entry(ATLAS, "platform2");

  CCollidable* coll = go->add_c<CCollidable>();
  coll->w = w;
  coll->h = h;
  return go;
}

GO* slidingplatform_make(float x, float y, float w, float h, float speed,
                         float minx, float maxx) {
  GO* go = platform_make(x, y, w, h);
  go->_vel.x = speed;

  CLeftAndRight* lnr = go->add_c<CLeftAndRight>();
  lnr->minx = minx;
  lnr->maxx = maxx;
  return go;
}

GO* bomb_make(Vector pos, Vector vel) {
  GO* go = world->create_go();

  go->_pos = *pos;
  go->_vel = *vel;

  CStaticSprite* ss = go->add_c<CStaticSprite>();
  ss->entry = world->atlas_entry(ATLAS, "bomb");

  CCollidable* coll = go->add_c<CCollidable>();
  coll->w = bomb_dim;
  coll->h = bomb_dim;

  CPlatformer* plat = go->add_c<CPlatformer>();
  plat->grav_accel = bomb_gravity_accel;

  go->add_c<CBombBehavior>();

  // sparks
  Vector_ offset = {0.0f, 16.0f};
  CParticleEmitter* pe = go->add_c<CParticleEmitter>();
  pe->entry = world->atlas_entry(ATLAS, "spark");
  pe->offset.x = 0.0f;
  pe->offset.y = 16.0f;
  pe->nmax = 10;
  pe->grav_accel = 80;
  pe->start_scale = 0.0;
  pe->init();

  return go;
}

void game_step(long delta, InputState state);

void game_support_init() {
  color_init();
  random_init(&rgen, 1234);

  main_clock = clock_make();

  player_gravity_accel = (player_jump_speed * player_jump_speed) / (2 * player_jump_height);
  bomb_gravity_accel = (throw_speed * throw_speed) / (2 * bomb_max_height);

  world = new World();
  player_go = world->player;
  camera = world->camera;
}

void game_init() {
  game_support_init();

  // background tilemap
  const int tdim = 64;
  const int world_width = screen_width * 3;
  const int world_height = screen_height * 6;
  background = tilemap_make(world_width / tdim, world_height / tdim, tdim, tdim);
  memset(background->tiles, 0, tilemap_size(background));
  TileSpec spec = (TileSpec)malloc(sizeof(struct TileSpec_));
  background->tile_specs = spec;
  spec[0].image = world->atlas_entry(ATLAS, "back1");
  spec[0].bitmask = TILESPEC_VISIBLE;

  world->load_level("resources/level1.lua");

  play_vorbis("sounds/DST-2ndBallad.ogg", 0.7);
  //audio_enqueue(sinsampler_make(audio_current_sample(), SAMPLE_FREQ * 10, C_(1), 8000, 0.0));

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
  world->input_state = state;

  float dt = clock_update(main_clock, delta / 1000.0);

  SpriteList bg = tilemap_spritelist(background, camera->_pos.x + screen_width,
                                     camera->_pos.y + screen_height,
                                     screen_width, screen_height);
  spritelist_enqueue_for_screen(bg);

  world_notify_collisions(world);
  world->update(dt);

  spritelist_enqueue_for_screen(CStaticSprite::list);
  CStaticSprite::list = NULL;

  spritelist_enqueue_for_screen(CDrawPatch::list);
  CDrawPatch::list = NULL;

  // render all particles
  spritelist_enqueue_for_screen_colored(CParticleEmitter::list);
  CParticleEmitter::list = NULL;

  render_hud();
}

void game_shutdown() {
}

void print_lstack() {
  lua_State* L = world->L;
  for(int ii = 0; ii < lua_gettop(L); ++ii) {
    printf("pos %d type %d\n", ii, lua_type(L, ii));
  }
}
