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

CInput* player_input;
TileMap background;
Clock main_clock;

int max_bombs = 50;
int current_n_bombs = 0;

int max_enemies = 20;
int current_n_enemies = 0;

enum Tags {
  TAG_NONE,
  TAG_PERMANENT
};

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

OBJECT_IMPL(CTimer);
OBJECT_PROPERTY(CTimer, time_remaining);

CTimer::CTimer(void* go)
  : Component((GO*)go, PRIORITY_THINK), time_remaining(0), expire_payload(NULL) {
}

void CTimer::update(float dt) {
  this->time_remaining -= dt;

  if(this->time_remaining <= 0) {
    Message* message = message_make(go, MESSAGE_TIMER_EXPIRED, this->expire_payload);
    // need to add component priorities for this to be effective
    message_postinbox(go, message);
    delete_me = 1;
  }
}

enum BombStates {
  BOMB_IDLE,
  BOMB_EXPLODING,
  BOMB_DONE
};

OBJECT_IMPL(CBombBehavior);

CBombBehavior::CBombBehavior(void* go)
  : Component((GO*)go, PRIORITY_ACT), state(BOMB_IDLE) {
  CTimer* timer = this->go->add_c<CTimer>();
  timer->time_remaining = bomb_delay - bomb_explode_start;
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
        Message* message = message_make(bomb, MESSAGE_EXPLOSION_NEARBY, NULL);
        message_postinbox(item, message);
        return 0;
      });
  }
}

OBJECT_IMPL(CEnemyBehavior);

enum EnemyState {
  ENEMY_FALLING,
  ENEMY_LANDED
};

CEnemyBehavior::CEnemyBehavior(void* go)
  : Component((GO*)go, PRIORITY_ACT), state(ENEMY_FALLING) {
}

void CEnemyBehavior::update(float dt) {
  if(this->state == ENEMY_FALLING && this->go->transform_parent) {
    // when we land, add the left and right behavior
    this->state = ENEMY_LANDED;

    CCollidable* coll = (CCollidable*)go->transform_parent->find_component(&CCollidable::Type);
    this->go->_vel.x = enemy_speed;
    CLeftAndRight* lnr = go->add_c<CLeftAndRight>();
    lnr->minx = -coll->w / 2;
    lnr->maxx = coll->w / 2;
  } else if(this->state == ENEMY_LANDED && !this->go->transform_parent) {
    // when our platform disappears, go back to falling
    this->state = ENEMY_FALLING;
    this->go->_vel.x = 0;
    CLeftAndRight* lnr = (CLeftAndRight*)go->find_component(&CLeftAndRight::Type);
    lnr->delete_me = 1;
  }

  // see if a bomb went off
  go->inbox.foreach([this](Message* message) -> int {
      if(message->kind == MESSAGE_EXPLOSION_NEARBY) {
        agent_send_terminate(go, message->source);
        return 1;
      }
      return 0;
    });
}

OBJECT_IMPL(CLeftAndRight);
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

OBJECT_IMPL(CInput);

CInput::CInput(void* go)
  : Component((GO*)go, PRIORITY_THINK) {

  this->state = NULL;
  this->fire_pressed = 0;
  this->facing = 1;
}

void CInput::update(float dt) {
  InputState input = this->state;

  if(input->action2) {
    if(!this->fire_pressed) {
      this->fire_pressed = 1;
    }
  } else if(this->fire_pressed) {
    //fire
    this->fire_pressed = 0;

    if(current_n_bombs < max_bombs) {
      struct Vector_ abs_pos;
      go->pos(&abs_pos);
      abs_pos.y += player_height;

      struct Vector_ abs_vel = {this->facing * throw_speed / 3, throw_speed};

      if(go->transform_parent) {
        struct Vector_ par_vel;
        go->transform_parent->vel(&par_vel);
        vector_add(&abs_vel, &abs_vel, &par_vel);
      }

      play_vorbis("sounds/Jump20.ogg", 0.4);
      bomb_make(&abs_pos, &abs_vel);
    }
  }

  go->_vel.x = input->leftright * player_speed;
  if(fabs(input->leftright) > 0.01) {
    this->facing = SIGN(input->leftright);
  }

  // dangerous conflation? we assume unparented means falling
  if(input->action1 && go->transform_parent) {
    go->_vel.y = player_jump_speed;
  }

  if(!input->action1 && !go->transform_parent) {
    go->_vel.y = MIN(go->_vel.y, 0);
  }
}

OBJECT_IMPL(CTestDisplay);
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

OBJECT_IMPL(CStaticSprite);
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

OBJECT_IMPL(CPlayerSprite);

SpriteList CPlayerSprite::list = NULL;

CPlayerSprite::CPlayerSprite(void* go)
  : Component((GO*)go, PRIORITY_SHOW) {
}

void CPlayerSprite::update(float dt) {
  Vector_ pos;
  go->pos(&pos);

  CInput* ci = (CInput*)go->find_component(&CInput::Type);
  float dx = floorf(camera->_pos.x);
  float dy = floorf(camera->_pos.y);
  float px = pos.x - dx;
  float py = pos.y - dy;
  Sprite sprite = frame_make_sprite();
  if(ci->facing > 0) {
    sprite_fillfromentry(sprite, go->world->atlas_entry(ATLAS, "guy"));
  } else {
    sprite_fillfromentry(sprite, go->world->atlas_entry(ATLAS, "guy-left"));
  }

  sprite->displayX = px;
  sprite->displayY = py;
  sprite->originX = 0.5;
  sprite->originY = 0.5;

  CStaticSprite::list = frame_spritelist_append(CStaticSprite::list, sprite);
}

OBJECT_IMPL(CDrawPatch);
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


OBJECT_IMPL(CCameraFocus);

CCameraFocus::CCameraFocus(void* go)
  : Component((GO*)go, PRIORITY_THINK), camera(NULL)  {
}

void CCameraFocus::update(float dt) {
  CInput* input = (CInput*)go->find_component(&CInput::Type);
  float facing_offset = screen_width / 4.0f * input->facing;
  float supported_offset = 0;

  if(input->state->updown) {
    supported_offset = screen_height / 2.0 * input->state->updown;
  } else {
    supported_offset = screen_height / 4.0;
  }

  if(input->state->leftright) {
    facing_offset = screen_width / 3.0f * input->state->leftright;
  }

  Vector_ offset = {screen_width / 2.0f, screen_height / 2.0f - supported_offset};
  Vector_ desired;
  go->pos(&desired);
  vector_sub(&desired, &desired, &offset);

  float max_v = 1600;
  if(input->state->updown){
    max_v = 600;
  }

  const float max_dx = max_v * dt;

  Vector_ delta;
  vector_sub(&delta, &desired, &camera->_pos);
  float mag = vector_mag(&delta);
  if(mag < max_dx) {
    // snap
    camera->_pos = desired;
    return;
  }

  vector_scale(&delta, &delta, max_dx / mag);
  vector_add(&camera->_pos, &camera->_pos, &delta);
}

OBJECT_IMPL(CParticleEmitter);
OBJECT_PROPERTY(CParticleEmitter, entry);
OBJECT_PROPERTY(CParticleEmitter, active);
OBJECT_PROPERTY(CParticleEmitter, max_life);
OBJECT_PROPERTY(CParticleEmitter, max_speed);
OBJECT_PROPERTY(CParticleEmitter, max_offset);
OBJECT_PROPERTY(CParticleEmitter, grav_accel);
OBJECT_PROPERTY(CParticleEmitter, start_scale);
OBJECT_PROPERTY(CParticleEmitter, end_scale);

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
  go->ttag = TAG_PERMANENT;

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

GO* enemy_make(float x, float y) {
  GO* go = world->create_go();
  go->_pos.x = x;
  go->_pos.y = y;

  CStaticSprite* ss = go->add_c<CStaticSprite>();
  ss->entry = world->atlas_entry(ATLAS, "enemy");

  CCollidable* coll = go->add_c<CCollidable>();
  coll->w = enemy_dim;
  coll->h = enemy_dim;

  CPlatformer* cp = go->add_c<CPlatformer>();
  cp->grav_accel = player_gravity_accel;

  go->add_c<CEnemyBehavior>();

  return go;
}

void player_setup() {
  camera->_pos.x = 100;
  camera->_pos.y = 100;
  vector_zero(&camera->_vel);

  player_go->_pos.x = 100;
  player_go->_pos.y = 100;
  player_go->ttag = TAG_PERMANENT;

  player_go->add_c<CPlayerSprite>();
  CCollidable* coll = player_go->add_c<CCollidable>();
  coll->w = player_width;
  coll->h = player_height;

  CPlatformer* cp = player_go->add_c<CPlatformer>();
  cp->grav_accel = player_gravity_accel;

  CCameraFocus* cam = player_go->add_c<CCameraFocus>();
  cam->camera = camera;

  player_input = player_go->add_c<CInput>();
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
  player_setup();

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

WinState win_state = STATE_START;
const float endgame_delay = 3;
float endgame_timeout;

void game_end(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);
  if(win_state == STATE_WIN) {
    SpriteList text = spritelist_from_string(NULL, world->atlas(ATLAS), FONT_MEDIUM,
                                             "WINNER", 400, screen_height/2);
    spritelist_enqueue_for_screen(text);
  } else if(win_state == STATE_LOSE) {
    SpriteList text = spritelist_from_string(NULL, world->atlas(ATLAS), FONT_MEDIUM,
                                             "LOSER", 400, screen_height/2);
    spritelist_enqueue_for_screen(text);
  }

  endgame_timeout -= dt;
  if(endgame_timeout <= 0) {
    win_state = STATE_START;
    struct Vector_ center = {screen_width / 2.0f, screen_height / 2.0f};
    world_foreach(world, &center, INFINITY, [] (GO* go) -> int {
        if(go->ttag != TAG_PERMANENT) {
          agent_send_terminate(go, world);
        }
        return 0;
      });
    set_game_step(game_step);
  }
}

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
  float dt = clock_update(main_clock, delta / 1000.0);
  enemy_timer -= dt;
  if(enemy_timer <= 0 && current_n_enemies < max_enemies) {
    enemy_timer = enemy_period;

    enemy_make(screen_width / 2, screen_height * 4);
    // add enemy
  }

  SpriteList bg = tilemap_spritelist(background, camera->_pos.x + screen_width,
                                     camera->_pos.y + screen_height,
                                     screen_width, screen_height);
  spritelist_enqueue_for_screen(bg);

  player_input->state = state;
  world_notify_collisions(world);
  world->update(dt);

  spritelist_enqueue_for_screen(CStaticSprite::list);
  CStaticSprite::list = NULL;

  spritelist_enqueue_for_screen(CDrawPatch::list);
  CDrawPatch::list = NULL;

  spritelist_enqueue_for_screen(CPlayerSprite::list);
  CPlayerSprite::list = NULL;

  // render all particles
  spritelist_enqueue_for_screen_colored(CParticleEmitter::list);
  CParticleEmitter::list = NULL;

  render_hud();
}

void game_shutdown() {
}
