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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

float bomb_delay = 6.0f;
float player_speed = 600;
float player_jump_speed = 1200;
float player_width = 64;
float player_height = 64;
float player_jump_height = 300;
float bomb_max_height = 400;
float bomb_dim = 32;
float player_gravity_accel;
float bomb_gravity_accel;
float throw_speed = 1200;
float ground_level = 100;
float charge_delay = 0.2;
float bomb_explode_start = 0.3;
float bomb_chain_factor = 5.0;
float enemy_speed = 100;
float enemy_dim = 48;

World* world;
struct Random_ rgen;
GO* player_go;
CInput* player_input;

SpriteAtlas atlas;
Clock main_clock;

int max_bombs = 50;
int current_n_bombs = 0;

int max_enemies = 20;
int current_n_enemies = 0;

enum Tags {
  TAG_PLAYER,
  TAG_BOMB,
  TAG_PLATFORM
};

OBJECT_IMPL(CTimer);

CTimer::CTimer()
  : Component(NULL), time_remaining(0), expire_payload(NULL)
{}

CTimer::CTimer(GO* go, float time_remaining, void* payload)
  : Component(go), time_remaining(time_remaining), expire_payload(payload) {
}

void CTimer::update(float dt) {
  this->time_remaining -= dt;

  if(this->time_remaining <= 0) {
    GO* go = component_to_go(this);
    Message* message = message_make(go, MESSAGE_TIMER_EXPIRED, this->expire_payload);
    // need to add component priorities for this to be effective
    message_postinbox(go, message);
    delete this;
  }
}

int is_timer_expired(GO* go, void* data) {
  // look for an expired timer message
  DLLNode node = agent_inbox(go)->head;
  while(node) {
    Message* message = node_to_message(node);
    if(message->kind == MESSAGE_TIMER_EXPIRED) {
      return 1;
    }
    node = node->next;
  }
  return 0;
}

enum BombStates {
  BOMB_IDLE,
  BOMB_EXPLODING,
  BOMB_DONE
};

void bomb_detonate(GO* bomb) {
  CTimer* timer = (CTimer*)go_find_component(bomb, &CTimer::Type);
  if(timer && timer->time_remaining > bomb_explode_start) {
    timer->time_remaining = bomb_explode_start;
  }
}

int delete_all_but_player(GO* go, void* udata) {
  if(go == udata) return 0;
  if(go->ttag == TAG_PLAYER) return 0;

  // ignore non-collidables
  CCollidable* coll = (CCollidable*)go_find_component(go, &CCollidable::Type);
  if(!coll) return 0;

  // destroy what we hit
  if(go->ttag == TAG_BOMB) {
    bomb_detonate(go);
  } else if(go->ttag != TAG_PLATFORM) {
    agent_send_terminate(go, world);
  }
  return 0;
}

OBJECT_IMPL(CBombBehavior);

CBombBehavior::CBombBehavior()
  : Component(NULL), state(BOMB_IDLE) {
}

CBombBehavior::CBombBehavior(GO* go)
  : Component(go), state(BOMB_IDLE) {
  new CTimer(go, bomb_delay - bomb_explode_start, NULL);
  this->state = BOMB_IDLE;
}

void CBombBehavior::update(float dt) {
  GO* bomb = this->parent_go;

  // if we're supported then zero our x so that we're sticky
  if(bomb->transform_parent) {
    bomb->vel.x = 0;
  }

  // nothing to do if a timer hasn't gone off
  if(!is_timer_expired(bomb, NULL)) return;

  if(this->state == BOMB_IDLE) {
    // set the next timer and change state
    new CTimer(bomb, bomb_explode_start, NULL);
    this->state = BOMB_EXPLODING;
    world_foreach(go_world(bomb), &bomb->pos, bomb_dim * bomb_chain_factor,
                  delete_all_but_player, bomb);
  } else if(this->state == BOMB_EXPLODING) {
    // destroy the bomb
    this->state = BOMB_DONE;
    agent_send_terminate(bomb, go_world(bomb));
  }
}

OBJECT_IMPL(CLeftAndRight);

CLeftAndRight::CLeftAndRight()
  : Component(NULL), minx(0), maxx(screen_width) {
}

CLeftAndRight::CLeftAndRight(GO* go, float minx, float maxx)
  : Component(go), minx(minx), maxx(maxx) {
}

void CLeftAndRight::update(float dt) {
  GO* go = component_to_go(this);

  if(go->vel.x > 0) {
    if(go->pos.x > this->maxx) {
      go->pos.x = this->maxx;
      go->vel.x = -go->vel.x;
    }
  } else {
    if(go->pos.x < this->minx) {
      go->pos.x = this->minx;
      go->vel.x = -go->vel.x;
    }
  }
}

OBJECT_IMPL(CInput);

CInput::CInput()
  : Component(NULL), state(NULL), fire_pressed(0), facing(1) {
}

CInput::CInput(GO* go)
  : Component(go) {

  this->state = NULL;
  this->fire_pressed = 0;
  this->facing = 1;
}

void CInput::update(float dt) {
  GO* go = component_to_go(this);
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
      go_pos(&abs_pos, go);
      abs_pos.y += player_height;

      struct Vector_ abs_vel = {this->facing * throw_speed / 3, throw_speed};

      if(go->transform_parent) {
        struct Vector_ par_vel;
        go_vel(&par_vel, go->transform_parent);
        vector_add(&abs_vel, &abs_vel, &par_vel);
      }

      bomb_make(&abs_pos, &abs_vel);
    }
  }

  go->vel.x = input->leftright * player_speed;
  if(fabs(input->leftright) > 0.01) {
    this->facing = SIGN(input->leftright);
  }

  // dangerous conflation? we assume unparented means falling
  if(input->action1 && go->transform_parent) {
    go->vel.y = player_jump_speed;
  }

  if(!input->action1 && !go->transform_parent) {
    go->vel.y = MIN(go->vel.y, 0);
  }
}

OBJECT_IMPL(CTestDisplay);

CTestDisplay::CTestDisplay()
  : Component(NULL) {
}

CTestDisplay::CTestDisplay(GO* go)
  : Component(go) {
}

void CTestDisplay::update(float dt) {
  GO* go = component_to_go(this);
  CCollidable* coll = (CCollidable*)go_find_component(go, &CCollidable::Type);

  assert(coll);

  struct ColoredRect_ rect;
  collidable_rect((Rect)&rect, coll);
  rect.color[0] = 1.0f;
  rect.color[1] = 0.0f;
  rect.color[2] = 0.0f;
  rect.color[3] = 1.0f;

  filledrect_enqueue_for_screen(&rect);
}

GO* platform_make(float x, float y, float w, float h) {
  GO* go = new GO(world);
  go->pos.x = x;
  go->pos.y = y;
  go->vel.x = 0;
  go->vel.y = 0;
  go->ttag = TAG_PLATFORM;

  new CTestDisplay(go);
  new CCollidable(go, w, h);
  return go;
}

GO* slidingplatform_make(float x, float y, float w, float h, float speed,
                         float minx, float maxx) {
  GO* go = platform_make(x, y, w, h);
  go->vel.x = speed;

  new CLeftAndRight(go, minx, maxx);
  return go;
}

void player_setup() {
  player_go = new GO(world);
  player_go->pos.x = 100;
  player_go->pos.y = 100;
  player_go->ttag = TAG_PLAYER;

  new CTestDisplay(player_go);
  new CCollidable(player_go, player_width, player_height);
  new CPlatformer(player_go, player_gravity_accel);
  player_input = new CInput(player_go);
}

GO* bomb_make(Vector pos, Vector vel) {
  GO* go = new GO(world);

  go->ttag = TAG_BOMB;
  go->pos = *pos;
  go->vel = *vel;

  new CTestDisplay(go);
  new CCollidable(go, bomb_dim, bomb_dim);
  new CPlatformer(go, bomb_gravity_accel);
  new CBombBehavior(go);

  return go;
}

void game_step(long delta, InputState state);

void game_support_init() {
  main_clock = clock_make();

  player_gravity_accel = (player_jump_speed * player_jump_speed) / (2 * player_jump_height);
  bomb_gravity_accel = (throw_speed * throw_speed) / (2 * bomb_max_height);

  atlas = spriteatlas_load("resources/images_default.dat", "resources/images_default.png");

  world = new World();
}

void game_init() {
  game_support_init();
  player_setup();

  platform_make(screen_width / 2, 32, screen_width, 64);
  slidingplatform_make(300, 300, 256, 64, 100, 128, 1024);
  slidingplatform_make(600, 600, 256, 64, 100, 128, 1024);

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
    SpriteList text = spritelist_from_string(NULL, atlas, FONT_MEDIUM,
                                             "WINNER", 400, screen_height/2);
    spritelist_enqueue_for_screen(text);
  } else if(win_state == STATE_LOSE) {
    SpriteList text = spritelist_from_string(NULL, atlas, FONT_MEDIUM,
                                             "LOSER", 400, screen_height/2);
    spritelist_enqueue_for_screen(text);
  }

  endgame_timeout -= dt;
  if(endgame_timeout <= 0) {
    win_state = STATE_START;
    struct Vector_ center = {screen_width / 2.0f, screen_height / 2.0f};
    world_foreach(world, &center, INFINITY, delete_all_but_player, NULL);
    set_game_step(game_step);
  }
}

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);
  enemy_timer -= dt;
  if(enemy_timer <= 0 && current_n_enemies < max_enemies) {
    enemy_timer = enemy_period;
    // add enemy
  }


  player_input->state = state;
  world_notify_collisions(world);
  world->update(dt);
}

void game_shutdown() {
}
