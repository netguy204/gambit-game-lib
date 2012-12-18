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
#include "worldgen.h"
#include "random.h"
#include "updateable.h"
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

World world;
struct Random_ rgen;
GO player_go;
CInput player_input;

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

void* CTimerObject_ctor(void* _self, va_list* app) {
  CTimer timer = super_ctor(CTimerObject(), _self, app);
  timer->time_remaining = va_arg(*app, double);
  timer->expire_payload = va_arg(*app, void*);
  return timer;
}

void CTimerObject_update(void* _self, float dt) {
  super_update(CTimerObject(), _self, dt);

  CTimer timer = _self;
  timer->time_remaining -= dt;

  if(timer->time_remaining <= 0) {
    GO go = component_to_go(timer);
    Message message = message_make((Agent)go, MESSAGE_TIMER_EXPIRED, timer->expire_payload);
    // need to add component priorities for this to be effective
    message_postinbox((Agent)go, message);
    delete(timer);
  }
}

const void* CTimerObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "CTimer",
              ComponentObject(), sizeof(struct CTimer_),
              ctor, CTimerObject_ctor,
              update, CTimerObject_update,
              0);
  return class;
}

int is_timer_expired(GO go, void* data) {
  // look for an expired timer message
  DLLNode node = agent_inbox(go)->head;
  while(node) {
    Message message = node_to_message(node);
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

void bomb_detonate(GO bomb) {
  CTimer timer = go_find_component(bomb, CTimerObject());
  if(timer && timer->time_remaining > bomb_explode_start) {
    timer->time_remaining = bomb_explode_start;
  }
}

int delete_all_but_player(GO go, void* udata) {
  if(go->ttag == TAG_PLAYER) return 0;

  // ignore non-collidables
  CCollidable coll = go_find_component(go, CCollidableObject());
  if(!coll) return 0;

  // check collision
  struct Rect_ orect;
  collidable_rect(&orect, coll);
  Rect mrect = udata;
  if(!rect_intersect(&orect, mrect)) return 0;

  // destroy what we hit
  if(go->ttag == TAG_BOMB) {
    bomb_detonate(go);
  } else if(go->ttag != TAG_PLATFORM) {
    agent_send_terminate((Agent)go, (Agent)world);
  }
  return 0;
}

void* CBombBehaviorObject_ctor(void* _self, va_list* app) {
  CBombBehavior bomb = super_ctor(CBombBehaviorObject(), _self, app);

  new(CTimerObject(), bomb, bomb_delay - bomb_explode_start, NULL);
  bomb->state = 0;
  return bomb;
}

void CBombBehaviorObject_update(void* _self, float dt) {
  super_update(CBombBehaviorObject(), _self, dt);
  CBombBehavior bb = _self;
  GO bomb = component_to_go(bb);

  // if we're supported then zero our x so that we're sticky
  if(bomb->transform_parent) {
    bomb->vel.x = 0;
  }

  // nothing to do if a timer hasn't gone off
  if(!is_timer_expired(bomb, NULL)) return;

  if(bb->state == BOMB_IDLE) {
    // set the next timer and change state
    new(CTimerObject(), bomb, bomb_explode_start, NULL);
    bb->state = BOMB_EXPLODING;
    world_foreach(go_world(bomb), &bomb->pos, bomb_dim * bomb_chain_factor,
                  delete_all_but_player, NULL);
  } else if(bb->state == BOMB_EXPLODING) {
    // destroy the bomb
    bb->state = BOMB_DONE;
    agent_send_terminate((Agent)bomb, (Agent)go_world(bomb));
  }
}

const void* CBombBehaviorObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "CBombBehavior",
              ComponentObject(), sizeof(struct CBombBehavior_),
              ctor, CBombBehaviorObject_ctor,
              update, CBombBehaviorObject_update,
              0);
  return class;
}

void* CLeftAndRightObject_ctor(void* _self, va_list* app) {
  CLeftAndRight lar = super_ctor(CLeftAndRightObject(), _self, app);
  lar->minx = va_arg(*app, double);
  lar->maxx = va_arg(*app, double);
  return lar;
}

void CLeftAndRightObject_update(void* _self, float dt) {
  super_update(CLeftAndRightObject(), _self, dt);
  CLeftAndRight lar = _self;
  GO go = component_to_go(lar);

  if(go->vel.x > 0) {
    if(go->pos.x > lar->maxx) {
      go->pos.x = lar->maxx;
      go->vel.x = -go->vel.x;
    }
  } else {
    if(go->pos.x < lar->minx) {
      go->pos.x = lar->minx;
      go->vel.x = -go->vel.x;
    }
  }
}

const void* CLeftAndRightObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "CLeftAndRight",
              ComponentObject(), sizeof(struct CLeftAndRight_),
              ctor, CLeftAndRightObject_ctor,
              update, CLeftAndRightObject_update,
              0);
  return class;
}

void* CInputObject_ctor(void* _self, va_list* app) {
  CInput ci = super_ctor(CInputObject(), _self, app);
  ci->state = NULL;
  ci->fire_pressed = 0;
  ci->facing = 1;
  return ci;
}

void CInputObject_update(void* _self, float dt) {
  super_update(CInputObject(), _self, dt);
  CInput ci = _self;
  GO go = component_to_go(ci);
  InputState input = ci->state;

  if(input->action2) {
    if(!ci->fire_pressed) {
      ci->fire_pressed = 1;
    }
  } else if(ci->fire_pressed) {
    //fire
    ci->fire_pressed = 0;

    if(current_n_bombs < max_bombs) {
      struct Vector_ abs_pos;
      go_pos(&abs_pos, go);

      struct Vector_ abs_vel = {ci->facing * throw_speed / 3, throw_speed};

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
    ci->facing = SIGN(input->leftright);
  }

  // dangerous conflation? we assume unparented means falling
  if(input->action1 && go->transform_parent) {
    go->vel.y = player_jump_speed;
  }

  if(!input->action1 && !go->transform_parent) {
    go->vel.y = MIN(go->vel.y, 0);
  }
}

const void* CInputObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "CInput",
              ComponentObject(), sizeof(struct CInput_),
              ctor, CInputObject_ctor,
              update, CInputObject_update,
              0);
  return class;
}

GO platform_make(float x, float y, float w, float h) {
  GO go = new(GameObject(), AGENT_IDLE, world);
  go->pos.x = x;
  go->pos.y = y;
  go->vel.x = 0;
  go->vel.y = 0;
  go->ttag = TAG_PLATFORM;

  new(CCollidableObject(), go, w, h);
  return go;
}

GO slidingplatform_make(float x, float y, float w, float h, float speed,
                        float minx, float maxx) {
  GO go = platform_make(x, y, w, h);
  go->vel.x = speed;

  new(CLeftAndRightObject(), go, minx, maxx);
  return go;
}

void player_setup() {
  player_go = new(GameObject(), AGENT_IDLE, world);
  player_go->pos.x = 100;
  player_go->pos.y = 100;
  player_go->ttag = TAG_PLAYER;

  new(CCollidableObject(), player_go, player_width, player_height);
  new(CPlatformerObject(), player_go, player_gravity_accel);
  player_input = new(CInputObject(), player_go);
}

GO bomb_make(Vector pos, Vector vel) {
  GO go = new(GameObject(), AGENT_IDLE, world);
  go->ttag = TAG_BOMB;

  go->pos = *pos;
  go->vel = *vel;
  new(CCollidableObject(), go, bomb_dim, bomb_dim);
  new(CPlatformerObject(), go, bomb_gravity_accel);
  new(CBombBehaviorObject(), go);

  return go;
}

void game_step(long delta, InputState state);

void game_support_init() {
  main_clock = clock_make();

  player_gravity_accel = (player_jump_speed * player_jump_speed) / (2 * player_jump_height);
  bomb_gravity_accel = (throw_speed * throw_speed) / (2 * bomb_max_height);

  atlas = spriteatlas_load("resources/images_default.dat", "resources/images_default.png");

  world = new(WorldObject(), COLLECTIVE_IDLE, 0);
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
    struct Vector_ center = {screen_width / 2, screen_height / 2};
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
  update(world, dt);
}

void print_names(DLL list) {
  DLLNode node = list->head;
  while(node) {
    Particle particle = node_to_particle(node);
    printf("%s\n", className(classOf(particle)));
    node = node->next;
  }
}

void game_shutdown() {
}
