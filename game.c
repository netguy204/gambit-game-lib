#include "game.h"
#include "testlib.h"
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
float bomb_max_height = 800;
float bomb_dim = 32;
float player_gravity_accel;
float bomb_gravity_accel;
float charge_speed = 1200.0;
float charge_max = 1200;
float ground_level = 100;
float charge_delay = 0.2;
float bomb_explode_start = 0.3;
float bomb_chain_factor = 5.0;
float enemy_speed = 100;
float enemy_dim = 48;

struct PlayerState_ player;

int max_platforms = 10;
const void* PlatformObject;
struct DLL_ platforms;
FixedAllocator platform_allocator;

struct Random_ rgen;
Clock main_clock;

const void* ParticleObject;
const void* PlatformerObject;

int max_bombs = 50;
const void* BombObject;
struct DLL_ bombs;
FixedAllocator bomb_allocator;

int max_enemies = 20;
const void* EnemyObject;
const void* EnemyPlatformObject;
struct DLL_ enemies;
struct DLL_ enemy_platforms;
FixedAllocator enemy_allocator;

void player_abs_pos(Vector pos) {
  platformer_abs_pos(pos, &player.platformer);
}

void player_abs_vel(Vector vel) {
  platformer_abs_vel(vel, &player.platformer);
}

void player_rect(Rect rect) {
  platformer_rect(rect, &player.platformer);
}

void* ParticleObject_ctor(void* _self, va_list* app) {
  Particle particle = super_ctor(ParticleObject, _self, app);
  DLL list = va_arg(*app, DLL);

  particle->containing_list = list;

  if(particle->containing_list) {
    dll_add_head(particle->containing_list, &particle->node);
  }

  particle->scale = 1.0f;
  particle->angle = 0.0f;
  particle->dsdt = 0.0f;
  particle->dadt = 0.0f;

  return particle;
}

void* ParticleObject_dtor(void* _self) {
  Particle particle = _self;
  if(particle->containing_list) {
    dll_remove(particle->containing_list, &particle->node);
  }
  return super_dtor(ParticleObject, _self);
}

void ParticleObject_update(void* _self, float dt) {
  // don't call super: we are root for this method
  Particle particle = _self;
  particle_integrate(particle, dt);
}

void* PlatformObject_alloci(void* _class) {
  return fixed_allocator_alloc(platform_allocator);
}

void PlatformObject_dealloci(void* _self) {
  fixed_allocator_free(platform_allocator, _self);
}

void PlatformObject_update(void* _self, float dt) {
  super_update(PlatformObject, _self, dt);
  Platform platform = _self;
  platform_rect(&platform->rect, platform);
}

void* PlatformObject_ctor(void* _self, va_list* app) {
  Platform platform = super_ctor(PlatformObject, _self, app);
  Particle particle = (Particle)platform;

  Vector pos = va_arg(*app, Vector);
  particle->pos = *pos;
  particle->vel.x = 0;
  particle->vel.y = 0;

  return platform;
}

void* PlatformerObject_ctor(void* _self, va_list* app) {
  Platformer platformer = super_ctor(PlatformerObject, _self, app);
  Particle particle = (Particle)platformer;

  Vector pos = va_arg(*app, Vector);
  Vector vel = va_arg(*app, Vector);

  // offset a bit in vel direction to get around the player
  struct Vector_ offset;
  if(vector_mag(vel) < 0.0001) {
    offset.y = player_width / 2;
    offset.x = 0;
  } else {
    vector_norm(&offset, vel);
    vector_scale(&offset, &offset, player_width / 2);
  }

  vector_add(&particle->pos, pos, &offset);
  particle->vel = *vel;
  platformer->falling = 1;
  platformer->grav_accel = 0.0f;
  platformer->parent = NULL;

  return particle;
}

void PlatformerObject_update(void* _self, float dt) {
  Platformer platformer = _self;
  Particle particle = _self;

  super_update(PlatformerObject, _self, dt);
  platformer_resolve(platformer, &platforms);

  if(platformer->falling) {
    particle->vel.y -= (platformer->grav_accel * dt);
  }
}

void* BombObject_alloci(const void* _class) {
  return fixed_allocator_alloc(bomb_allocator);
}

void BombObject_dealloci(void* _self) {
  fixed_allocator_free(bomb_allocator, _self);
}

void* BombObject_ctor(void* _self, va_list* app) {
  Bomb bomb = super_ctor(BombObject, _self, app);
  Platformer platformer = (Platformer)bomb;
  platformer->grav_accel = bomb_gravity_accel;
  bomb->time_remaining = bomb_delay;
  bomb->searched_neighbors = 0;
  return bomb;
}

void bomb_detonate(Bomb bomb) {
  if(bomb->time_remaining > bomb_explode_start) {
    bomb->time_remaining = bomb_explode_start;
  }
}

Platformer platformer_within(Platformer platformer, float min_dist, DLL list) {
  DLLNode node = list->head;
  struct Vector_ mp;
  platformer_abs_pos(&mp, platformer);

  while(node) {
    Platformer p = node_to_platformer(node);

    if(p != platformer) {
      struct Vector_ tp;
      struct Vector_ offset;

      platformer_abs_pos(&tp, p);
      vector_sub(&offset, &mp, &tp);
      if(vector_mag(&offset) < min_dist) {
        return p;
      }
    }
    node = node->next;
  }
  return NULL;
}

void BombObject_update(void* _self, float dt) {
  super_update(BombObject, _self, dt);

  Bomb bomb = _self;
  Platformer platformer = _self;
  Particle particle = _self;

  bomb->time_remaining -= dt;

  if(!platformer->falling) {
    particle->vel.x = 0.0f;
  }

  if(bomb->time_remaining <= 0) {
    delete(bomb);
  } else if(bomb->time_remaining < 0.1) {
    // contract to acheive zero at t=0
    particle->dsdt = -particle->scale / bomb->time_remaining;

    // search our neighbors to see if we can set off a chain reaction
    if(!bomb->searched_neighbors) {
      bomb->searched_neighbors = 1;
      Platformer p;
      float dist = platformer->w * bomb_chain_factor;
      if((p = platformer_within(platformer, dist, &bombs))) {
        bomb_detonate((Bomb)p);
      }

      // kill any enemies we hit
      while((p = platformer_within(platformer, dist, &enemies))) {
        delete(p);
      }
    }

  } else if(bomb->time_remaining < bomb_explode_start) {
    // expand to acheive 2x at t = 0.1
    particle->dsdt = 2.0 / (bomb->time_remaining - 0.1);
  }
}

void* EnemyObject_alloci(void* _class) {
  return fixed_allocator_alloc(enemy_allocator);
}

void EnemyObject_dealloci(void* _self) {
  fixed_allocator_free(enemy_allocator, _self);
}

void EnemyObject_mirrorparticle(Enemy enemy) {
  // be careful not to copy the object or node in the particle
  Platformer epl = (Platformer)enemy;
  size_t offset = offsetof(struct Particle_, pos);
  memcpy((char*)(&enemy->platform) + offset, (char*)enemy + offset,
         sizeof(struct Particle_) - offset);

  enemy->platform.w = epl->w;
  enemy->platform.h = epl->h;
}

void* EnemyObject_ctor(void* _self, va_list* app) {
  Enemy enemy = super_ctor(EnemyObject, _self, app);
  Particle particle = (Particle)enemy;
  Platformer platformer = (Platformer)enemy;

  init(EnemyPlatformObject, &enemy->platform, &enemy_platforms, &particle->pos);
  platformer->grav_accel = player_gravity_accel;
  platformer->w = enemy_dim;
  platformer->h = enemy_dim;
  EnemyObject_mirrorparticle(enemy);

  return enemy;
}

void* EnemyObject_dtor(void* _self) {
  Enemy enemy = _self;
  dtor(&enemy->platform);
  return super_dtor(EnemyObject, _self);
}

void EnemyPlatformObject_update(void* _self, float dt) {
  // slave ourselves to our parent particle and don't do our own
  // updates
  Platform platform = _self;
  Enemy enemy = container_of(platform, struct Enemy_, platform);
  Platformer epl = (Platformer)enemy;

  EnemyObject_mirrorparticle(enemy);

  platform_rect(&enemy->platform.rect, platform);
}

void EnemyObject_update(void* _self, float dt) {
  Enemy enemy = _self;
  Particle epa = _self;
  Platformer epl = _self;

  super_update(EnemyObject, _self, dt);

  // if we've just hit a platform, give us a horizontal kick
  if(!epl->falling && fabs(epa->vel.x) < 0.001) {
    epa->vel.x = enemy_speed;
  }

  // if we're near the edge of a platform, turn around
  if(epl->parent) {
    float xd = (epl->parent->w / 2.0) - fabs(epa->pos.x);
    float margin = 5;
    if(xd < margin) {
      float bump = margin + 1 - xd;
      epa->vel.x = -epa->vel.x;
      epa->pos.x += SIGN(epa->vel.x) * bump;
    }
  }

  update(&enemy->platform, dt);
}

void game_step(long delta, InputState state);

const void* SlidingPlatformObject;
void SlidingPlatformObject_update(void* _self, float dt) {
  super_update(SlidingPlatformObject, _self, dt);

  Platform platform = _self;
  Particle particle = _self;

  float x = particle->pos.x;
  if(x > 1024 || x < 64) {
    particle->pos.x = MAX(64, MIN(1024, x));
    particle->vel.x = -particle->vel.x;
  }
}

void game_support_init() {
  main_clock = clock_make();
  platform_allocator = fixed_allocator_make(sizeof(struct Platform_), max_platforms,
                                            "platform_allocator");
  bomb_allocator = fixed_allocator_make(sizeof(struct Bomb_), max_bombs,
                                        "bomb_allocator");
  enemy_allocator = fixed_allocator_make(sizeof(struct Enemy_), max_enemies,
                                         "enemy_allocator");

  updateable_init();

  ParticleObject = new(UpdateableClass, "Particle",
                       Object, sizeof(struct Particle_),
                       ctor, ParticleObject_ctor,
                       dtor, ParticleObject_dtor,
                       update, ParticleObject_update,
                       0);

  PlatformObject = new(UpdateableClass, "Platform",
                       ParticleObject, sizeof(struct Platform_),
                       alloci, PlatformObject_alloci,
                       dealloci, PlatformObject_dealloci,
                       ctor, PlatformObject_ctor,
                       update, PlatformObject_update,
                       0);

  EnemyPlatformObject = new(UpdateableClass, "EnemyPlatform",
                            PlatformObject, sizeof(struct Platform_),
                            update, EnemyPlatformObject_update,
                            0);

  SlidingPlatformObject = new(UpdateableClass, "SlidingPlatform",
                              PlatformObject, sizeof(struct Platform_),
                              update, SlidingPlatformObject_update,
                              0);

  PlatformerObject = new(UpdateableClass, "Platformer",
                         ParticleObject, sizeof(struct Platformer_),
                         ctor, PlatformerObject_ctor,
                         update, PlatformerObject_update,
                         0);

  BombObject = new(UpdateableClass, "Bomb",
                   PlatformerObject, sizeof(struct Bomb_),
                   alloci, BombObject_alloci,
                   dealloci, BombObject_dealloci,
                   ctor, BombObject_ctor,
                   update, BombObject_update,
                   0);

  EnemyObject = new(UpdateableClass, "Enemy",
                    PlatformerObject, sizeof(struct Enemy_),
                    alloci, EnemyObject_alloci,
                    dealloci, EnemyObject_dealloci,
                    ctor, EnemyObject_ctor,
                    dtor, EnemyObject_dtor,
                    update, EnemyObject_update,
                    0);

  player_gravity_accel = (player_jump_speed * player_jump_speed) / (2 * player_jump_height);
  bomb_gravity_accel = (charge_max * charge_max) / (2 * bomb_max_height);
}

void game_init() {
  game_support_init();

  struct Vector_ player_pos = {100.0f, 100.0f};
  struct Vector_ player_vel = {0.0f, 0.0f};
  init(PlatformerObject, &player, NULL, &player_pos, &player_vel);
  ((Platformer)&player)->w = player_width;
  ((Platformer)&player)->h = player_height;
  ((Platformer)&player)->grav_accel = player_gravity_accel;
  player.charging = 0;
  player.fire_pressed = 0;
  vector_zero(&player.fire_charge);

  struct Vector_ ground_platform = {screen_width / 2, 32};
  Platform ground = new(PlatformObject, &platforms, &ground_platform);
  ground->w = screen_width;
  ground->h = 64;

  /*
  struct Vector_ left_wall = {32, screen_height/2};
  Platform left = new(PlatformObject, &platforms, &left_wall);
  left->w = 64;
  left->h = screen_height;

  struct Vector_ right_wall = {screen_width - 32, screen_height/2};
  Platform right = new(PlatformObject, &platforms, &right_wall);
  right->w = 64;
  right->h = screen_height;
  */

  struct Vector_ test_platform = {300, 300};
  Platform platform = new(SlidingPlatformObject, &platforms, &test_platform);
  Particle pp = (Particle)platform;
  platform->w = 256;
  platform->h = 64;
  pp->vel.x = 100;

  struct Vector_ test_platform2 = {600, 600};
  Platform platform2 = new(SlidingPlatformObject, &platforms, &test_platform2);
  Particle pp2 = (Particle)platform2;
  platform2->w = 256;
  platform2->h = 64;
  pp2->vel.x = 100;

  set_game_step(game_step);
}

void handle_input(InputState state, float dt) {
  Particle pp = (Particle)&player;

  if(state->action2) {
    if(!player.fire_pressed) {
      player.fire_pressed = 1;
      player.fire_timeout = charge_delay;
    }

    player.fire_timeout -= dt;
    if(player.fire_timeout <= 0 && !player.platformer.falling) {
      player.charging = 1;
    }

    if(player.charging) {
      if(!player.platformer.falling) {
        pp->vel.x = 0;
      }

      struct Vector_ addl_charge = {
        state->leftright * charge_speed * dt,
        state->updown * charge_speed * dt
      };

      vector_add(&player.fire_charge, &player.fire_charge, &addl_charge);
      float mag = vector_mag(&player.fire_charge);
      if(mag > charge_max) {
        vector_scale(&player.fire_charge, &player.fire_charge, charge_max / mag);
      }
    }
  } else if(player.fire_pressed) {
    //fire
    player.fire_pressed = 0;
    player.charging = 0;

    if(dll_count(&bombs) < max_bombs) {
      struct Vector_ abs_pos;
      player_abs_pos(&abs_pos);

      struct Vector_ abs_vel = player.fire_charge;
      if(player.platformer.parent) {
        vector_add(&abs_vel, &abs_vel, &player.platformer.parent->particle.vel);
      }

      Bomb bomb = new(BombObject, &bombs, &abs_pos, &abs_vel);
      platformer_setdims((Platformer)bomb, bomb_dim, bomb_dim);
    }
  } else {
    pp->vel.x = state->leftright * player_speed;
  }

  if(state->action1 && !player.platformer.falling) {
    pp->vel.y = player_jump_speed;
  }

  if(!state->action1 && player.platformer.falling) {
    pp->vel.y = MIN(pp->vel.y, 0);
  }
}

void update_particles(DLL list, float dt) {
  DLLNode node = list->head;
  while(node) {
    update(node_to_particle(node), dt);
    node = node->next;
  }
}

void platformer_enqueue(Platformer platformer, float r, float g, float b) {
  struct ColoredRect_ prect;
  Particle particle = (Particle)platformer;
  platformer_rect((Rect)&prect, platformer);
  rect_scaled((Rect)&prect, (Rect)&prect, particle->scale, particle->scale);

  prect.color[0] = r;
  prect.color[1] = g;
  prect.color[2] = b;
  prect.color[3] = 1.0f;

  filledrect_enqueue_for_screen(&prect);
}

void platformers_enqueue(DLL list) {
  DLLNode node = list->head;
  while(node) {
    Platformer platformer = node_to_platformer(node);
    platformer_enqueue(platformer, 1.0, 0.0, 0.0);
    node = node->next;
  }
}

const float enemy_period = 1;
float enemy_timer = enemy_period;

typedef enum {
  STATE_START,
  STATE_PLAY,
  STATE_WIN
} WinState;

WinState win_state = STATE_START;

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);
  enemy_timer -= dt;
  if(enemy_timer <= 0 && dll_count(&enemies) < max_enemies) {
    if(win_state == STATE_START) {
      win_state = STATE_PLAY;
    }

    enemy_timer = enemy_period;
    struct Vector_ epos = {screen_width/2, screen_height};
    struct Vector_ evel = {0, 0};
    Enemy enemy = new(EnemyObject, &enemies, &epos, &evel);
  }

  if(win_state == STATE_PLAY && dll_count(&enemies) == 0) {
    win_state = STATE_WIN;
  }

  update_particles(&platforms, dt);
  update_particles(&bombs, dt);
  update_particles(&enemies, dt);

  handle_input(state, dt);
  update(&player, dt);

  platformer_enqueue((Platformer)&player, 1.0f, 0.0f, 1.0f);

  if(player.charging) {
    struct ColoredRect_ crect;
    struct Vector_ cvect;
    struct Vector_ player_abs;
    player_abs_pos(&player_abs);

    vector_scale(&cvect, &player.fire_charge, 0.5);
    vector_add(&cvect, &player_abs, &cvect);
    rect_centered((Rect)&crect, &cvect, player_width/2, player_height/2);
    crect.color[0] = 0.0f;
    crect.color[1] = 0.0f;
    crect.color[2] = 1.0f;
    crect.color[3] = 1.0f;

    filledrect_enqueue_for_screen(&crect);
  }

  // draw bombs
  platformers_enqueue(&bombs);
  platformers_enqueue(&enemies);

  // draw platforms
  DLLNode node = platforms.head;
  while(node) {
    Platform platform = node_to_platform(node);
    struct ColoredRect_ rect;
    memcpy(&rect, &platform->rect, sizeof(struct Rect_));
    rect.color[0] = 0.0f;
    rect.color[1] = 0.8f;
    rect.color[2] = 0.0f;
    rect.color[3] = 1.0f;

    filledrect_enqueue_for_screen(&rect);
    node = node->next;
  }

  if(win_state == STATE_WIN) {
    printf("WIN\n");
    exit(0);
  }

  const float enemy_sep = player_width / 2 + enemy_dim / 2;
  Platformer ep;
  if((ep = platformer_within((Platformer)&player, enemy_sep, &enemies))) {
    printf("LOSE: %p\n", ep);
    exit(0);
  }
}

void game_shutdown() {
}
