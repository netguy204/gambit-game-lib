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
struct DLL_ enemies;
FixedAllocator enemy_allocator;

Platform platform_make() {
  return fixed_allocator_alloc(platform_allocator);
}

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
  platformer->parent = NULL;

  return particle;
}

void PlatformerObject_update(void* _self, float dt) {
  super_update(PlatformerObject, _self, dt);

  Platformer platformer = _self;
  Particle particle = _self;

  platformer_resolve(platformer, &platforms);

  if(platformer->falling) {
    particle->vel.y -= (bomb_gravity_accel * dt);
  } else {
    particle->vel.x = 0;
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
  bomb->time_remaining = bomb_delay;
  bomb->searched_neighbors = 0;
  return bomb;
}

void bomb_detonate(Bomb bomb) {
  if(bomb->time_remaining > bomb_explode_start) {
    bomb->time_remaining = bomb_explode_start;
  }
}

void BombObject_update(void* _self, float dt) {
  super_update(BombObject, _self, dt);

  Bomb bomb = _self;
  Platformer platformer = _self;
  Particle particle = _self;

  bomb->time_remaining -= dt;
  if(bomb->time_remaining <= 0) {
    delete(bomb);
  } else if(bomb->time_remaining < 0.1) {
    // contract to acheive zero at t=0
    particle->dsdt = -particle->scale / bomb->time_remaining;

    // search our neighbors to see if we can set off a chain reaction
    if(!bomb->searched_neighbors) {
      bomb->searched_neighbors = 1;
      DLLNode node = bombs.head;
      while(node) {
        Particle p = node_to_particle(node);
        if(p != particle) {
          struct Vector_ offset;
          vector_sub(&offset, &p->pos, &particle->pos);
          if(vector_mag(&offset) < platformer->w * bomb_chain_factor) {
            bomb_detonate((Bomb)p);
          }
        }
        node = node->next;
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

void EnemyObject_update(void* _self, float dt) {
  super_update(EnemyObject, _self, dt);
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
  player.charging = 0;
  player.fire_pressed = 0;
  vector_zero(&player.fire_charge);

  struct Vector_ ground_platform = {screen_width / 2, 32};
  Platform ground = new(PlatformObject, &platforms, &ground_platform);
  ground->w = screen_width;
  ground->h = 64;

  struct Vector_ test_platform = {300, 300};
  Platform platform = new(SlidingPlatformObject, &platforms, &test_platform);
  Particle pp = (Particle)platform;
  platform->w = 256;
  platform->h = 64;
  pp->vel.x = 100;

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

  platformer_resolve(&player.platformer, &platforms);

  if(state->action1 && !player.platformer.falling) {
    pp->vel.y = player_jump_speed;
  }

  if(!state->action1 && player.platformer.falling) {
    pp->vel.y = MIN(pp->vel.y, 0);
  }
}

void player_integrate(float dt) {
  Particle pp = (Particle)&player;
  if(player.platformer.falling) {
    pp->vel.y -= player_gravity_accel * dt;
  }
  particle_integrate(pp, dt);
}

void update_particles(DLL list, float dt) {
  DLLNode node = list->head;
  while(node) {
    update(node_to_particle(node), dt);
    node = node->next;
  }
}

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);

  update_particles(&platforms, dt);
  update_particles(&bombs, dt);

  player_integrate(dt);

  handle_input(state, dt);

  struct ColoredRect_ prect;
  player_rect((Rect)&prect);

  struct Vector_ player_abs;
  player_abs_pos(&player_abs);

  prect.color[0] = 1.0f;
  prect.color[1] = 0.0f;
  prect.color[2] = 1.0f;
  prect.color[3] = 1.0f;

  filledrect_enqueue_for_screen(&prect);

  if(player.charging) {
    struct ColoredRect_ crect;
    struct Vector_ cvect;
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
  DLLNode node = bombs.head;
  while(node) {
    struct ColoredRect_ brect;
    Particle particle = container_of(node, struct Particle_, node);
    Platformer platformer = (Platformer)particle;
    struct Vector_ bpos;
    platformer_abs_pos(&bpos, platformer);
    rect_centered((Rect)&brect, &bpos,
                  bomb_dim * particle->scale,
                  bomb_dim * particle->scale);
    brect.color[0] = 1.0f;
    brect.color[1] = 0.0f;
    brect.color[2] = 0.0f;
    brect.color[3] = 1.0f;

    filledrect_enqueue_for_screen(&brect);

    node = node->next;
  }

  // draw platforms
  node = platforms.head;
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
}

void game_shutdown() {
}
