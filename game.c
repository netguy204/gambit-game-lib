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

int max_bombs = 50;
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

struct PlayerState_ player;
struct Platform_ platforms[2];

struct Random_ rgen;
Clock main_clock;

const void* BombObject;
struct DLL_ bombs;
FixedAllocator bomb_allocator;

void platformer_init(Platformer platformer, Vector pos, float w, float h) {
  Particle particle = (Particle)platformer;
  particle->pos = *pos;
  particle->vel.x = 0.0f;
  particle->vel.y = 0.0f;
  particle->image = NULL;
  particle->scale = 1.0f;
  particle->dsdt = 0.0f;
  particle->angle = 0.0f;
  particle->dadt = 0.0f;

  platformer->w = w;
  platformer->h = h;
  platformer->parent = NULL;
  platformer->falling = 1;
}

void platformer_abs_pos(Vector pos, Platformer platformer) {
  if(platformer->parent) {
    vector_add(pos, &platformer->parent->particle.pos, &platformer->particle.pos);
  } else {
    *pos = platformer->particle.pos;
  }
}

void platformer_rect(Rect rect, Platformer platformer) {
  struct Vector_ pos;
  platformer_abs_pos(&pos, platformer);
  rect_centered(rect, &pos, platformer->w, platformer->h);
}

Platform is_platform_colliding(Rect a) {
  int ii;
  for(ii = 0; ii < array_size(platforms); ++ii) {
    Platform platform = &platforms[ii];
    if(rect_intersect(a, (Rect)&platform->rect)) {
      return platform;
    }
  }
  return NULL;
}

// assumes a collision has already been found
void resolve_interpenetration(Vector resolution, Rect minor, Rect major) {
  float xint =
    MIN(minor->maxx, major->maxx) -
    MAX(minor->minx, major->minx);

  float yint =
    MIN(minor->maxy, major->maxy) -
    MAX(minor->miny, major->miny);

  struct Vector_ vmajor;
  struct Vector_ vminor;
  rect_center(&vmajor, major);
  rect_center(&vminor, minor);

  struct Vector_ to_major;
  vector_sub(&to_major, &vmajor, &vminor);
  float nudge = 0.1;

  if(yint > xint) {
    // resolve x penetration
    resolution->y = 0.0f;
    if(to_major.x > 0.0f) {
      resolution->x = -xint - nudge;
    } else {
      resolution->x = xint + nudge;
    }
  } else {
    // resolve y penetration
    resolution->x = 0.0f;
    if(to_major.y > 0.0f) {
      resolution->y = -yint;
    } else {
      resolution->y = yint;
    }
  }
}

int is_supported(Rect a, Platform platform) {
  struct Rect_ shifted = {a->minx + 0.5, a->miny - 0.5, a->maxx - 0.5, a->miny};
  if(rect_intersect(&shifted, (Rect)&platform->rect)) {
    return 1;
  } else {
    return 0;
  }
}

void platformer_resolve(Platformer platformer) {
  struct Rect_ prect;
  platformer_rect(&prect, platformer);

  if(!platformer->falling && (!platformer->parent ||
                              !is_supported(&prect, platformer->parent))) {
    platformer->falling = 1;
    platformer_abs_pos(&platformer->particle.pos, platformer);
    platformer->parent = NULL;
  }

  if(platformer->falling) {
    Platform platform;
    if((platform = is_platform_colliding(&prect))) {
      struct Vector_ resolution;
      resolve_interpenetration(&resolution, &prect, (Rect)&platform->rect);
      vector_add(&platformer->particle.pos, &platformer->particle.pos, &resolution);

      if(fabs(resolution.x) > 0.0f) {
        // bumped it, resolve the colision and remove our x component
        platformer->particle.vel.x = 0.0f;
      } else {
        platformer->particle.vel.y = 0.0f;
      }

      platformer_rect(&prect, platformer);
      if(is_supported(&prect, platform)) {
        platformer->falling = 0;
        struct Vector_ abs_pos;
        platformer_abs_pos(&abs_pos, platformer);
        vector_sub(&platformer->particle.pos, &abs_pos, &platform->particle.pos);
        platformer->parent = platform;
      }
    }
  }
}

void player_abs_pos(Vector pos) {
  platformer_abs_pos(pos, &player.platformer);
}

void player_rect(Rect rect) {
  platformer_rect(rect, &player.platformer);
}

void* BombObject_alloci(const void* _class) {
  return fixed_allocator_alloc(bomb_allocator);
}

void* BombObject_ctor(void* _self, va_list* app) {
  Bomb bomb = super_ctor(BombObject, _self, app);
  Particle particle = (Particle)bomb;

  DLL list = va_arg(*app, DLL);
  Vector pos = va_arg(*app, Vector);
  Vector vel = va_arg(*app, Vector);

  platformer_init((Platformer)bomb, pos, bomb_dim, bomb_dim);

  particle->containing_list = list;

  if(particle->containing_list) {
    dll_add_head(particle->containing_list, &particle->node);
  }

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
  particle->scale = 1.0f;
  particle->angle = 0.0f;
  particle->dsdt = 0.0f;
  particle->dadt = 0.0f;
  bomb->time_remaining = bomb_delay;

  return bomb;
}

void* BombObject_dtor(void* _self) {
  Particle particle = _self;
  if(particle->containing_list) {
    dll_remove(particle->containing_list, &particle->node);
  }
  return super_dtor(BombObject, _self);
}

void BombObject_dealloci(void* _self) {
  fixed_allocator_free(bomb_allocator, _self);
}

void BombObject_update(void* _self, float dt) {
  Bomb bomb = _self;
  Platformer platformer = _self;
  Particle particle = _self;

  platformer_resolve(platformer);

  if(platformer->falling) {
    particle->vel.y -= (bomb_gravity_accel * dt);
  } else {
    particle->vel.x = 0;
  }

  bomb->time_remaining -= dt;
  if(bomb->time_remaining <= 0) {
    delete(bomb);
  } else if(bomb->time_remaining < 0.1) {
    // contract to acheive zero at t=0
    particle->dsdt = -particle->scale / bomb->time_remaining;
  } else if(bomb->time_remaining < 0.3) {
    // expand to acheive 2x at t = 0.1
    particle->dsdt = 2.0 / (bomb->time_remaining - 0.1);
  }

  particle_integrate(particle, dt);
}

void game_step(long delta, InputState state);

void platform_init(Platform platform, Vector pos, float w, float h) {
  platform->particle.pos = *pos;
  platform->particle.vel.x = 0;
  platform->particle.vel.y = 0;
  rect_centered((Rect)&platform->rect, &platform->particle.pos, w, h);
  platform->rect.color[0] = 0.0;
  platform->rect.color[1] = 0.8;
  platform->rect.color[2] = 0.0;
  platform->rect.color[3] = 1.0;
}

void game_init() {
  main_clock = clock_make();

  struct Vector_ player_start = {100.0f, 100.0f};
  platformer_init(&player.platformer, &player_start, player_width, player_height);
  player.charging = 0;
  player.fire_pressed = 0;

  struct Vector_ ground_platform = {screen_width / 2, 32};
  platform_init(&platforms[0], &ground_platform, screen_width, 64);

  struct Vector_ test_platform = {300, 300};
  platform_init(&platforms[1], &test_platform, 256, 64);

  vector_zero(&player.fire_charge);
  player_gravity_accel = (player_jump_speed * player_jump_speed) / (2 * player_jump_height);
  bomb_gravity_accel = (charge_max * charge_max) / (2 * bomb_max_height);

  bomb_allocator = fixed_allocator_make(sizeof(struct Bomb_), max_bombs,
                                        "bomb_allocator");

  updateable_init();
  BombObject = new(UpdateableClass, "Bomb",
                   Object, sizeof(struct Bomb_),
                   alloci, BombObject_alloci,
                   dealloci, BombObject_dealloci,
                   ctor, BombObject_ctor,
                   dtor, BombObject_dtor,
                   update, BombObject_update,
                   0);

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
      Bomb bomb = new(BombObject, &bombs, &abs_pos,
                      &player.fire_charge);
    }
  } else {
    pp->vel.x = state->leftright * player_speed;
  }

  platformer_resolve(&player.platformer);

  if(state->action1 && !player.platformer.falling) {
    player.platformer.falling = 1;
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

  player_integrate(dt);
  update_particles(&bombs, dt);

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
  int ii;
  for(ii = 0; ii < array_size(platforms); ++ii) {
    filledrect_enqueue_for_screen(&platforms[ii].rect);
  }
}

void game_shutdown() {
}
