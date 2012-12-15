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

struct World_ world;

SpriteAtlas atlas;

const void* PlayerObject;
struct PlayerState_ player;

int max_platforms = 10;
const void* PlatformObject;
FixedAllocator platform_allocator;

struct Random_ rgen;
Clock main_clock;

const void* ParticleObject;
const void* PlatformerObject;

int max_bombs = 50;
int current_n_bombs = 0;
const void* BombObject;
FixedAllocator bomb_allocator;

int max_enemies = 20;
int current_n_enemies = 0;
const void* EnemyObject;
const void* EnemyPlatformObject;
FixedAllocator enemy_allocator;

const void* BossObject;

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
  // keep our rect up-to-date
  platform_rect(&dcr_rect(platform), platform);
}

void* PlatformObject_ctor(void* _self, va_list* app) {
  Platform platform = super_ctor(PlatformObject, _self, app);
  Particle particle = (Particle)platform;

  Vector pos = va_arg(*app, Vector);
  particle->pos = *pos;
  particle->vel.x = 0;
  particle->vel.y = 0;
  dcr_mask(platform) = MASK_PLATFORM;

  return platform;
}

void* PlatformerObject_ctor(void* _self, va_list* app) {
  Platformer platformer = super_ctor(PlatformerObject, _self, app);
  Particle particle = (Particle)platformer;

  Vector pos = va_arg(*app, Vector);
  Vector vel = va_arg(*app, Vector);

  particle->pos = *pos;
  particle->vel = *vel;
  platformer->falling = 1;
  platformer->grav_accel = 0.0f;
  platformer->parent = NULL;
  dcr_mask(platformer) = MASK_PLATFORMER;
  platformer->platform_mask = MASK_PLATFORM;

  return particle;
}

void PlatformerObject_update(void* _self, float dt) {
  Platformer platformer = _self;
  Particle particle = _self;

  super_update(PlatformerObject, _self, dt);
  platformer_resolve(platformer, &world, platformer->platform_mask);

  // keep our rect up-to-date
  platformer_rect(&dcr_rect(platformer), platformer);

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
  platformer->platform_mask |= MASK_ENEMY_PLATFORM;
  current_n_bombs++;

  // do an update too so that our rect is initialized
  update(platformer, 0.0);

  return bomb;
}

void* BombObject_dtor(void* _self) {
  current_n_bombs--;
  return super_dtor(BombObject, _self);
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

int delete_all_but_player(DCR dcr, void* udata) {
  if(dcr != (DCR)&player) {
    if(isInstanceOf(BombObject, dcr)) {
      bomb_detonate((Bomb)dcr);
    } else {
      delete(dcr);
    }
  }
  return 0;
}

void BombObject_update(void* _self, float dt) {
  super_update(BombObject, _self, dt);

  Bomb bomb = _self;
  Platformer platformer = _self;
  Particle particle = _self;

  bomb->time_remaining -= dt;

  // bombs stick when they land
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
      struct Rect_ search_rect;
      rect_scaled(&search_rect, &dcr_rect(platformer), bomb_chain_factor, bomb_chain_factor);
      world_foreach(&world, &search_rect, MASK_PLATFORMER, delete_all_but_player, NULL);
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
  Particle pp = (Particle)&enemy->platform;
  platformer_abs_pos(&pp->pos, epl);
  platformer_abs_vel(&pp->vel, epl);

  dcr_w(&enemy->platform) = dcr_w(epl);
  dcr_h(&enemy->platform) = dcr_h(epl);
  dcr_rect(&enemy->platform) = dcr_rect(epl);
}

void* EnemyObject_ctor(void* _self, va_list* app) {
  Enemy enemy = super_ctor(EnemyObject, _self, app);
  Particle particle = (Particle)enemy;
  Platformer platformer = (Platformer)enemy;

  init(EnemyPlatformObject, &enemy->platform, &world.game_objects, &particle->pos);
  platformer->grav_accel = player_gravity_accel;
  dcr_w(platformer) = enemy_dim;
  dcr_h(platformer) = enemy_dim;

  EnemyObject_mirrorparticle(enemy);
  current_n_enemies++;

  return enemy;
}

void* EnemyObject_dtor(void* _self) {
  Enemy enemy = _self;
  dtor(&enemy->platform);
  current_n_enemies--;
  return super_dtor(EnemyObject, _self);
}

void* EnemyPlatformObject_ctor(void* _self, va_list* app) {
  Platform platform = super_ctor(EnemyPlatformObject, _self, app);
  dcr_mask(platform) = MASK_ENEMY_PLATFORM;
  return platform;
}

void EnemyPlatformObject_update(void* _self, float dt) {
  // slave ourselves to our parent particle and don't do our own
  // updates
  Platform platform = _self;
  Enemy enemy = container_of(platform, struct Enemy_, platform);
  Platformer epl = (Platformer)enemy;

  EnemyObject_mirrorparticle(enemy);
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
    float xd = (dcr_w(epl->parent) / 2.0) - fabs(epa->pos.x);
    float margin = 5;
    if(xd < margin) {
      float bump = margin + 1 - xd;
      epa->vel.x = -epa->vel.x;
      epa->pos.x += SIGN(epa->vel.x) * bump;
    }
  }

  update(&enemy->platform, dt);
}

void BossObject_ctor(void* _self, va_list* app) {
  Boss boss = super_ctor(BossObject, _self, app);
  boss->hp = 3;
}

void BossObject_update(void* _self, float dt) {
  super_update(BossObject, _self, dt);

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

void* PlayerObject_ctor(void* _self, va_list *app) {
  PlayerState ps = super_ctor(PlayerObject, _self, app);

  dcr_w(ps) = player_width;
  dcr_h(ps) = player_height;
  ((Platformer)ps)->grav_accel = player_gravity_accel;
  ps->charging = 0;
  ps->fire_pressed = 0;
  ps->facing = 1;
  vector_zero(&ps->fire_charge);

  return ps;
}

// lets pretend this is a dynamic variable
InputState player_input = NULL;
void PlayerObject_update(void* _self, float dt) {
  super_update(PlayerObject, _self, dt);

  PlayerState player = _self;
  Particle pp = _self;

  if(player_input->action2) {
    if(!player->fire_pressed) {
      player->fire_pressed = 1;
      player->fire_timeout = charge_delay;
    }

    player->fire_timeout -= dt;
    if(player->fire_timeout <= 0 && !player->platformer.falling) {
      player->charging = 1;
    }

    if(player->charging) {
      if(!player->platformer.falling) {
        pp->vel.x = 0;
      }
    }
  } else if(player->fire_pressed) {
    //fire
    player->fire_pressed = 0;
    player->charging = 0;

    if(current_n_bombs < max_bombs) {
      struct Vector_ abs_pos;
      platformer_abs_pos(&abs_pos, _self);

      struct Vector_ abs_vel = {player->facing * throw_speed / 3, throw_speed};

      if(player->platformer.parent) {
        Particle par_part = (Particle)player->platformer.parent;
        vector_add(&abs_vel, &abs_vel, &par_part->vel);
      }

      Bomb bomb = new(BombObject, &world.game_objects, &abs_pos, &abs_vel);
      platformer_setdims((Platformer)bomb, bomb_dim, bomb_dim);
    }
  }

  pp->vel.x = player_input->leftright * player_speed;
  if(fabs(player_input->leftright) > 0.01) {
    player->facing = SIGN(player_input->leftright);
  }

  if(player_input->action1 && !player->platformer.falling) {
    pp->vel.y = player_jump_speed;
  }

  if(!player_input->action1 && player->platformer.falling) {
    pp->vel.y = MIN(pp->vel.y, 0);
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

  ParticleObject = new(UpdateableClass(), "Particle",
                       Object, sizeof(struct Particle_),
                       ctor, ParticleObject_ctor,
                       dtor, ParticleObject_dtor,
                       update, ParticleObject_update,
                       0);

  PlatformObject = new(UpdateableClass(), "Platform",
                       ParticleObject, sizeof(struct Platform_),
                       alloci, PlatformObject_alloci,
                       dealloci, PlatformObject_dealloci,
                       ctor, PlatformObject_ctor,
                       update, PlatformObject_update,
                       0);

  EnemyPlatformObject = new(UpdateableClass(), "EnemyPlatform",
                            PlatformObject, sizeof(struct Platform_),
                            ctor, EnemyPlatformObject_ctor,
                            update, EnemyPlatformObject_update,
                            0);

  SlidingPlatformObject = new(UpdateableClass(), "SlidingPlatform",
                              PlatformObject, sizeof(struct Platform_),
                              update, SlidingPlatformObject_update,
                              0);

  PlatformerObject = new(UpdateableClass(), "Platformer",
                         ParticleObject, sizeof(struct Platformer_),
                         ctor, PlatformerObject_ctor,
                         update, PlatformerObject_update,
                         0);

  BombObject = new(UpdateableClass(), "Bomb",
                   PlatformerObject, sizeof(struct Bomb_),
                   alloci, BombObject_alloci,
                   dealloci, BombObject_dealloci,
                   ctor, BombObject_ctor,
                   dtor, BombObject_dtor,
                   update, BombObject_update,
                   0);

  EnemyObject = new(UpdateableClass(), "Enemy",
                    PlatformerObject, sizeof(struct Enemy_),
                    alloci, EnemyObject_alloci,
                    dealloci, EnemyObject_dealloci,
                    ctor, EnemyObject_ctor,
                    dtor, EnemyObject_dtor,
                    update, EnemyObject_update,
                    0);

  PlayerObject = new(UpdateableClass(), "Player",
                     PlatformerObject, sizeof(struct PlayerState_),
                     ctor, PlayerObject_ctor,
                     update, PlayerObject_update,
                     0);

  player_gravity_accel = (player_jump_speed * player_jump_speed) / (2 * player_jump_height);
  bomb_gravity_accel = (throw_speed * throw_speed) / (2 * bomb_max_height);

  atlas = spriteatlas_load("resources/images_default.dat", "resources/images_default.png");
}

void game_init() {
  game_support_init();

  struct Vector_ player_pos = {100.0f, 100.0f};
  struct Vector_ player_vel = {0.0f, 0.0f};
  init(PlayerObject, &player, NULL, &player_pos, &player_vel);

  struct Vector_ ground_platform = {screen_width / 2, 32};
  Platform ground = new(PlatformObject, &world.game_objects, &ground_platform);
  dcr_w(ground) = screen_width;
  dcr_h(ground) = 64;

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
  Platform platform = new(SlidingPlatformObject, &world.game_objects, &test_platform);
  Particle pp = (Particle)platform;
  dcr_w(platform) = 256;
  dcr_h(platform) = 64;
  pp->vel.x = 100;

  struct Vector_ test_platform2 = {600, 600};
  Platform platform2 = new(SlidingPlatformObject, &world.game_objects, &test_platform2);
  Particle pp2 = (Particle)platform2;
  dcr_w(platform2) = 256;
  dcr_h(platform2) = 64;
  pp2->vel.x = 100;

  set_game_step(game_step);
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

void platformers_enqueue(DLL list, float r, float g, float b) {
  DLLNode node = list->head;
  while(node) {
    Platformer platformer = node_to_platformer(node);
    platformer_enqueue(platformer, r, g, b);
    node = node->next;
  }
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
    struct Rect_ screen_rect = {0, 0, screen_width, screen_height};
    world_foreach(&world, &screen_rect, MASK_PLATFORMER, delete_all_but_player, NULL);
    set_game_step(game_step);
  }
}

int enemy_kills_player_callback(DCR dcr, void* udata) {
  if(isInstanceOf(EnemyObject, dcr)) {
    Particle pp = (Particle)&player;
    pp->pos.x = 100;
    pp->pos.y = 100;
    endgame_timeout = endgame_delay;
    win_state = STATE_LOSE;
    set_game_step(game_end);
    return 1;
  } else {
    return 0;
  }
}

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);
  enemy_timer -= dt;
  if(enemy_timer <= 0 && current_n_enemies < max_enemies) {
    if(win_state == STATE_START) {
      win_state = STATE_PLAY;
    }

    enemy_timer = enemy_period;
    struct Vector_ epos = {screen_width/2, screen_height};
    struct Vector_ evel = {0, 0};
    Enemy enemy = new(EnemyObject, &world.game_objects, &epos, &evel);
  }

  if(win_state == STATE_PLAY && current_n_enemies == 0) {
    win_state = STATE_WIN;
    endgame_timeout = endgame_delay;
    set_game_step(game_end);
  }

  update_particles(&world.game_objects, dt);

  player_input = state;
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
  DLLNode node = world.game_objects.head;
  while(node) {
    struct ColoredRect_ rect;
    Particle particle = node_to_particle(node);
    DCR dcr = (DCR)particle;
    memcpy(&rect, &dcr_rect(dcr), sizeof(struct Rect_));
    rect_scaled((Rect)&rect, (Rect)&rect, particle->scale, particle->scale);

    rect.color[3] = 1.0f;
    int skip = 0;

    if(isInstanceOf(BombObject, dcr)) {
      rect.color[0] = 1.0;
      rect.color[1] = 0.0;
      rect.color[2] = 0.0;
    } else if(isInstanceOf(EnemyObject, dcr)) {
      rect.color[0] = 1.0;
      rect.color[1] = 1.0;
      rect.color[2] = 0.0;
    } else if(isInstanceOf(EnemyPlatformObject, dcr)) {
      skip = 1;
    } else if(isInstanceOf(PlatformObject, dcr)) {
      rect.color[0] = 0.0;
      rect.color[1] = 0.8;
      rect.color[2] = 0.0;
    } else {
      skip = 1;
    }

    if(!skip) {
      filledrect_enqueue_for_screen(&rect);
    }

    node = node->next;
  }

  world_foreach(&world, &dcr_rect(&player), MASK_PLATFORMER, enemy_kills_player_callback, NULL);
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
