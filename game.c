#include "game.h"
#include "testlib.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"
#include "particle.h"
#include "rect.h"

#include <stdlib.h>
#include <math.h>

#define NUM_GAME_PARTICLES 30

float player_speed = 600;
float player_bullet_speed = 1200;
float enemy_speed = 50;

typedef struct GameParticle_ {
  struct Particle_ particle;
  struct Rect_ rect;
} *GameParticle;

GameParticle player;
struct DLL_ enemies;
struct DLL_ player_bullets;

ImageResource stars;
ImageResource image_enemy;
ImageResource image_player_bullet;

Clock main_clock;

FixedAllocator particle_allocator;

GameParticle gameparticle_make() {
  GameParticle particle = fixed_allocator_alloc(particle_allocator);
  particle->particle.scale = 1.0;
  particle->particle.angle = 0;
  return particle;
}

void gameparticle_free(GameParticle particle) {
  fixed_allocator_free(particle_allocator, particle);
}

int rand_in_range(int lower, int upper) {
  int range = upper - lower;
  return lower + (rand() % range);
}

Sprite frame_resource_sprite(ImageResource resource) {
  Sprite sprite = frame_make_sprite();
  sprite->resource = resource;
  sprite->w = resource->w;
  sprite->h = resource->h;
  return sprite;
}

GameParticle spawn_enemy() {
  GameParticle enemy = gameparticle_make();
  enemy->particle.image = image_enemy;
  enemy->particle.pos.x = screen_width + (image_enemy->w / 2);

  int nrows = ceil(screen_height / image_enemy->h);
  enemy->particle.pos.y =
    image_enemy->h * rand_in_range(0, nrows)
    - (image_enemy->h / 2);
  enemy->particle.vel.x = -rand_in_range(enemy_speed, 2*enemy_speed);
  enemy->particle.vel.y = 0;
  enemy->particle.angle = 180.0;
  return enemy;
}

GameParticle spawn_bullet(Vector pos, Vector vel, ImageResource image) {
  GameParticle bullet = gameparticle_make();
  bullet->particle.image = image;
  bullet->particle.pos = *pos;
  bullet->particle.vel = *vel;
  return bullet;
}

typedef int(*GameParticleTest)(GameParticle p);

// step all particles forward and remove those that fail the test
void gameparticles_update(DLL list, float dt, GameParticleTest test) {
  GameParticle p = (GameParticle)list->head;

  while(p != NULL) {
    particle_integrate((Particle)p, dt);
    GameParticle next = (GameParticle)p->particle.node.next;

    if(!test(p)) {
      dll_remove(list, (DLLNode)p);
      gameparticle_free(p);
    }

    p = next;
  }
}

int enemy_boundary_test(GameParticle p) {
  return p->particle.pos.x > -(particle_width((Particle)p) / 2.0f);
}

void enemies_update(float dt) {
  gameparticles_update(&enemies, dt, &enemy_boundary_test);

  if(dll_count(&enemies) < 10) {
    GameParticle enemy = spawn_enemy();
    dll_add_head(&enemies, (DLLNode)enemy);
  }
}

int player_bullet_boundary_test(GameParticle p) {
  return p->particle.pos.x < screen_width + (particle_width((Particle)p) / 2.0f);
}

void player_bullets_update(float dt) {
  gameparticles_update(&player_bullets, dt, &player_bullet_boundary_test);
}

void sprite_submit(Sprite sprite) {
  SpriteList sl = frame_spritelist_append(NULL, sprite);
  spritelist_enqueue_for_screen(sl);
}

typedef struct RepeatingLatch_ {
  long last_time;
  float period;
  int latch_value, last_state;
} *RepeatingLatch;

int repeatinglatch_state(RepeatingLatch latch, int input_state) {
  if(latch->last_state != input_state) {
    latch->last_state = input_state;
    latch->last_time = clock_time(main_clock);
    return input_state;
  } else {
    // has the clock expired?
    long dt_ticks = clock_time(main_clock) - latch->last_time;
    float dt = clock_cycles_to_seconds(dt_ticks);
    if(dt >= latch->period) {
      latch->last_time = clock_time(main_clock);
      return input_state;
    } else {
      // return the default value
      return latch->latch_value;
    }
  }
}

struct RepeatingLatch_ player_gun_latch;

void game_init() {
  particle_allocator = fixed_allocator_make(sizeof(struct GameParticle_),
                                            NUM_GAME_PARTICLES,
                                            "particle_allocator");
  main_clock = clock_make();

  stars = image_load("spacer/night-sky-stars.jpg");
  image_enemy = image_load("spacer/ship-right.png");
  image_player_bullet = image_load("spacer/plasma.png");

  dll_zero(&enemies);
  dll_zero(&player_bullets);

  player = gameparticle_make();
  player->particle.image = image_load("spacer/hero.png");
  player->particle.pos.x = player->particle.image->w;
  player->particle.pos.y = screen_height / 2;
  player->particle.vel.x = 0;
  player->particle.vel.y = 0;

  player_gun_latch.period = 0.2;
  player_gun_latch.latch_value = 0;
  player_gun_latch.last_time = 0;
  player_gun_latch.last_state = 0;
}

void handle_input(InputState state) {
  player->particle.vel.x = state->leftright * player_speed;
  player->particle.vel.y = state->updown * player_speed;
  if(repeatinglatch_state(&player_gun_latch, state->action1)) {
    struct Vector_ v = { player_bullet_speed, 0.0f };
    GameParticle bullet = spawn_bullet(&player->particle.pos, &v,
                                       image_player_bullet);
    dll_add_head(&player_bullets, (DLLNode)bullet);
  }
}

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);

  Sprite background = frame_resource_sprite(stars);
  background->displayX = 0;
  background->displayY = 0;
  sprite_submit(background);

  // read player input
  handle_input(state);

  // update player position
  particle_integrate((Particle)player, dt);

  // update enemies
  enemies_update(dt);

  // update bullets
  player_bullets_update(dt);

  // draw the enemies
  spritelist_enqueue_for_screen(particles_spritelist(&enemies));

  // draw the bullets
  spritelist_enqueue_for_screen(particles_spritelist(&player_bullets));

  // draw the player
  sprite_submit(particle_sprite((Particle)player));
}

void game_shutdown() {

}
