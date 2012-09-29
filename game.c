#include "game.h"
#include "testlib.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"
#include "particle.h"
#include "rect.h"
#include "controls.h"
#include "config.h"

#include <stdlib.h>
#include <math.h>

float player_speed = 600;
float player_bullet_speed = 1200;
float enemy_speed = 50;

GameParticle player;
struct RepeatingLatch_ player_gun_latch;

struct DLL_ enemies;
struct DLL_ player_bullets;
struct DLL_ pretty_particles;

ImageResource stars;
ImageResource image_enemy;
ImageResource image_player_bullet;
ImageResource image_smoke;

Clock main_clock;

FixedAllocator gameparticle_allocator;
FixedAllocator prettyparticle_allocator;


GameParticle gameparticle_make() {
  GameParticle particle = fixed_allocator_alloc(gameparticle_allocator);
  particle->particle.scale = 1.0f;
  particle->particle.angle = 0.0f;
  particle->particle.dsdt = 0.0f;
  particle->particle.dadt = 0.0f;
  return particle;
}

void gameparticle_free(GameParticle particle) {
  fixed_allocator_free(gameparticle_allocator, particle);
}

void gameparticle_remove(DLL list, GameParticle particle) {
  dll_remove(list, (DLLNode)particle);
  gameparticle_free(particle);
}

PrettyParticle prettyparticle_make() {
  PrettyParticle p = fixed_allocator_alloc(prettyparticle_allocator);
  p->particle.scale = 1.0f;
  p->particle.angle = 0.0f;
  p->particle.dsdt = 0.0f;
  p->particle.dadt = 0.0f;
  return p;
}

void prettyparticle_free(PrettyParticle particle) {
  fixed_allocator_free(prettyparticle_allocator, particle);
}

void prettyparticle_remove(DLL list, PrettyParticle particle) {
  dll_remove(list, (DLLNode)particle);
  prettyparticle_free(particle);
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

  int nrows = floor(screen_height / image_enemy->h);
  enemy->particle.pos.y =
    image_enemy->h * rand_in_range(0, nrows)
    + (image_enemy->h / 2);
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
  bullet->particle.scale = 0.25f;
  bullet->particle.dsdt = 0.5;
  bullet->particle.angle = rand_in_range(0, 360);
  bullet->particle.dadt = 500;
  return bullet;
}

PrettyParticle spawn_smoke(Vector pos, Vector vel) {
  PrettyParticle smoke = prettyparticle_make();
  smoke->particle.image = image_smoke;
  smoke->particle.pos = *pos;
  smoke->particle.vel = *vel;
  smoke->particle.angle = rand_in_range(0, 360);
  smoke->particle.dsdt = 0.5f * rand_in_range(1, 4);
  smoke->particle.dadt = rand_in_range(-20, 20);
  smoke->end_time =
    clock_time(main_clock) +
    clock_seconds_to_cycles(rand_in_range(500, 3500) / 1000.0f);

  return smoke;
}

typedef int(*ParticleTest)(Particle p);
typedef void(*ParticleRemove)(DLL, Particle);

// step all particles forward and remove those that fail the test
void gameparticles_update(DLL list, float dt,
                          ParticleTest test, ParticleRemove remove) {
  Particle p = (Particle)list->head;

  while(p != NULL) {
    particle_integrate(p, dt);
    Particle next = (Particle)p->node.next;

    if(!test(p)) {
      remove(list, p);
    }

    p = next;
  }
}

int enemy_boundary_test(Particle p) {
  return p->pos.x > -(particle_width(p) / 2.0f);
}

void enemies_update(float dt) {
  gameparticles_update(&enemies, dt, &enemy_boundary_test,
                       (ParticleRemove)&gameparticle_remove);

  if(dll_count(&enemies) < 10) {
    GameParticle enemy = spawn_enemy();
    dll_add_head(&enemies, (DLLNode)enemy);
  }
}

int player_bullet_boundary_test(Particle p) {
  return p->pos.x < screen_width + (particle_width(p) / 2.0f);
}

void player_bullets_update(float dt) {
  gameparticles_update(&player_bullets, dt, &player_bullet_boundary_test,
                       (ParticleRemove)&gameparticle_remove);
}

int particle_timeout_test(Particle p) {
  PrettyParticle smoke = (PrettyParticle)p;
  return clock_time(main_clock) < smoke->end_time;
}

void prettyparticles_update(float dt) {
  gameparticles_update(&pretty_particles, dt, &particle_timeout_test,
                       (ParticleRemove)&prettyparticle_remove);
}

void collide_arrays(CollisionRecord as, int na, CollisionRecord bs, int nb,
                    OnCollision on_collision, void * udata) {
  int ia;
  int ib;

  for(ia = 0; ia < na; ++ia) {
    CollisionRecord a = &as[ia];
    if(a->skip) continue;

    for(ib = 0; ib < nb; ++ib) {
      CollisionRecord b = &bs[ib];
      if(a->skip || b->skip) break;

      if(rect_intersect(&a->rect, &b->rect)) {
        on_collision(a, b, udata);
      }
    }
  }
}

CollisionRecord particles_collisionrecords(DLL list, int* count, float scale) {
  *count = dll_count(list);
  CollisionRecord crs = frame_alloc(sizeof(struct CollisionRecord_) *
                                    *count);
  DLLNode node = list->head;
  int ii = 0;
  while(node) {
    rect_for_particle(&(crs[ii].rect), (Particle)node, scale);
    crs[ii].data = node;
    crs[ii].skip = 0;
    node = node->next;
    ++ii;
  }

  return crs;
}

void bullet_vs_enemy(CollisionRecord bullet, CollisionRecord enemy, void * udata) {
  // add a particle
  GameParticle ep = (GameParticle)enemy->data;
  PrettyParticle smoke = spawn_smoke(&ep->particle.pos, &ep->particle.vel);
  dll_add_head(&pretty_particles, (DLLNode)smoke);

  gameparticle_remove(&player_bullets, bullet->data);
  gameparticle_remove(&enemies, enemy->data);
  SAFETY(bullet->data = NULL);
  SAFETY(enemy->data = NULL);
  bullet->skip = 1;
  enemy->skip = 1;
}

void check_collisions() {
  int num_bullets;
  int num_enemies;
  CollisionRecord pbs =
    particles_collisionrecords(&player_bullets, &num_bullets, 0.7f);
  CollisionRecord es =
    particles_collisionrecords(&enemies, &num_enemies, 0.8f);

  collide_arrays(pbs, num_bullets, es, num_enemies, &bullet_vs_enemy, NULL);
}

void sprite_submit(Sprite sprite) {
  SpriteList sl = frame_spritelist_append(NULL, sprite);
  spritelist_enqueue_for_screen(sl);
}

void game_init() {
  gameparticle_allocator =
    fixed_allocator_make(sizeof(struct GameParticle_),
                         MAX_NUM_GAMEPARTICLES,
                         "gameparticle_allocator");

  prettyparticle_allocator =
    fixed_allocator_make(sizeof(struct PrettyParticle_),
                         MAX_NUM_PRETTYPARTICLES,
                         "prettyparticle_allocator");

  main_clock = clock_make();

  stars = image_load("spacer/night-sky-stars.jpg");
  image_enemy = image_load("spacer/ship-right.png");
  image_player_bullet = image_load("spacer/plasma.png");
  image_smoke = image_load("spacer/smoke.png");

  dll_zero(&enemies);
  dll_zero(&player_bullets);
  dll_zero(&pretty_particles);

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
  if(repeatinglatch_state(&player_gun_latch, main_clock, state->action1)) {
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
  background->w = screen_width;
  background->h = screen_height;
  sprite_submit(background);

  // read player input
  handle_input(state);

  // update player position
  particle_integrate((Particle)player, dt);

  // update enemies
  enemies_update(dt);

  // update bullets
  player_bullets_update(dt);

  // update particles
  prettyparticles_update(dt);

  // collision detection
  check_collisions();

  // draw the particles
  spritelist_enqueue_for_screen(particles_spritelist(&pretty_particles));

  // draw the enemies
  spritelist_enqueue_for_screen(particles_spritelist(&enemies));

  // draw the bullets
  spritelist_enqueue_for_screen(particles_spritelist(&player_bullets));

  // draw the player
  sprite_submit(particle_sprite((Particle)player));
}

void game_shutdown() {

}
