#include "game.h"
#include "testlib.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"

#include <stdlib.h>
#include <math.h>

#define NUM_GAME_PARTICLES 30

float player_speed = 600;
float enemy_speed = 50;

typedef struct GameParticle_ {
  struct DLLNode_ node;
  Vector pos;
  Vector vel;
  ImageResource image;
  float scale;
  float angle;
} *GameParticle;

FixedAllocator particle_allocator;

GameParticle player;
struct DLL_ enemies;

ImageResource stars;
ImageResource image_enemy;

Clock main_clock;

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

GameParticle gameparticle_make() {
  GameParticle particle = fixed_allocator_alloc(particle_allocator);
  particle->scale = 1.0;
  particle->angle = 0;
  return particle;
}

void gameparticle_free(GameParticle particle) {
  fixed_allocator_free(particle_allocator, particle);
}

float gameparticle_width(GameParticle particle) {
  return particle->image->w;
}

float gameparticle_height(GameParticle particle) {
  return particle->image->h;
}

Sprite gameparticle_sprite(GameParticle particle) {
  Sprite sprite = frame_make_sprite();
  sprite->resource = particle->image;
  sprite->w = gameparticle_width(particle);
  sprite->h = gameparticle_height(particle);
  sprite->displayX = particle->pos.x;
  sprite->displayY = particle->pos.y;
  sprite->originX = 0.5;
  sprite->originY = 0.5;
  sprite->angle = particle->angle;
  return sprite;
}

void gameparticle_integrate(GameParticle particle, float dt) {
  vector_integrate(&particle->pos, &particle->pos, &particle->vel, dt);
}

SpriteList gameparticles_spritelist(DLL list) {
  SpriteList result = NULL;
  GameParticle p = (GameParticle)list->head;
  while(p) {
    result = frame_spritelist_append(result, gameparticle_sprite(p));
    p = (GameParticle)p->node.next;
  }
  return result;
}

GameParticle spawn_enemy() {
  GameParticle enemy = gameparticle_make();
  enemy->image = image_enemy;
  enemy->pos.x = screen_width + (image_enemy->w / 2);

  int nrows = ceil(screen_height / image_enemy->h);
  enemy->pos.y =
    image_enemy->h * rand_in_range(0, nrows)
    - (image_enemy->h / 2);
  enemy->vel.x = -rand_in_range(enemy_speed, 2*enemy_speed);
  enemy->vel.y = 0;
  enemy->angle = 180.0;
  dll_add_head(&enemies, (DLLNode)enemy);
  return enemy;
}

void enemies_update(float dt) {
  GameParticle p = (GameParticle)enemies.head;

  // step all enemies forward and remove those that are offscreen
  while(p != NULL) {
    gameparticle_integrate(p, dt);
    GameParticle next = (GameParticle)p->node.next;

    if(p->pos.x < -(gameparticle_width(p) / 2)) {
      dll_remove(&enemies, (DLLNode)p);
      gameparticle_free(p);
    }

    p = next;
  }

  if(dll_count(&enemies) < 10) {
    spawn_enemy();
  }
}

void sprite_submit(Sprite sprite) {
  SpriteList sl = frame_spritelist_append(NULL, sprite);
  spritelist_enqueue_for_screen(sl);
}

void game_init() {
  particle_allocator = fixed_allocator_make(sizeof(struct GameParticle_),
                                            NUM_GAME_PARTICLES,
                                            "particle_allocator");
  main_clock = clock_make();

  stars = image_load("spacer/night-sky-stars.jpg");
  image_enemy = image_load("spacer/ship-right.png");

  enemies.head = NULL;
  enemies.tail = NULL;

  player = gameparticle_make();
  player->image = image_load("spacer/hero.png");
  player->pos.x = player->image->w;
  player->pos.y = screen_height / 2;
  player->vel.x = 0;
  player->vel.y = 0;
}

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);

  Sprite background = frame_resource_sprite(stars);
  background->displayX = 0;
  background->displayY = 0;
  sprite_submit(background);

  // read player input
  player->vel.x = state->leftright * player_speed;
  player->vel.y = state->updown * player_speed;

  // update player position
  gameparticle_integrate(player, dt);

  // update enemies
  enemies_update(dt);

  // draw the enemies
  spritelist_enqueue_for_screen(gameparticles_spritelist(&enemies));

  // draw the player
  sprite_submit(gameparticle_sprite(player));
}

void game_shutdown() {

}
