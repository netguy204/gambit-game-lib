#include "game.h"
#include "testlib.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"
#include "particle.h"

#include <stdlib.h>
#include <math.h>

float player_speed = 600;
float enemy_speed = 50;


Particle player;
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

Particle spawn_enemy() {
  Particle enemy = particle_make();
  enemy->image = image_enemy;
  enemy->pos.x = screen_width + (image_enemy->w / 2);

  int nrows = ceil(screen_height / image_enemy->h);
  enemy->pos.y =
    image_enemy->h * rand_in_range(0, nrows)
    - (image_enemy->h / 2);
  enemy->vel.x = -rand_in_range(enemy_speed, 2*enemy_speed);
  enemy->vel.y = 0;
  enemy->angle = 180.0;
  return enemy;
}

void enemies_update(float dt) {
  Particle p = (Particle)enemies.head;

  // step all enemies forward and remove those that are offscreen
  while(p != NULL) {
    particle_integrate(p, dt);
    Particle next = (Particle)p->node.next;

    if(p->pos.x < -(particle_width(p) / 2)) {
      dll_remove(&enemies, (DLLNode)p);
      particle_free(p);
    }

    p = next;
  }

  if(dll_count(&enemies) < 10) {
    Particle enemy = spawn_enemy();
    dll_add_head(&enemies, (DLLNode)enemy);
  }
}

void sprite_submit(Sprite sprite) {
  SpriteList sl = frame_spritelist_append(NULL, sprite);
  spritelist_enqueue_for_screen(sl);
}

void game_init() {
  particles_init();
  main_clock = clock_make();

  stars = image_load("spacer/night-sky-stars.jpg");
  image_enemy = image_load("spacer/ship-right.png");

  enemies.head = NULL;
  enemies.tail = NULL;

  player = particle_make();
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
  particle_integrate(player, dt);

  // update enemies
  enemies_update(dt);

  // draw the enemies
  spritelist_enqueue_for_screen(particles_spritelist(&enemies));

  // draw the player
  sprite_submit(particle_sprite(player));
}

void game_shutdown() {

}
