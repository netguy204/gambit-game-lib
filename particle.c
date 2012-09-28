#include "particle.h"
#include "testlib.h"
#include "vector.h"

#define NUM_GAME_PARTICLES 30

FixedAllocator particle_allocator;

void particles_init() {
  particle_allocator = fixed_allocator_make(sizeof(struct Particle_),
                                            NUM_GAME_PARTICLES,
                                            "particle_allocator");
}

Particle particle_make() {
  Particle particle = fixed_allocator_alloc(particle_allocator);
  particle->scale = 1.0;
  particle->angle = 0;
  return particle;
}

void particle_free(Particle particle) {
  fixed_allocator_free(particle_allocator, particle);
}

float particle_width(Particle particle) {
  return particle->image->w;
}

float particle_height(Particle particle) {
  return particle->image->h;
}

Sprite particle_sprite(Particle particle) {
  Sprite sprite = frame_make_sprite();
  sprite->resource = particle->image;
  sprite->w = particle_width(particle);
  sprite->h = particle_height(particle);
  sprite->displayX = particle->pos.x;
  sprite->displayY = particle->pos.y;
  sprite->originX = 0.5;
  sprite->originY = 0.5;
  sprite->angle = particle->angle;
  return sprite;
}

void particle_integrate(Particle particle, float dt) {
  vector_integrate(&particle->pos, &particle->pos, &particle->vel, dt);
}

SpriteList particles_spritelist(DLL list) {
  SpriteList result = NULL;
  Particle p = (Particle)list->head;
  while(p) {
    result = frame_spritelist_append(result, particle_sprite(p));
    p = (Particle)p->node.next;
  }
  return result;
}
