#include "particle.h"
#include "testlib.h"
#include "vector.h"

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

void rect_for_particle(Rect rect, Particle particle, float scale) {
  float hw = (particle_width(particle) * scale) / 2.0f;
  float hh = (particle_height(particle) * scale) / 2.0f;

  rect->minx = particle->pos.x - hw;
  rect->maxx = particle->pos.x + hw;
  rect->miny = particle->pos.y - hh;
  rect->maxy = particle->pos.y + hh;
}
