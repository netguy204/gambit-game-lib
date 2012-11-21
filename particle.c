#include "particle.h"
#include "testlib.h"
#include "vector.h"

float particle_width(Particle particle) {
  return particle->image->w * particle->scale;
}

float particle_height(Particle particle) {
  return particle->image->h * particle->scale;
}

void particle_center(Particle particle, Vector v) {
  v->x = particle->pos.x + particle_width(particle) / 2;
  v->y = particle->pos.y + particle_height(particle) / 2;
}

Sprite particle_sprite(Particle particle) {
  Sprite sprite = frame_make_sprite();
  sprite_fillfromentry(sprite, particle->image);

  float x = particle->pos.x - screen_x_br;
  float y = particle->pos.y - screen_y_br;
  float w = particle_width(particle);
  float h = particle_height(particle);
  if(x + w < 0 || x - w > screen_width ||
     y + h < 0 || y - h > screen_height) {
    return NULL;
  }

  sprite->w = w;
  sprite->h = h;
  sprite->displayX = x;
  sprite->displayY = y;
  sprite->originX = 0.5;
  sprite->originY = 0.5;
  sprite->angle = particle->angle;
  return sprite;
}

void particle_integrate(Particle particle, float dt) {
  vector_integrate(&particle->pos, &particle->pos, &particle->vel, dt);
  particle->scale += (particle->dsdt * dt);
  particle->angle += (particle->dadt * dt);
}

Particle node_to_particle(DLLNode node) {
  return container_of(node, struct Particle_, node);
}

SpriteList particles_spritelist(DLL list) {
  SpriteList result = NULL;
  DLLNode node = list->head;
  while(node) {
    Particle p = node_to_particle(node);
    Sprite sprite = particle_sprite(p);
    if(sprite) {
      result = frame_spritelist_append(result, sprite);
    }
    node = node->next;
  }
  return result;
}

void rect_for_particle(Rect rect, Particle particle, float scale) {
  float hw = particle_width(particle) / 2.0f;
  float hh = particle_height(particle) / 2.0f;

  rect->minx = particle->pos.x - hw;
  rect->maxx = particle->pos.x + hw;
  rect->miny = particle->pos.y - hh;
  rect->maxy = particle->pos.y + hh;
}
