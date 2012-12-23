#include "particle.h"
#include "testlib.h"
#include "vector.h"

OBJECT_IMPL(Particle);

Particle::Particle() {
  containing_list = NULL;
  vector_zero(&this->pos);
  vector_zero(&this->vel);
  this->image = NULL;
  this->scale = 1.0f;
  this->dsdt = 0.0f;
  this->angle = 0.0f;
  this->dadt = 0.0f;
}

void Particle::update(float dt) {
  vector_integrate(&this->pos, &this->pos, &this->vel, dt);
  this->scale += (this->dsdt * dt);
  this->angle += (this->dadt * dt);
}

float particle_width(Particle* particle) {
  return particle->image->w * particle->scale;
}

float particle_height(Particle* particle) {
  return particle->image->h * particle->scale;
}

void particle_center(Particle* particle, Vector v) {
  v->x = particle->pos.x + particle_width(particle) / 2;
  v->y = particle->pos.y + particle_height(particle) / 2;
}

Sprite particle_sprite(Particle* particle) {
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

SpriteList particles_spritelist(ParticleDLL* list) {
  SpriteList result = NULL;
  DLLNode node = list->head;
  while(node) {
    Particle* p = list->to_element(node);
    Sprite sprite = particle_sprite(p);
    if(sprite) {
      result = frame_spritelist_append(result, sprite);
    }
    node = node->next;
  }
  return result;
}

void rect_for_particle(Rect rect, Particle* particle, float scale) {
  float hw = particle_width(particle) / 2.0f;
  float hh = particle_height(particle) / 2.0f;

  rect->minx = particle->pos.x - hw;
  rect->maxx = particle->pos.x + hw;
  rect->miny = particle->pos.y - hh;
  rect->maxy = particle->pos.y + hh;
}
