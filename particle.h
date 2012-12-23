#ifndef PARTICLE_H
#define PARTICLE_H

#include "listlib.h"
#include "vector.h"
#include "testlib.h"
#include "rect.h"
#include "spriteatlas.h"
#include "ooc.h"

class Particle : public Object {
 public:
  OBJECT_PROTO(Particle);

  Particle();

  virtual void update(float dt);

  struct DLLNode_ node;
  SimpleDLL* containing_list;

  struct Vector_ pos;
  struct Vector_ vel;
  SpriteAtlasEntry image;
  float scale;
  float dsdt;
  float angle;
  float dadt;
};

typedef DLL_DECLARE(Particle, node) ParticleDLL;

float particle_width(Particle* particle);
float particle_height(Particle* particle);
void particle_center(Particle* particle, Vector v);
Sprite particle_sprite(Particle* particle);

SpriteList particles_spritelist(ParticleDLL* list);
void rect_for_particle(Rect rect, Particle* particle, float scale);

#endif
