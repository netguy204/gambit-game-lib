#ifndef PARTICLE_H
#define PARTICLE_H

#include "listlib.h"
#include "vector.h"
#include "testlib.h"

typedef struct Particle_ {
  struct DLLNode_ node;
  Vector pos;
  Vector vel;
  ImageResource image;
  float scale;
  float angle;
} *Particle;

void particles_init();
Particle particle_make();
void particle_free(Particle particle);
float particle_width(Particle particle);
float particle_height(Particle particle);
Sprite particle_sprite(Particle particle);
void particle_integrate(Particle particle, float dt);
SpriteList particles_spritelist(DLL list);

#endif
