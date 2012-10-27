#ifndef PARTICLE_H
#define PARTICLE_H

#include "listlib.h"
#include "vector.h"
#include "testlib.h"
#include "rect.h"
#include "spriteatlas.h"

typedef struct Particle_ {
  struct DLLNode_ node;
  struct Vector_ pos;
  struct Vector_ vel;
  SpriteAtlasEntry image;
  float scale;
  float dsdt;
  float angle;
  float dadt;
} *Particle;

float particle_width(Particle particle);
float particle_height(Particle particle);
void particle_center(Particle particle, Vector v);
Sprite particle_sprite(Particle particle);
void particle_integrate(Particle particle, float dt);
SpriteList particles_spritelist(DLL list);
void rect_for_particle(Rect rect, Particle particle, float scale);

#endif
