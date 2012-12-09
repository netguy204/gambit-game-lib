#ifndef PLATFORM_H
#define PLATFORM_H

#include "particle.h"

typedef struct Platform_ {
  struct Particle_ particle;
  struct Rect_ rect; // computed by update
  float w;
  float h;
} *Platform;

void platform_rect(Rect rect, Platform platform);
int is_supported(Rect a, Platform platform);
Platform node_to_platform(DLLNode node);

typedef struct Platformer_ {
  struct Particle_ particle;
  Platform parent;
  float grav_accel;
  float w;
  float h;
  int falling;
} *Platformer;

void platformer_setdims(Platformer platformer, float w, float h);
void platformer_init(Platformer platformer, Vector pos, float w, float h);
void platformer_abs_pos(Vector pos, Platformer platformer);
void platformer_abs_vel(Vector vel, Platformer platformer);
void platformer_rect(Rect rect, Platformer platformer);
Platform is_platform_colliding(Rect a, DLL platforms);
void resolve_interpenetration(Vector resolution, Rect minor, Rect major);
void platformer_resolve(Platformer platformer, DLL platforms);

#endif
