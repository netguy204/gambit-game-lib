#ifndef PLATFORM_H
#define PLATFORM_H

#include "particle.h"

typedef struct Platform_ {
  struct Particle_ particle;
  struct ColoredRect_ rect;
  struct DLLNode_ node;
} *Platform;

typedef struct Platformer_ {
  struct Particle_ particle;
  Platform parent;
  float w;
  float h;
  int falling;
} *Platformer;

Platform node_to_platform(DLLNode node);
void platformer_init(Platformer platformer, Vector pos, float w, float h);
void platformer_abs_pos(Vector pos, Platformer platformer);
void platformer_abs_vel(Vector vel, Platformer platformer);
void platformer_rect(Rect rect, Platformer platformer);
Platform is_platform_colliding(Rect a, DLL platforms);
void resolve_interpenetration(Vector resolution, Rect minor, Rect major);
int is_supported(Rect a, Platform platform);
void platformer_resolve(Platformer platformer, DLL platforms);
void platform_update_rect(Platform platform);

#endif
