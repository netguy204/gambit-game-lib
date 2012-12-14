#ifndef PLATFORM_H
#define PLATFORM_H

#include "particle.h"

// dynamic-collidable-rect
typedef struct DCR_ {
  struct Particle_ _;
  struct Rect_ rect; // cached value, computed by update
  int collision_mask;
  float w;
  float h;
} *DCR;

#define dcr_w(o) (((DCR)o)->w)
#define dcr_h(o) (((DCR)o)->h)
#define dcr_rect(o) (((DCR)o)->rect)

typedef struct Platform_ {
  struct DCR_ _;
} *Platform;

void platform_rect(Rect rect, Platform platform);
int is_supported(Rect a, Platform platform);
Platform node_to_platform(DLLNode node);

typedef struct Platformer_ {
  struct DCR_ _;
  Platform parent;
  float grav_accel;
  int falling;
} *Platformer;

Platformer node_to_platformer(DLLNode node);
void platformer_setdims(Platformer platformer, float w, float h);
void platformer_init(Platformer platformer, Vector pos, float w, float h);
void platformer_abs_pos(Vector pos, Platformer platformer);
void platformer_abs_vel(Vector vel, Platformer platformer);
void platformer_rect(Rect rect, Platformer platformer);
Platform is_platform_colliding(Rect a, DLL platforms);
void resolve_interpenetration(Vector resolution, Rect minor, Rect major);
void platformer_resolve(Platformer platformer, DLL platforms);

#endif
