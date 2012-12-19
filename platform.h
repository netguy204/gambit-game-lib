#ifndef PLATFORM_H
#define PLATFORM_H

#include "gameobject.h"
#include "rect.h"

typedef enum {
  MASK_NON_COLLIDER = 0,
  MASK_PLATFORM = 1,
  MASK_PLATFORMER = 2,
  MASK_ENEMY_PLATFORM = 4
} CollisionMask;


// registers a GO with the collidables section of the world. GO
// components will get COLLIDED messages letting them known when they
// hit other GOs that have a collidable component
typedef struct CCollidable_ {
  struct Component_ _;
  struct DLLNode_ node;
  float w;
  float h;
  int mask;
} *CCollidable;

// args: GO, w, h
const void* CCollidableObject();

CCollidable node_to_collidable(DLLNode node);
void collidable_rect(Rect rect, CCollidable coll);
int collidable_intersect(CCollidable a, CCollidable b);

typedef struct CPlatformer_ {
  struct Component_ _;
  float grav_accel;
  float max_speed;
  int platform_mask;
} *CPlatformer;

// args: GO, accel
const void* CPlatformerObject();

void world_notify_collisions(World world);

int is_supported(Rect supportee, Rect supporter);
void resolve_interpenetration(Vector resolution, Rect minor, Rect major);

#endif
