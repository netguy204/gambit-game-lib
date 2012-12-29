#include "platform.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

int rect_is_supported(Rect a, Rect b) {
  struct Rect_ shifted = {a->minx + 0.5f, a->miny - 0.5f, a->maxx - 0.5f, a->miny};
  if(rect_intersect(&shifted, b)) {
    return 1;
  } else {
    return 0;
  }
}

// assumes a collision has already been found
void rect_resolve_interpenetration(Vector resolution, Rect minor, Rect major) {
  float xint =
    MIN(minor->maxx, major->maxx) -
    MAX(minor->minx, major->minx);

  float yint =
    MIN(minor->maxy, major->maxy) -
    MAX(minor->miny, major->miny);

  struct Vector_ vmajor;
  struct Vector_ vminor;
  rect_center(&vmajor, major);
  rect_center(&vminor, minor);

  struct Vector_ to_major;
  vector_sub(&to_major, &vmajor, &vminor);
  float nudge = 0.1;

  if(yint > xint) {
    // resolve x penetration
    resolution->y = 0.0f;
    if(to_major.x > 0.0f) {
      resolution->x = -xint - nudge;
    } else {
      resolution->x = xint + nudge;
    }
  } else {
    // resolve y penetration
    resolution->x = 0.0f;
    if(to_major.y > 0.0f) {
      resolution->y = -yint - nudge;
    } else {
      resolution->y = yint + nudge;
    }
  }
}

OBJECT_IMPL(CPlatformer, Component);
OBJECT_PROPERTY(CPlatformer, grav_accel);
OBJECT_PROPERTY(CPlatformer, max_speed);
OBJECT_PROPERTY(CPlatformer, platform_mask);

CPlatformer::CPlatformer(void* go)
  : Component((GO*)go, PRIORITY_THINK), grav_accel(0), platform_mask(MASK_PLATFORMER) {
  this->max_speed = 64 / .1;
}

int CPlatformer::is_supported() {
  GO* go2 = go->transform_parent;
  if(!go2) return 0;

  CCollidable* c1 = (CCollidable*)go->find_component(&CCollidable::Type);
  if(!c1) return 0;

  CCollidable* c2 = (CCollidable*)go2->find_component(&CCollidable::Type);

  struct Rect_ r1, r2;
  c1->rect(&r1);
  c2->rect(&r2);

  return rect_is_supported(&r1, &r2);
}

void CPlatformer::resolve_interpenetration() {
  // look for a collision message
  go->inbox.foreach([this](Message* message) -> int {
      if(message->kind == MESSAGE_COLLIDING) {
        // is it a kind of collidable we can stick to?
        CCollidable* cself = (CCollidable*)message->data2;
        CCollidable* cother = (CCollidable*)message->data;

        if(cother->mask & platform_mask) {
          // resolve the collision
          struct Vector_ resolution;
          struct Rect_ rself, rother;
          cself->rect(&rself);
          cother->rect(&rother);
          rect_resolve_interpenetration(&resolution, &rself, &rother);
          vector_add(&go->_pos, &go->_pos, &resolution);

          // zero our velocity in the collision direction
          if(fabs(resolution.x) > 0) {
            go->_vel.x = 0;
          } else {
            go->_vel.y = 0;
          }
        }
      }
      return 0;
    });
}

void CPlatformer::look_for_support() {
  go->inbox.foreach([this](Message* message) -> int {
      if(message->kind != MESSAGE_COLLIDING) return 0;
      CCollidable* cself = (CCollidable*)message->data2;
      CCollidable* cother = (CCollidable*)message->data;
      GO* other_go = cother->go;

      struct Rect_ rself, rother;
      cself->rect(&rself);
      cother->rect(&rother);

      // check for support
      if(rect_is_supported(&rself, &rother)) {
        // parent this object to the supporter
        go_set_parent(go, other_go);
      }

      // done
      return 1;
    });
}

void CPlatformer::messages_received() {
  resolve_interpenetration();
  if(!go->transform_parent) {
    look_for_support();
  }
}

void CPlatformer::update(float dt) {
  // apply gravity and look for support if not supported

  if(!go->transform_parent) {
    go->_vel.y = MAX(-this->max_speed, go->_vel.y - this->grav_accel * dt);
  } else if(!is_supported()) {
    go_set_parent(go, NULL);
  }
}
