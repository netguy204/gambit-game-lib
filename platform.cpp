#include "platform.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

OBJECT_IMPL(CPlatformer);

CPlatformer::CPlatformer()
  : Component(NULL), grav_accel(9.81), platform_mask(0) {
}

CPlatformer::CPlatformer(GO* go, float grav_accel)
  : Component(go), grav_accel(grav_accel), platform_mask(MASK_PLATFORMER) {
  this->max_speed = 64 / .1;
}

int CPlatformerObject_issupported(CPlatformer* plat) {
  GO* go1 = component_to_go(plat);
  GO* go2 = go1->transform_parent;
  if(!go2) return 0;

  CCollidable* c1 = (CCollidable*)go_find_component(go1, &CCollidable::Type);
  if(!c1) return 0;

  CCollidable* c2 = (CCollidable*)go_find_component(go2, &CCollidable::Type);
  if(!c2) return 0; // why could this happen?

  struct Rect_ r1, r2;
  c1->rect(&r1);
  c2->rect(&r2);

  return is_supported(&r1, &r2);
}

void CPlatformerObject_lookforsupport(CPlatformer* plat, float dt) {
  GO* go = component_to_go(plat);

  // look for a collision message
  DLLNode node = go->inbox.head;
  while(node) {
    Message* message = go->inbox.to_element(node);
    if(message->kind == MESSAGE_COLLIDING) {
      // is it a kind of collidable we can stick to?
      CCollidable* cself = (CCollidable*)message->data2;
      CCollidable* cother = (CCollidable*)message->data;
      GO* other_go = component_to_go(cother);

      if(cother->mask & plat->platform_mask) {
        // resolve the collision
        struct Vector_ resolution;
        struct Rect_ rself, rother;
        cself->rect(&rself);
        cother->rect(&rother);
        resolve_interpenetration(&resolution, &rself, &rother);
        vector_add(&go->_pos, &go->_pos, &resolution);

        // zero our velocity in the collision direction
        if(fabs(resolution.x) > 0) {
          go->_vel.x = 0;
        } else {
          go->_vel.y = 0;
        }

        // check for support
        if(is_supported(&rself, &rother)) {
          // parent this object to the supporter
          go_set_parent(go, other_go);
        }

        // done
        return;
      }
    }

    node = node->next;
  }
}

void CPlatformer::update(float dt) {
  GO* go = this->parent_go;

  // apply gravity and look for support if not supported
  if(!go->transform_parent) {
    go->_vel.y = MAX(-this->max_speed, go->_vel.y - this->grav_accel * dt);
    CPlatformerObject_lookforsupport(this, dt);
  } else if(!CPlatformerObject_issupported(this)) {
    go_set_parent(go, NULL);
  }
}

// assumes a collision has already been found
void resolve_interpenetration(Vector resolution, Rect minor, Rect major) {
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

int is_supported(Rect a, Rect b) {
  struct Rect_ shifted = {a->minx + 0.5f, a->miny - 0.5f, a->maxx - 0.5f, a->miny};
  if(rect_intersect(&shifted, b)) {
    return 1;
  } else {
    return 0;
  }
}
