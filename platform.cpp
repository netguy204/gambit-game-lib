#include "platform.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

CCollidable* node_to_collidable(DLLNode node) {
  return container_of(node, CCollidable, node);
}

void collidable_rect(Rect rect, CCollidable* coll) {
  GO* go = component_to_go(coll);

  struct Vector_ pos;
  go_pos(&pos, go);
  rect_centered(rect, &pos, coll->w, coll->h);
}

int collidable_intersect(CCollidable* a, CCollidable* b) {
  struct Rect_ ra, rb;
  collidable_rect(&ra, a);
  collidable_rect(&rb, b);
  return rect_intersect(&ra, &rb);
}

OBJECT_IMPL(CCollidable);

CCollidable::CCollidable()
  : Component(NULL), w(0), h(0) {
}

CCollidable::CCollidable(GO* go, float w, float h)
  : Component(go), w(w), h(h), mask(MASK_PLATFORMER) {
  if(go) {
    dll_add_head(&go_world(go)->collidables, &this->node);
  }
}

CCollidable::~CCollidable() {
  GO* go = this->parent_go;
  if(go) {
    dll_remove(&go_world(go)->collidables, &this->node);
  }
}

void world_notify_collisions(World* world) {
  DLLNode n1 = world->collidables.head;
  while(n1) {
    CCollidable* c1 = node_to_collidable(n1);
    GO* g1 = component_to_go(c1);

    DLLNode n2 = n1->next;
    while(n2) {
      CCollidable* c2 = node_to_collidable(n2);

      if((c1->mask & c2->mask) && collidable_intersect(c1, c2)) {
        GO* g2 = component_to_go(c2);

        Message* m1 = message_make(g2, MESSAGE_COLLIDING, c2);
        m1->data2 = c1;
        message_postinbox(g1, m1);

        Message* m2 = message_make(g1, MESSAGE_COLLIDING, c1);
        m2->data2 = c2;
        message_postinbox(g2, m2);
      }

      n2 = n2->next;
    }
    n1 = n1->next;
  }
}

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
  collidable_rect(&r1, c1);
  collidable_rect(&r2, c2);

  return is_supported(&r1, &r2);
}

void CPlatformerObject_lookforsupport(CPlatformer* plat, float dt) {
  GO* go = component_to_go(plat);

  // look for a collision message
  DLLNode node = agent_inbox(go)->head;
  while(node) {
    Message* message = node_to_message(node);
    if(message->kind == MESSAGE_COLLIDING) {
      // is it a kind of collidable we can stick to?
      CCollidable* cself = (CCollidable*)message->data2;
      CCollidable* cother = (CCollidable*)message->data;
      GO* other_go = component_to_go(cother);

      if(cother->mask & plat->platform_mask) {
        // resolve the collision
        struct Vector_ resolution;
        struct Rect_ rself, rother;
        collidable_rect(&rself, cself);
        collidable_rect(&rother, cother);
        resolve_interpenetration(&resolution, &rself, &rother);
        vector_add(&go->pos, &go->pos, &resolution);

        // zero our velocity in the collision direction
        if(fabs(resolution.x) > 0) {
          go->vel.x = 0;
        } else {
          go->vel.y = 0;
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
    go->vel.y = MAX(-this->max_speed, go->vel.y - this->grav_accel * dt);
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
