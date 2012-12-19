#include "platform.h"
#include "updateable.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

CCollidable node_to_collidable(DLLNode node) {
  return container_of(node, struct CCollidable_, node);
}

void collidable_rect(Rect rect, CCollidable coll) {
  GO go = component_to_go(coll);

  struct Vector_ pos;
  go_pos(&pos, go);
  rect_centered(rect, &pos, coll->w, coll->h);
}

int collidable_intersect(CCollidable a, CCollidable b) {
  struct Rect_ ra, rb;
  collidable_rect(&ra, a);
  collidable_rect(&rb, b);
  return rect_intersect(&ra, &rb);
}

void* CCollidableObject_ctor(void* _self, va_list* app) {
  CCollidable coll = super_ctor(CCollidableObject(), _self, app);
  GO go = component_to_go(coll);

  dll_add_head(&go_world(go)->collidables, &coll->node);

  coll->w = va_arg(*app, double);
  coll->h = va_arg(*app, double);
  coll->mask = MASK_PLATFORMER;
  return coll;
}

void* CCollidableObject_dtor(void* _self) {
  CCollidable coll = _self;
  GO go = component_to_go(coll);

  dll_remove(&go_world(go)->collidables, &coll->node);
  return super_dtor(CCollidableObject(), _self);
}

const void* CCollidableObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "CCollidable",
              ComponentObject(), sizeof(struct CCollidable_),
              ctor, CCollidableObject_ctor,
              dtor, CCollidableObject_dtor,
              0);
  return class;
}

void world_notify_collisions(World world) {
  DLLNode n1 = world->collidables.head;
  while(n1) {
    CCollidable c1 = node_to_collidable(n1);
    GO g1 = component_to_go(c1);

    DLLNode n2 = n1->next;
    while(n2) {
      CCollidable c2 = node_to_collidable(n2);

      if((c1->mask & c2->mask) && collidable_intersect(c1, c2)) {
        GO g2 = component_to_go(c2);

        Message m1 = message_make((Agent)g2, MESSAGE_COLLIDING, c2);
        m1->data2 = c1;
        message_postinbox((Agent)g1, m1);

        Message m2 = message_make((Agent)g1, MESSAGE_COLLIDING, c1);
        m2->data2 = c2;
        message_postinbox((Agent)g2, m2);
      }

      n2 = n2->next;
    }
    n1 = n1->next;
  }
}

void* CPlatformerObject_ctor(void* _self, va_list* app) {
  CPlatformer plat = super_ctor(CPlatformerObject(), _self, app);
  plat->grav_accel = va_arg(*app, double);
  plat->max_speed = 64 / .1;
  plat->platform_mask = MASK_PLATFORMER;
  return plat;
}

int CPlatformerObject_issupported(CPlatformer plat) {
  GO go1 = component_to_go(plat);
  GO go2 = go1->transform_parent;
  if(!go2) return 0;

  CCollidable c1 = go_find_component(go1, CCollidableObject());
  if(!c1) return 0;

  CCollidable c2 = go_find_component(go2, CCollidableObject());

  struct Rect_ r1, r2;
  collidable_rect(&r1, c1);
  collidable_rect(&r2, c2);

  return is_supported(&r1, &r2);
}

void CPlatformerObject_lookforsupport(CPlatformer plat, float dt) {
  GO go = component_to_go(plat);

  // look for a collision message
  DLLNode node = agent_inbox(go)->head;
  while(node) {
    Message message = node_to_message(node);
    if(message->kind == MESSAGE_COLLIDING) {
      // is it a kind of collidable we can stick to?
      CCollidable cself = message->data2;
      CCollidable cother = message->data;
      GO other_go = component_to_go(cother);

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

void CPlatformerObject_update(void* _self, float dt) {
  super_update(CPlatformerObject(), _self, dt);
  CPlatformer plat = _self;
  GO go = component_to_go(plat);

  // apply gravity and look for support if not supported
  if(!go->transform_parent) {
    go->vel.y = MAX(-plat->max_speed, go->vel.y - plat->grav_accel * dt);
    CPlatformerObject_lookforsupport(plat, dt);
  } else if(!CPlatformerObject_issupported(plat)) {
    go_set_parent(go, NULL);
  }
}

const void* CPlatformerObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "CPlatformer",
              ComponentObject(), sizeof(struct CPlatformer_),
              ctor, CPlatformerObject_ctor,
              update, CPlatformerObject_update,
              0);
  return class;
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
  struct Rect_ shifted = {a->minx + 0.5, a->miny - 0.5, a->maxx - 0.5, a->miny};
  if(rect_intersect(&shifted, b)) {
    return 1;
  } else {
    return 0;
  }
}
