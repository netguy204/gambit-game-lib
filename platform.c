#include "platform.h"

#include <math.h>

void world_foreach(World world, Rect rect, int mask,
                   WorldCallback callback, void* udata) {
  DLLNode node = world->game_objects.head;

  while(node) {
    DLLNode next = node->next;
    DCR dcr = (DCR)node_to_particle(node);
    if((dcr_mask(dcr) & mask) && rect_intersect(&dcr->rect, rect)) {
      if(callback(dcr, udata)) return;
    }
    node = next;
  }
}

void platform_rect(Rect rect, Platform platform) {
  Particle particle = (Particle)platform;
  rect_centered(rect, &particle->pos,
                dcr_w(platform), dcr_h(platform));
}

void platformer_setdims(Platformer platformer, float w, float h) {
  dcr_w(platformer) = w;
  dcr_h(platformer) = h;
}

void platformer_init(Platformer platformer, Vector pos, float w, float h) {
  Particle particle = (Particle)platformer;
  particle->pos = *pos;
  particle->vel.x = 0.0f;
  particle->vel.y = 0.0f;
  particle->image = NULL;
  particle->scale = 1.0f;
  particle->dsdt = 0.0f;
  particle->angle = 0.0f;
  particle->dadt = 0.0f;

  dcr_w(platformer) = w;
  dcr_h(platformer) = h;
  platformer->parent = NULL;
  platformer->falling = 1;
}

void platformer_abs_pos(Vector pos, Platformer platformer) {
  Particle pp = (Particle)platformer;
  Particle parp = (Particle)platformer->parent;

  if(platformer->parent) {
    vector_add(pos, &parp->pos, &pp->pos);
  } else {
    *pos = pp->pos;
  }
}

void platformer_abs_vel(Vector vel, Platformer platformer) {
  Particle pp = (Particle)platformer;
  Particle parp = (Particle)platformer->parent;
  if(platformer->parent) {
    vector_add(vel, &parp->vel, &pp->vel);
  } else {
    *vel = pp->vel;
  }
}

void platformer_rect(Rect rect, Platformer platformer) {
  struct Vector_ pos;
  platformer_abs_pos(&pos, platformer);
  rect_centered(rect, &pos, dcr_w(platformer), dcr_h(platformer));
}

Platform node_to_platform(DLLNode node) {
  return (Platform)container_of(node, struct Particle_, node);
}

Platformer node_to_platformer(DLLNode node) {
  return (Platformer)container_of(node, struct Particle_, node);
}

int is_platform_colliding_helper(DCR dcr, void* udata) {
  Platform* pptr = udata;
  *pptr = (Platform)dcr;
  return 1;
}

Platform is_platform_colliding(Rect a, World world, int mask) {
  Platform result = NULL;
  world_foreach(world, a, mask, is_platform_colliding_helper, &result);
  return result;
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
      resolution->y = -yint;
    } else {
      resolution->y = yint;
    }
  }
}

int is_supported(Rect a, Platform platform) {
  struct Rect_ shifted = {a->minx + 0.5, a->miny - 0.5, a->maxx - 0.5, a->miny};
  if(rect_intersect(&shifted, &dcr_rect(platform))) {
    return 1;
  } else {
    return 0;
  }
}

void platformer_resolve(Platformer platformer, World world, int mask) {
  struct Rect_ prect;
  Particle pp = (Particle)platformer;
  platformer_rect(&prect, platformer);

  if(!platformer->falling && (!platformer->parent ||
                              !is_supported(&prect, platformer->parent))) {
    platformer->falling = 1;
    platformer_abs_pos(&pp->pos, platformer);
    platformer->parent = NULL;
  }

  if(platformer->falling) {
    Platform platform;

    if((platform = is_platform_colliding(&prect, world, mask))) {
      Particle platp = (Particle)platform;
      struct Vector_ resolution;
      resolve_interpenetration(&resolution, &prect, &dcr_rect(platform));
      vector_add(&pp->pos, &pp->pos, &resolution);

      if(fabs(resolution.x) > 0.0f) {
        // bumped it, resolve the colision and remove our x component
        pp->vel.x = 0.0f;
      } else {
        pp->vel.y = 0.0f;
      }

      platformer_rect(&prect, platformer);
      if(is_supported(&prect, platform)) {
        platformer->falling = 0;
        struct Vector_ abs_pos;
        platformer_abs_pos(&abs_pos, platformer);
        vector_sub(&pp->pos, &abs_pos, &platp->pos);
        platformer->parent = platform;
      }
    }
  }
}
