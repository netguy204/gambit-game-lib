#include "platform.h"

#include <math.h>

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

  platformer->w = w;
  platformer->h = h;
  platformer->parent = NULL;
  platformer->falling = 1;
}

void platformer_abs_pos(Vector pos, Platformer platformer) {
  if(platformer->parent) {
    vector_add(pos, &platformer->parent->particle.pos, &platformer->particle.pos);
  } else {
    *pos = platformer->particle.pos;
  }
}

void platformer_abs_vel(Vector vel, Platformer platformer) {
  if(platformer->parent) {
    vector_add(vel, &platformer->parent->particle.vel, &platformer->particle.vel);
  } else {
    *vel = platformer->particle.vel;
  }
}

void platformer_rect(Rect rect, Platformer platformer) {
  struct Vector_ pos;
  platformer_abs_pos(&pos, platformer);
  rect_centered(rect, &pos, platformer->w, platformer->h);
}

Platform node_to_platform(DLLNode node) {
  return container_of(node, struct Platform_, node);
}

Platform is_platform_colliding(Rect a, DLL platforms) {
  DLLNode node = platforms->head;
  while(node) {
    Platform platform = node_to_platform(node);
    if(rect_intersect(a, (Rect)&platform->rect)) {
      return platform;
    }
    node = node->next;
  }
  return NULL;
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
  if(rect_intersect(&shifted, (Rect)&platform->rect)) {
    return 1;
  } else {
    return 0;
  }
}

void platformer_resolve(Platformer platformer, DLL platforms) {
  struct Rect_ prect;
  platformer_rect(&prect, platformer);

  if(!platformer->falling && (!platformer->parent ||
                              !is_supported(&prect, platformer->parent))) {
    platformer->falling = 1;
    platformer_abs_pos(&platformer->particle.pos, platformer);
    platformer->parent = NULL;
  }

  if(platformer->falling) {
    Platform platform;
    if((platform = is_platform_colliding(&prect, platforms))) {
      struct Vector_ resolution;
      resolve_interpenetration(&resolution, &prect, (Rect)&platform->rect);
      vector_add(&platformer->particle.pos, &platformer->particle.pos, &resolution);

      if(fabs(resolution.x) > 0.0f) {
        // bumped it, resolve the colision and remove our x component
        platformer->particle.vel.x = 0.0f;
      } else {
        platformer->particle.vel.y = 0.0f;
      }

      platformer_rect(&prect, platformer);
      if(is_supported(&prect, platform)) {
        platformer->falling = 0;
        struct Vector_ abs_pos;
        platformer_abs_pos(&abs_pos, platformer);
        vector_sub(&platformer->particle.pos, &abs_pos, &platform->particle.pos);
        platformer->parent = platform;
      }
    }
  }
}

void platform_update_rect(Platform p) {
  float w = rect_width((Rect)&p->rect);
  float h = rect_height((Rect)&p->rect);
  rect_centered((Rect)&p->rect, &p->particle.pos, w, h);
}
