#include "spriter.h"
#include "filenumbers.h"
#include "utils.h"
#include "config.h"

#include <string.h>
#include <math.h>

Entity* spriter_load(const char* compiled_spriter, SpriteAtlas atlas) {
  FILE* cs = fopen(compiled_spriter, "rb");
  Entity* ent = (Entity*)malloc(sizeof(Entity));

  read_ushort(cs, &ent->nanimations);
  ent->animations = (Animation*)malloc(sizeof(Animation) * ent->nanimations);

  for(int aidx = 0; aidx < ent->nanimations; ++aidx) {
    Animation* anim = &ent->animations[aidx];
    fread(anim->name, sizeof(anim->name), 1, cs);
    read_ushort(cs, &anim->length_ms);
    read_ushort(cs, &anim->looping);
    read_ushort(cs, &anim->nframes);
    anim->frames = (MasterKey*)malloc(sizeof(MasterKey) * anim->nframes);

    for(int fidx = 0; fidx < anim->nframes; ++fidx) {
      MasterKey* frame = &anim->frames[fidx];
      read_ushort(cs, &frame->time_ms);

      read_ushort(cs, &frame->nrefs);
      frame->refs = (MasterElementRef*)malloc(sizeof(MasterElementRef) * frame->nrefs);

      for(int ridx = 0; ridx < frame->nrefs; ++ridx) {
        MasterElementRef* ref = &frame->refs[ridx];
        read_ushort(cs, &ref->timeline_idx);
        read_ushort(cs, &ref->keyframe_idx);
        read_short(cs, &ref->parent_idx);
      }

      read_ushort(cs, &frame->nbones);
      frame->bones = (MasterElementRef*)malloc(sizeof(MasterElementRef) * frame->nbones);

      for(int ridx = 0; ridx < frame->nbones; ++ridx) {
        MasterElementRef* ref = &frame->bones[ridx];
        read_ushort(cs, &ref->timeline_idx);
        read_ushort(cs, &ref->keyframe_idx);
        read_short(cs, &ref->parent_idx);
      }
    }

    read_ushort(cs, &anim->ntimelines);
    anim->timelines = (Timeline*)malloc(sizeof(Timeline) * anim->ntimelines);
    for(int tidx = 0; tidx < anim->ntimelines; ++tidx) {
      Timeline* tl = &anim->timelines[tidx];
      read_ushort(cs, &tl->nelements);
      tl->elements = (KeyFrameElement*)malloc(sizeof(KeyFrameElement) * tl->nelements);

      for(int eidx = 0; eidx < tl->nelements; ++eidx) {
        KeyFrameElement* el = &tl->elements[eidx];
        char entry_name[MAX_ENTRY_NAME];
        fread(entry_name, sizeof(entry_name), 1, cs);
        if(entry_name[0] != '\0') {
          el->entry = spriteatlas_find(atlas, entry_name);
        } else {
          // must be a bone
          el->entry = NULL;
        }
        read_fixed(cs, &el->angle);
        read_fixed(cs, &el->pivot_x);
        read_fixed(cs, &el->pivot_y);
        read_fixed(cs, &el->scale_x);
        read_fixed(cs, &el->scale_y);
        read_fixed(cs, &el->x);
        read_fixed(cs, &el->y);
        read_short(cs, &el->spin);
      }
    }
  }
  return ent;
}

void spriter_free(Entity* ent) {
  for(int aidx = 0; aidx < ent->nanimations; ++aidx) {
    Animation* anim = &ent->animations[aidx];
    for(int fidx = 0; fidx < anim->nframes; ++fidx) {
      MasterKey* frame = &anim->frames[fidx];
      free(frame->refs);
    }
    free(anim->frames);

    for(int tidx = 0; tidx < anim->ntimelines; ++tidx) {
      Timeline* tl = &anim->timelines[tidx];
      free(tl->elements);
    }
    free(anim->timelines);
  }

  free(ent->animations);
  free(ent);
}

Animation* spriter_find(Entity* entity, const char* name) {
  for(int ii = 0; ii < entity->nanimations; ++ii) {
    Animation* anim = &entity->animations[ii];
    if(strncmp(anim->name, name, MAX_ENTRY_NAME - 1) == 0) return anim;
  }
  fail_exit("couldn't find animation %s in Entity", name);
  return NULL;
}


float lerp_internal(float a, float s, float d) {
  return a + (s * d);
}

float lerp(float a, float b, float s) {
  float d = b - a;
  return lerp_internal(a, s, d);
}

float clerp(float a, float b, int d, float s) {
  if(d == 1) {
    if((b - a) < 0) {
      return lerp(a, b + 2 * M_PI, s);
    } else {
      return lerp(a, b, s);
    }
  } else {
    if((b - a) > 0) {
      return lerp(a, b - 2 * M_PI, s);
    } else {
      return lerp(a, b, s);
    }
  }
}

// from http://code.google.com/p/scml-pp/source/browse/trunk/source/SCMLpp.cpp line 2490
static void rotate_point(float& x, float& y, float angle, float origin_x, float origin_y, bool flipped)
{
  if(flipped)
    angle = -angle;

  float s = sin(angle*M_PI/180);
  float c = cos(angle*M_PI/180);
  float xnew = (x * c) - (y * s);
  float ynew = (x * s) + (y * c);
  xnew += origin_x;
  ynew += origin_y;

  x = xnew;
  y = ynew;
}

BaseSprite spriter_append(BaseSprite list, Animation* anim,
                          Vector pos, unsigned short anim_time_ms) {
  // clamp or restrict the animation time
  if(anim->looping) {
    anim_time_ms = anim_time_ms % anim->length_ms;
  } else {
    anim_time_ms = MAX(0, MIN(anim->length_ms, anim_time_ms));
  }

  // find the master keys that sandwich this time
  int idx_before = -1;
  int idx_after = -1;

  for(int ii = 0; ii < anim->nframes; ++ii) {
    MasterKey* frame = &anim->frames[ii];
    if(frame->time_ms > anim_time_ms) {
      // found the stopping point
      if(ii == 0) {
        // at or before zero, are we looping?
        if(anim->looping) {
          idx_before = anim->nframes - 1;
          idx_after = ii;
        } else {
          idx_before = ii;
          idx_after = ii;
        }
      } else {
        idx_before = ii - 1;
        idx_after = ii;
      }
      break;
    }
  }

  if(idx_before == -1) {
    // we must be off the end of the animation
    if(anim->looping) {
      idx_before = anim->nframes - 1;
      idx_after = 0;
    } else {
      idx_before = anim->nframes - 1;
      idx_after = anim->nframes - 1;
    }
  }

  unsigned short before_t = anim->frames[idx_before].time_ms;
  unsigned short after_t = anim->frames[idx_after].time_ms;
  if(after_t < before_t) {
    // in the looping section
    after_t = anim->length_ms;
  }
  unsigned short dt = after_t - before_t;

  // compute the scale factor
  float s;
  if(dt == 0) {
    s = 0;
  } else {
    s = ((double)(anim_time_ms - before_t)) / ((double)dt);
  }

  MasterKey* master = &anim->frames[idx_before];

  // build a transform stack that is as big as the bone heirarchy
  // could possibly be deap
  int* timeline_stack = (int*)frame_alloc(sizeof(int) * master->nrefs);
  int timeline_stack_size = master->nrefs;

  // now, using the list in the before key, we start tweening and
  // drawing
  for(int ii = master->nrefs - 1; ii >= 0; --ii) {
    MasterElementRef* ref = &master->refs[ii];

    // create the sprite
    Sprite sprite = frame_make_sprite();
    KeyFrameElement* end_before = &anim->timelines[ref->timeline_idx].elements[idx_before];
    sprite_fillfromentry(sprite, end_before->entry);
    sprite->displayX = pos->x;
    sprite->displayY = pos->y;
    sprite->angle = 0;
    sprite->originX = 0;
    sprite->originY = 0;

    // fill out the list of timelines we need to step through to
    // follow the bone heirarchy
    int last_stack_idx = timeline_stack_size - 1;
    timeline_stack[last_stack_idx] = ref->timeline_idx;
    while(ref->parent_idx >= 0) {
      ref = &master->bones[ref->parent_idx];
      timeline_stack[--last_stack_idx] = ref->timeline_idx;
    }

    // now walk from root to end following the bone heirarchy and
    // composing the transforms
    float pscale_x = 1.0f;
    float pscale_y = 1.0f;
    float pangle = 0.0f;
    for(int ii = last_stack_idx; ii < timeline_stack_size; ++ii) {
      int tl_idx = timeline_stack[ii];
      Timeline* tl = &anim->timelines[tl_idx];
      KeyFrameElement* before = &tl->elements[idx_before];
      KeyFrameElement* after = &tl->elements[idx_after];
      float scale_x = lerp(before->scale_x, after->scale_x, s);
      float scale_y = lerp(before->scale_y, after->scale_y, s);
      float angle = clerp(before->angle, after->angle, before->spin, s);
      float x = lerp(before->x, after->x, s) * pscale_x;
      float y = lerp(before->y, after->y, s) * pscale_y;

      bool flipped = (pscale_x < 0) || (pscale_y < 0);
      rotate_point(x, y, pangle,
                   sprite->displayX, sprite->displayY, flipped);
      sprite->displayX = x;
      sprite->displayY = y;
      sprite->w *= scale_x;
      sprite->h *= scale_y;
      sprite->angle += angle;

      // origin doesn't compose
      if(ii == (timeline_stack_size - 1)) {
        sprite->originX = lerp(before->pivot_x, after->pivot_x, s);
        sprite->originY = lerp(before->pivot_y, after->pivot_y, s);
      }

      pscale_x *= scale_x;
      pscale_y *= scale_y;
      pangle += angle;
    }

    sprite_append(list, sprite);
  }

  return list;
}
