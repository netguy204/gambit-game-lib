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
        el->entry = spriteatlas_find(atlas, entry_name);
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

  // now, using the list in the before key, we start tweening and
  // drawing
  MasterKey* master = &anim->frames[idx_before];
  for(int ii = master->nrefs - 1; ii >= 0; --ii) {
    int tl_idx = master->refs[ii].timeline_idx;
    Timeline* tl = &anim->timelines[tl_idx];
    KeyFrameElement* before = &tl->elements[idx_before];
    KeyFrameElement* after = &tl->elements[idx_after];
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, before->entry);
    sprite->displayX = pos->x + lerp(before->x, after->x, s);
    sprite->displayY = pos->y + lerp(before->y, after->y, s);
    sprite->w *= lerp(before->scale_x, after->scale_x, s);
    sprite->h *= lerp(before->scale_y, after->scale_y, s);
    sprite->angle = clerp(before->angle, after->angle, before->spin, s);
    sprite->originX = lerp(before->pivot_x, after->pivot_x, s);
    sprite->originY = lerp(before->pivot_y, after->pivot_y, s);
    sprite_append(list, sprite);
  }

  return list;
}
