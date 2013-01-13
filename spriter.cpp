#include "spriter.h"
#include "filenumbers.h"
#include "utils.h"

#include <string.h>

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
    anim->frames = (KeyFrame*)malloc(sizeof(KeyFrame) * anim->nframes);

    for(int fidx = 0; fidx < anim->nframes; ++fidx) {
      KeyFrame* frame = &anim->frames[fidx];
      read_ushort(cs, &frame->time_ms);
      read_ushort(cs, &frame->nelements);
      frame->elements = (KeyFrameElement*)malloc(sizeof(KeyFrameElement) * frame->nelements);

      for(int eidx = 0; eidx < frame->nelements; ++eidx) {
        KeyFrameElement* el = &frame->elements[eidx];
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
      }
    }
  }
  return ent;
}

void spriter_free(Entity* ent) {
  for(int aidx = 0; aidx < ent->nanimations; ++aidx) {
    Animation* anim = &ent->animations[aidx];
    for(int fidx = 0; fidx < anim->nframes; ++fidx) {
      KeyFrame* frame = &anim->frames[fidx];
      free(frame->elements);
    }
    free(anim->frames);
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
