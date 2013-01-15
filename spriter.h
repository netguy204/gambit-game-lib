#ifndef SPRITER_H
#define SPRITER_H

#include "spriteatlas.h"

struct KeyFrameElement {
  SpriteAtlasEntry entry; // the compiled format stores the name here
  float angle;
  float pivot_x;
  float pivot_y;
  float scale_x;
  float scale_y;
  float x;
  float y;
  short spin;
};

struct Timeline {
  unsigned short nelements;
  KeyFrameElement* elements;
};

struct MasterElementRef {
  unsigned short timeline_idx;
  unsigned short keyframe_idx;
  short parent_idx;
};

struct MasterKey {
  unsigned short time_ms;

  unsigned short nrefs;
  MasterElementRef* refs;

  unsigned short nbones;
  MasterElementRef* bones;
};

struct Animation {
  char name[MAX_ENTRY_NAME];
  unsigned short length_ms;
  unsigned short looping;

  unsigned short nframes;
  MasterKey* frames;

  unsigned short ntimelines;
  Timeline* timelines;
};

struct Entity {
  unsigned short nanimations;
  Animation* animations;
};

Entity* spriter_load(const char* compiled_spriter, SpriteAtlas atlas);
void spriter_free(Entity* ent);

Animation* spriter_find(Entity* entity, const char* name);

BaseSprite spriter_append(BaseSprite list, Animation* animation,
                          Vector pos, unsigned short anim_time_ms);

#endif
