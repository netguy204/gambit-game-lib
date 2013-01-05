#ifndef SPRITEATLAS_H
#define SPRITEATLAS_H

#include "testlib.h"
#include "listlib.h"

#include <stdint.h>

#define MAX_ENTRY_NAME 12

struct SpriteAtlas_;

typedef struct SpriteAtlasEntry_ {
  struct SpriteAtlas_* atlas;
  unsigned short w, h;
  float u0, v0, u1, v1;
  char name[MAX_ENTRY_NAME];
} *SpriteAtlasEntry;

typedef struct SpriteAtlas_ {
  ImageResource image;
  int nentries;
  struct SpriteAtlasEntry_ entries[0];
} *SpriteAtlas;

SpriteAtlas spriteatlas_load(const char* name, const char* imgtype);
void spriteatlas_free(SpriteAtlas atlas);
int spriteatlas_index(SpriteAtlasEntry entry);
SpriteAtlasEntry spriteatlas_find(SpriteAtlas atlas, const char* name);

void sprite_fillfromentry(BaseSprite sprite, SpriteAtlasEntry entry);

#endif
