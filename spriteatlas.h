#ifndef SPRITEATLAS_H
#define SPRITEATLAS_H

#include "testlib.h"
#include "listlib.h"

#define MAX_ENTRY_NAME 12

struct SpriteAtlas_;

typedef struct SpriteAtlasEntry_ {
  struct SpriteAtlas_* atlas;
  int w, h;
  float u0, v0, u1, v1;
  char name[MAX_ENTRY_NAME];
} *SpriteAtlasEntry;

typedef struct SpriteAtlas_ {
  ImageResource image;
  int nentries;
  struct SpriteAtlasEntry_ entries[0];
} *SpriteAtlas;

SpriteAtlas spriteatlas_load(char* datafile, char* imgfile);
void spriteatlas_free(SpriteAtlas atlas);
SpriteAtlasEntry spriteatlas_find(SpriteAtlas atlas, char* name);

void sprite_fillfromentry(Sprite sprite, SpriteAtlasEntry entry);

#endif
