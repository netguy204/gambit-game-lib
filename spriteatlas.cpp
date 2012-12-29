#include "spriteatlas.h"
#include "testlib.h"
#include "utils.h"

#include <stdio.h>
#include <string.h>

SpriteAtlas spriteatlas_load(const char* name, const char* imgtype) {
  char datafilename[128];
  char imgfilename[128];

  snprintf(datafilename, sizeof(datafilename), "%s.dat", name);
  snprintf(imgfilename, sizeof(imgfilename), "%s.%s", name, imgtype);

  FILE* datafile = fopen(datafilename, "r");
  if(datafile == NULL) fail_exit("failed to open %s", datafilename);

  fseek(datafile, 0L, SEEK_END);
  int length = ftell(datafile);
  fseek(datafile, 0L, SEEK_SET);

  SpriteAtlas atlas = (SpriteAtlas)malloc(sizeof(struct SpriteAtlas_) + length);
  atlas->nentries = length / sizeof(struct SpriteAtlasEntry_);

  // slurp in the data
  fread(atlas->entries, sizeof(struct SpriteAtlasEntry_), atlas->nentries, datafile);
  fclose(datafile);

  // build the backrefs
  int ii;
  for(ii = 0; ii < atlas->nentries; ++ii) {
    atlas->entries[ii].atlas = atlas;
  }

  atlas->image = image_load(imgfilename);
  return atlas;
}

void spriteatlas_free(SpriteAtlas atlas) {
  image_free(atlas->image);
  free(atlas);
}

int spriteatlas_index(SpriteAtlasEntry entry) {
  return entry - entry->atlas->entries;
}

SpriteAtlasEntry spriteatlas_find(SpriteAtlas atlas, const char* name) {
  int ii;
  for(ii = 0; ii < atlas->nentries; ++ii) {
    SpriteAtlasEntry entry = &atlas->entries[ii];
    if(strncmp(entry->name, name, MAX_ENTRY_NAME - 1) == 0) return entry;
  }
  fail_exit("couldn't find %s in atlas", name);
  return NULL;
}

void sprite_fillfromentry(Sprite sprite, SpriteAtlasEntry entry) {
  sprite->texture = entry->atlas->image->texture;
  sprite->u0 = entry->u0;
  sprite->u1 = entry->u1;
  sprite->v0 = entry->v0;
  sprite->v1 = entry->v1;
  sprite->w = entry->w;
  sprite->h = entry->h;
}
