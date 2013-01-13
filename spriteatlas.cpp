#include "spriteatlas.h"
#include "filenumbers.h"
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

  unsigned short nentries;
  read_ushort(datafile, &nentries);

  SpriteAtlas atlas = (SpriteAtlas)malloc(sizeof(struct SpriteAtlas_) +
                                          sizeof(struct SpriteAtlasEntry_) * nentries);
  atlas->nentries = nentries;

  // read the data
  int ii;
  for(ii = 0; ii < atlas->nentries; ++ii) {
    SpriteAtlasEntry entry = &atlas->entries[ii];
    entry->atlas = atlas;
    read_ushort(datafile, &entry->w);
    read_ushort(datafile, &entry->h);
    read_norm_fixed(datafile, &entry->u0);
    read_norm_fixed(datafile, &entry->v0);
    read_norm_fixed(datafile, &entry->u1);
    read_norm_fixed(datafile, &entry->v1);
    fread(entry->name, sizeof(entry->name), 1, datafile);
  }

  fclose(datafile);

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

void sprite_fillfromentry(BaseSprite sprite, SpriteAtlasEntry entry) {
  sprite->texture = entry->atlas->image->texture;
  sprite->u0 = entry->u0;
  sprite->u1 = entry->u1;
  sprite->v0 = entry->v0;
  sprite->v1 = entry->v1;
  sprite->w = entry->w;
  sprite->h = entry->h;
}
