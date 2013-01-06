#include "spriteatlas.h"
#include "testlib.h"
#include "utils.h"

#include <stdio.h>
#include <string.h>

#ifndef __APPLE__
#include <sys/endian.h>
#endif

#define COORD_SCALE (1<<15)

void read_short(FILE* fh, unsigned short* value) {
  fread(value, sizeof(unsigned short), 1, fh);
  *value = ntohs(*value);
}

void read_fixed(FILE* fh, float* value) {
  unsigned short fixed_value;
  read_short(fh, &fixed_value);
  *value = ((double)fixed_value) / COORD_SCALE;
}

SpriteAtlas spriteatlas_load(const char* name, const char* imgtype) {
  char datafilename[128];
  char imgfilename[128];

  snprintf(datafilename, sizeof(datafilename), "%s.dat", name);
  snprintf(imgfilename, sizeof(imgfilename), "%s.%s", name, imgtype);

  FILE* datafile = fopen(datafilename, "r");
  if(datafile == NULL) fail_exit("failed to open %s", datafilename);

  unsigned short nentries;
  read_short(datafile, &nentries);

  SpriteAtlas atlas = (SpriteAtlas)malloc(sizeof(struct SpriteAtlas_) +
                                          sizeof(struct SpriteAtlasEntry_) * nentries);
  atlas->nentries = nentries;

  // read the data
  int ii;
  for(ii = 0; ii < atlas->nentries; ++ii) {
    SpriteAtlasEntry entry = &atlas->entries[ii];
    entry->atlas = atlas;
    read_short(datafile, &entry->w);
    read_short(datafile, &entry->h);
    read_fixed(datafile, &entry->u0);
    read_fixed(datafile, &entry->v0);
    read_fixed(datafile, &entry->u1);
    read_fixed(datafile, &entry->v1);
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
