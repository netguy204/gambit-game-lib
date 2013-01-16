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
  SpriteAtlas atlas = NULL;

  if(datafile == NULL) {
    // assume this is a single sprite image
    atlas = (SpriteAtlas)malloc(sizeof(SpriteAtlas_) + sizeof(SpriteAtlasEntry_));
    atlas->nentries = 1;
    atlas->image = image_load(imgfilename);

    SpriteAtlasEntry entry = &atlas->entries[0];
    entry->atlas = atlas;
    entry->w = atlas->image->w;
    entry->h = atlas->image->h;
    entry->u0 = 0;
    entry->u1 = 1;
    entry->v0 = 1;
    entry->v1 = 0;

    // get the name after the last slash
    const char* basename = strrchr(name, '/');
    if(basename) {
      ++basename;
    } else {
      basename = name;
    }

    strncpy(entry->name, basename, sizeof(entry->name));
  } else {
    unsigned short nentries;
    read_ushort(datafile, &nentries);

    atlas = (SpriteAtlas)malloc(sizeof(SpriteAtlas_) +
                                sizeof(SpriteAtlasEntry_) * nentries);
    atlas->nentries = nentries;
    atlas->image = image_load(imgfilename);

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
  }

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
