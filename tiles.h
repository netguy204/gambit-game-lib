#ifndef TILES_H
#define TILES_H

#include "spriteatlas.h"
#include "testlib.h"

/*
 * IT = "In Tiles"
 * IP = "In Pixels"
 */

enum TileSpecMaskEntries {
  TILESPEC_COLLIDABLE = 1,
  TILESPEC_VISIBLE = 2
};

typedef struct TileSpec_ {
  SpriteAtlasEntry image;
  int bitmask;
} *TileSpec;

typedef struct TileMap_ {
  TileSpec tile_specs;
  int width_IT, height_IT;
  int tile_width_IP, tile_height_IP;
  float x_bl, y_bl;
  unsigned char tiles[0];
} *TileMap;

TileMap tilemap_make(int width, int height, int tw, int th);
void tilemap_free(TileMap map);

TileMap tilemap_testmake(SpriteAtlas atlas);
SpriteList tilemap_spritelist(TileMap map, float x_bl, float y_bl, float wpx, float hpx);

#endif
