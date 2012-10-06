#ifndef TILES_H
#define TILES_H

#include "spriteatlas.h"
#include "testlib.h"

/*
 * IT = "In Tiles"
 * IP = "In Pixels"
 */

typedef struct TileMap_ {
  SpriteAtlas atlas;
  int width_IT, height_IT;
  int tile_width_IP, tile_height_IP;
  float x_bl, y_bl;
  unsigned char tiles[0];
} *TileMap;

TileMap tilemap_make(SpriteAtlas atlas, int width, int height, int tw, int th);
void tilemap_free(TileMap map);

TileMap tilemap_testmake(SpriteAtlasEntry t1, SpriteAtlasEntry t2);
SpriteList tilemap_spritelist(TileMap map, float x_bl, float y_bl, float wpx, float hpx);

#endif
