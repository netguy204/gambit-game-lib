#include "tiles.h"

#include <math.h>

TileMap tilemap_make(SpriteAtlas atlas, int width, int height, int tw, int th) {
  int num_tiles = width * height;
  TileMap map = malloc(sizeof(struct TileMap_) + (sizeof(char) * num_tiles));
  map->atlas = atlas;
  map->width_IT = width;
  map->height_IT = height;
  map->tile_width_IP = tw;
  map->tile_height_IP = th;
  map->x_bl = 0;
  map->y_bl = 0;
  return map;
}

void tilemap_free(TileMap map) {
  free(map);
}

TileMap tilemap_testmake(SpriteAtlasEntry t1, SpriteAtlasEntry t2) {
  TileMap map = tilemap_make(t1->atlas, 101, 100, t1->w, t1->h);

  char i1 = spriteatlas_index(t1);
  char i2 = spriteatlas_index(t2);

  int ii;
  for(ii = 0; ii < 101 * 100; ++ii) {
    if(ii % 2 == 0) {
      map->tiles[ii] = i1;
    } else {
      map->tiles[ii] = i2;
    }
  }

  return map;
}

SpriteList tilemap_spritelist(TileMap map, float x_bl, float y_bl, float wpx, float hpx) {
  float mx_bl = x_bl - map->x_bl;
  float my_bl = y_bl - map->y_bl;
  float mx_tr = mx_bl + wpx;
  float my_tr = my_bl + hpx;

  int tx_bl = MAX(0, floor(mx_bl / map->tile_width_IP));
  int ty_bl = MAX(0, floor(my_bl / map->tile_height_IP));
  int tx_tr = MIN(map->width_IT - 1, ceil(mx_tr / map->tile_width_IP));
  int ty_tr = MIN(map->height_IT - 1, ceil(my_tr / map->tile_height_IP));

  SpriteList spritelist = NULL;

  int xx, yy;
  for(yy = ty_bl; yy <= ty_tr; ++yy) {
    int yoffset = map->width_IT * yy;

    for(xx = tx_bl; xx <= tx_tr; ++xx) {
      int offset = yoffset + xx;
      float x = (map->x_bl + xx * map->tile_width_IP) - x_bl;
      float y = (map->y_bl + yy * map->tile_height_IP) - y_bl;

      int tile = map->tiles[offset];
      SpriteAtlasEntry entry = &map->atlas->entries[tile];

      Sprite sprite = frame_make_sprite();
      sprite_fillfromentry(sprite, entry);
      sprite->originX = 0.0f;
      sprite->originY = 0.0f;
      sprite->displayX = x;
      sprite->displayY = y;
      spritelist = frame_spritelist_append(spritelist, sprite);
    }
  }

  return spritelist;
}
