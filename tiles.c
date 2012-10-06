#include "tiles.h"
#include "perlin.h"
#include "vector.h"

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
  int MAXX = 101;
  int MAXY = 100;
  TileMap map = tilemap_make(t1->atlas, MAXX, MAXY, t1->w, t1->h);

  char i1 = spriteatlas_index(t1);
  char i2 = spriteatlas_index(t2);

  struct Random_ random;
  random_init(&random, 1234);

  struct Perlin_ perlin;
  struct Vector_ offset = {0.0f, 0.0f};
  struct Vector_ scale = {0.1f, 0.1f};
  perlin_init(&perlin, &random, &offset, &scale);

  struct Perlin_ perlin2;
  struct Vector_ offset2 = {0.0f, 0.0f};
  struct Vector_ scale2 = {0.05f, 0.05f};
  perlin_init(&perlin2, &random, &offset2, &scale2);

  int xx, yy;
  for(yy = 0; yy < MAXY; ++yy) {
    for(xx = 0; xx < MAXX; ++xx) {
      struct Vector_ point = {xx, yy};
      int index = MAXX * yy + xx;
      float sample =
        perlin_sample(&perlin, &point) +
        perlin_sample(&perlin2, &point);

      if(sample > 0.4f) {
        map->tiles[index] = i1;
      } else {
        map->tiles[index] = i2;
      }
    }
  }

  return map;
}

int clamp(int val, int min, int max) {
  if(val < min) return min;
  if(val > max) return max;
  return val;
}

SpriteList tilemap_spritelist(TileMap map, float x_bl, float y_bl, float wpx, float hpx) {
  float mx_bl = x_bl - map->x_bl;
  float my_bl = y_bl - map->y_bl;
  float mx_tr = mx_bl + wpx;
  float my_tr = my_bl + hpx;

  int tx_bl = clamp(floor(mx_bl / map->tile_width_IP), 0, map->width_IT);
  int ty_bl = clamp(floor(my_bl / map->tile_height_IP), 0, map->height_IT);
  int tx_tr = clamp(ceil(mx_tr / map->tile_width_IP), 0, map->width_IT);
  int ty_tr = clamp(ceil(my_tr / map->tile_height_IP), 0, map->height_IT);

  SpriteList spritelist = NULL;

  int ox = (int)floorf((map->x_bl + tx_bl * map->tile_width_IP) - x_bl);
  int oy = (int)floorf((map->y_bl + ty_bl * map->tile_height_IP) - y_bl);

  int xx, yy;
  for(yy = 0; yy < ty_tr - ty_bl; ++yy) {
    int yoffset = map->width_IT * (ty_bl + yy);
    int y = oy + (map->tile_height_IP * yy);

    for(xx = 0; xx < tx_tr - tx_bl; ++xx) {
      int offset = yoffset + tx_bl + xx;
      int x = ox + (map->tile_width_IP * xx);

      int tile = map->tiles[offset];
      SpriteAtlasEntry entry = &map->atlas->entries[tile];

      Sprite sprite = frame_make_sprite();
      sprite_fillfromentry(sprite, entry);
      sprite->originX = 0.0f;
      sprite->originY = 0.0f;
      sprite->displayX = (float)x;
      sprite->displayY = (float)y;
      spritelist = frame_spritelist_append(spritelist, sprite);
    }
  }

  return spritelist;
}
