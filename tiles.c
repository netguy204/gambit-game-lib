#include "tiles.h"
#include "perlin.h"
#include "vector.h"

#include <math.h>

TileMap tilemap_make(int width, int height, int tw, int th) {
  int num_tiles = width * height;
  TileMap map = malloc(sizeof(struct TileMap_) + (sizeof(char) * num_tiles));
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

enum TestTiles {
  TILE_BLANK,
  TILE_GRASS,
  TILE_DIRT,
  TILE_STONE,
  TILE_MAX
};

int tilemap_index(TileMap map, int x, int y) {
  return map->width_IT * y + x;
}

int tilemap_validindex(TileMap map, int x, int y) {
  return x >= 0 && x < map->width_IT && y >= 0 && y < map->height_IT;
}

TileMap tilemap_testmake(SpriteAtlas atlas) {
  int MAXX = 1000;
  int MAXY = 100;

  TileSpec specs = malloc(sizeof(struct TileSpec_) * TILE_MAX);
  specs[TILE_BLANK].bitmask = 0;
  specs[TILE_BLANK].image = NULL;
  specs[TILE_GRASS].image = spriteatlas_find(atlas, "grass.png");
  specs[TILE_GRASS].bitmask = TILESPEC_COLLIDABLE | TILESPEC_VISIBLE;
  specs[TILE_DIRT].image = spriteatlas_find(atlas, "dirt.png");
  specs[TILE_DIRT].bitmask = TILESPEC_COLLIDABLE | TILESPEC_VISIBLE;
  specs[TILE_STONE].image = spriteatlas_find(atlas, "stone.png");
  specs[TILE_STONE].bitmask = TILESPEC_COLLIDABLE | TILESPEC_VISIBLE;

  SpriteAtlasEntry example = specs[TILE_GRASS].image;
  TileMap map = tilemap_make(MAXX, MAXY, example->w, example->h);
  map->tile_specs = specs;

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
    float vgrad = (float)((MAXY - yy)) / MAXY;

    for(xx = 0; xx < MAXX; ++xx) {
      struct Vector_ point = {xx, yy};
      int index = MAXX * yy + xx;
      float sample =
        vgrad +
        (perlin_sample(&perlin, &point) + perlin_sample(&perlin2, &point));

      if(sample > 0.5f) {
        map->tiles[index] = TILE_DIRT;
      } else {
        map->tiles[index] = TILE_BLANK;
      }
    }
  }

  // add stone by intersecting skewed noise with the dirt
  struct Perlin_ perlin3;
  struct Vector_ offset3 = {0.0f, 0.0f};
  struct Vector_ scale3 = {0.03f, 0.1f};
  perlin_init(&perlin3, &random, &offset3, &scale3);

  for(yy = 0; yy < MAXY; ++yy) {
    for(xx = 0; xx < MAXX; ++xx) {
      int index = MAXX * yy + xx;
      if(map->tiles[index] != TILE_DIRT) continue;

      struct Vector_ point = {xx, yy};
      float sample = perlin_sample(&perlin3, &point);
      if(sample > 0.3f) {
        map->tiles[index] = TILE_STONE;
      }
    }
  }

  int above[][2] = {
    {0, 1},
    {-1, 2},
    {0, 2},
    {1, 2},
    {-2, 3},
    {-1, 3},
    {0, 3},
    {1, 3},
    {2, 3}};

  int nabove = sizeof(above) / (sizeof(int) * 2);
  int zz;

  // look for dirt with nothing immediately above it, turh that into
  // grass
  for(yy = 0; yy < MAXY; ++yy) {
    for(xx = 0; xx < MAXX; ++xx) {
      int index = tilemap_index(map, xx, yy);

      if(map->tiles[index] != TILE_DIRT) continue;

      int found = 0;
      for(zz = 0; zz < nabove; ++zz) {
        int x = above[zz][0] + xx;
        int y = above[zz][1] + yy;
        if(!tilemap_validindex(map, x, y)) continue;
        int index2 = tilemap_index(map, x, y);
        if(map->tiles[index2] != TILE_BLANK) {
          found = 1;
          break;
        }
      }

      // clear sky? plant grass!
      if(!found) {
        map->tiles[index] = TILE_GRASS;
      }
    }
  }

  // replace the bottom row with stone
  for(xx = 0; xx < MAXX; ++xx) {
    map->tiles[xx] = TILE_STONE;
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
      TileSpec spec = &map->tile_specs[tile];
      if((spec->bitmask & TILESPEC_VISIBLE) == 0) continue;

      Sprite sprite = frame_make_sprite();
      sprite_fillfromentry(sprite, spec->image);
      sprite->originX = 0.0f;
      sprite->originY = 0.0f;
      sprite->displayX = (float)x;
      sprite->displayY = (float)y;
      spritelist = frame_spritelist_append(spritelist, sprite);
    }
  }

  return spritelist;
}

int tilemap_floodfill(TileMap map, Vector start, char* memory, int limit) {
  return 0;
}
