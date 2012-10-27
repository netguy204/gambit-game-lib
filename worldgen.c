#include "worldgen.h"

#include "perlin.h"
#include "vector.h"
#include "tiles.h"
#include "spriteatlas.h"
#include "heapvector.h"

#include <math.h>

enum TestTiles {
  TILE_BLANK,
  TILE_GRASS,
  TILE_DIRT,
  TILE_STONE,
  TILE_STONE2,
  TILE_BUILDING,
  TILE_MAX
};

void mark_candidate(LabelEntry entries, int nentries, void* udata) {
  HeapVector* hvp = (HeapVector*)udata;
  HeapVector hv = *hvp;

  float x = 0;
  float y = 0;
  int ii, jj;

  // centroid the entries
  float values = 0;
  for(ii = 0; ii < nentries; ++ii) {
    LabelEntry entry = &entries[ii];
    x += entry->pos.x * entry->value;
    y += entry->pos.y * entry->value;
    values += entry->value;
  }

  x /= values;
  y /= values;

  struct TilePosition_ pos = { roundf(x) + 4, roundf(y) + 4 };
  HV_PUSH_VALUE(hv, struct TilePosition_, pos);
  *hvp = hv;
}

void scatter_buildings(TileMap map, TilePosition pos) {
  int ii, jj;

  int directions[][2] = {
    {0,  -1},
    {-1, -1},
    {-1, -2},
    {1,  -1},
    {1,  -2},
    {-2, -1},
    {2,  -1},
  };

  // place a building on ground in each direction
  for(ii = 0; ii < 7; ++ii) {
    // search out a max of 5 spaces
    for(jj = 0; jj < 5; ++jj) {
      struct TilePosition_ pos2 = { pos->x + directions[ii][0] * jj,
                                    pos->y + directions[ii][1] * jj };
      struct TilePosition_ above = { pos2.x, pos2.y + 1};

      int idx = tilemap_index(map, &pos2);
      int idx_above = tilemap_index(map, &above);

      if(map->tiles[idx] != TILE_BLANK &&
         map->tiles[idx_above] == TILE_BLANK) {
        map->tiles[idx_above] = TILE_BUILDING;
      }
    }
  }
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
  specs[TILE_STONE2].image = spriteatlas_find(atlas, "stone2.png");
  specs[TILE_STONE2].bitmask = TILESPEC_COLLIDABLE | TILESPEC_VISIBLE;
  specs[TILE_BUILDING].image = spriteatlas_find(atlas, "building.png");
  specs[TILE_BUILDING].bitmask = TILESPEC_COLLIDABLE | TILESPEC_VISIBLE;

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
      struct TilePosition_ pos = {xx, yy};
      int index = tilemap_index(map, &pos);

      if(map->tiles[index] != TILE_DIRT) continue;

      int found = 0;
      for(zz = 0; zz < nabove; ++zz) {
        int x = above[zz][0] + xx;
        int y = above[zz][1] + yy;
        struct TilePosition_ p2 = {x, y};
        if(!tilemap_validindex(map, &p2)) continue;
        int index2 = tilemap_index(map, &p2);
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

  // replace the bottom row with stone and the top row with sky
  int top_row = (MAXY - 1) * (MAXX - 1);
  for(xx = 0; xx < MAXX; ++xx) {
    map->tiles[xx] = TILE_STONE;
    map->tiles[top_row + xx] = TILE_BLANK;
  }

  // floodfill from the top and see what we get
  struct TilePosition_ start = {0, MAXY - 1};
  char * reachable = malloc(tilemap_size(map));
  memset(reachable, -1, tilemap_size(map));
  struct CharImage_ reachable_img = { MAXX, MAXY, reachable };
  struct CharImage_ map_img = { MAXX, MAXY, map->tiles };
  int count = charimage_floodfill(&reachable_img, &map_img, &start, 1, NULL, NULL);

  charimage_spit(&reachable_img, "reachable.csv");

  char template[] =
    { 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 1, 1, 1, 0, 0,
      0, 1, 1, 1, 1, 1, 1, 0,
      0, 1, 1, 1, 1, 1, 1, 0,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1 };

  struct CharImage_ template_img = { 8, 8, template };
  charimage_replace_value(&template_img, 0, -1);

  struct CharImage_ correlation_img;
  correlation_img.w = reachable_img.w - template_img.w;
  correlation_img.h = reachable_img.h - template_img.h;
  correlation_img.data = malloc(correlation_img.w * correlation_img.h);
  charimage_crosscorrelate(&correlation_img, &reachable_img, &template_img);
  charimage_spit(&correlation_img, "correlation.csv");

  charimage_threshold(&correlation_img, 55);

  HeapVector hv = heapvector_make(10);
  charimage_label(&correlation_img, reachable, mark_candidate, &hv);

  int ii;
  int ncivs = HV_SIZE(hv, struct TilePosition_);
  for(ii = 0; ii < ncivs; ++ii) {
    scatter_buildings(map, HV_GET(hv, struct TilePosition_, ii));
  }

  heapvector_free(hv);

  free(reachable);
  free(correlation_img.data);

  return map;
}
