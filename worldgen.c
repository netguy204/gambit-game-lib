#include "worldgen.h"

#include "perlin.h"
#include "vector.h"
#include "tiles.h"
#include "spriteatlas.h"
#include "heapvector.h"
#include "utils.h"
#include "pathfinder.h"

#include <math.h>

Civilization civilizations;
PairwisePaths civpaths;

enum TestTiles {
  TILE_BLANK,
  TILE_GRASS,
  TILE_DIRT,
  TILE_STONE,
  TILE_STONE2,
  TILE_DIRTSTONE_TRANSITION,
  TILE_BUILDING,
  TILE_MAX
};

typedef struct CandidateUdata_ {
  HeapVector hv;
  TileMap map;
  CharImage reachable_img;
} *CandidateUdata;

void mark_candidate(LabelEntry entries, int nentries, void* udata) {
  CandidateUdata cdata = (CandidateUdata)udata;

  int ii, jj;

  // centroid the entries
  int8_t max_value = 0;
  struct TilePosition_ max_pos;

  for(ii = 0; ii < nentries; ++ii) {
    LabelEntry entry = &entries[ii];
    struct TilePosition_ pos = { entry->pos.x + 4, entry->pos.y + 4 };
    int8_t reachable = charimage_get(cdata->reachable_img, pos.x, pos.y);
    if(entry->value > max_value && reachable == 1) {
      max_value = entry->value;
      max_pos = pos;
    }
  }

  if(max_value) {
    HV_PUSH_VALUE(cdata->hv, struct TilePosition_, max_pos);
  }
}

void scatter_buildings(TileMap map, TilePosition pos, Civilization civilization) {
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

  int building = 0;

  // place a building on ground in each direction
  for(ii = 0; ii < array_size(directions); ++ii) {
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
        civilization->buildings[building++] = idx_above;
        break;
      }
    }
  }
  civilization->nbuildings = building;
}

TileMap tilemap_testmake(SpriteAtlas atlas) {
  int MAXX = 1000;
  int MAXY = 100;

  TileSpec specs = malloc(sizeof(struct TileSpec_) * TILE_MAX);
  int standard = TILESPEC_COLLIDABLE | TILESPEC_VISIBLE;

  specs[TILE_BLANK].image = NULL;
  specs[TILE_BLANK].bitmask = TILESPEC_PASSABLE;
  specs[TILE_GRASS].image = spriteatlas_find(atlas, "grass.png");
  specs[TILE_GRASS].bitmask = standard;
  specs[TILE_DIRT].image = spriteatlas_find(atlas, "techno1.png");
  specs[TILE_DIRT].bitmask = standard;
  specs[TILE_STONE].image = spriteatlas_find(atlas, "techno2.png");
  specs[TILE_STONE].bitmask = standard;
  specs[TILE_STONE2].image = spriteatlas_find(atlas, "stone2.png");
  specs[TILE_STONE2].bitmask = standard;
  specs[TILE_DIRTSTONE_TRANSITION].image = spriteatlas_find(atlas, "12uptransition.png");
  specs[TILE_DIRTSTONE_TRANSITION].bitmask = standard;
  specs[TILE_BUILDING].image = spriteatlas_find(atlas, "building.png");
  specs[TILE_BUILDING].bitmask = standard | TILESPEC_PASSABLE;

  SpriteAtlasEntry example = specs[TILE_GRASS].image;
  TileMap map = tilemap_make(MAXX, MAXY, example->w, example->h);
  map->tile_specs = specs;

  struct Timer_ timer;

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

  PROFILE_START(&timer, "sampling world structure");
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
  PROFILE_END(&timer);

  // add stone by intersecting skewed noise with the dirt
  struct Perlin_ perlin3;
  struct Vector_ offset3 = {0.0f, 0.0f};
  struct Vector_ scale3 = {0.03f, 0.1f};
  perlin_init(&perlin3, &random, &offset3, &scale3);

  PROFILE_START(&timer, "adding stone");
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
  PROFILE_END(&timer);

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
  PROFILE_START(&timer, "planting grass");
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
  PROFILE_END(&timer);

  // look for stone to dirt transition points
  int mapsize = MAXX * MAXY;
  for(xx = 0; xx < mapsize; ++xx) {
    if(map->tiles[xx] == TILE_DIRT) {
      int up_idx = xx + MAXX;
      if(up_idx >= mapsize) continue;

      if(map->tiles[up_idx] == TILE_STONE) {
        map->tiles[xx] = TILE_DIRTSTONE_TRANSITION;
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
  PROFILE_START(&timer, "finding reachable regions");
  struct TilePosition_ start = {0, MAXY - 1};

  struct CharImage_ reachable_img;
  charimage_init_sizeof_tilemap(&reachable_img, map);
  memset(reachable_img.data, -1, tilemap_size(map));

  struct CharImage_ map_img;
  charimage_from_tilemap(&map_img, map);

  charimage_floodfill(&reachable_img, &map_img, &start, 1, NULL, NULL);
  PROFILE_END(&timer);

  charimage_spit(&reachable_img, "reachable.csv");

  int8_t template[] =
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
  PROFILE_START(&timer, "finding civ plant candidate locations");
  correlation_img.w = reachable_img.w - template_img.w;
  correlation_img.h = reachable_img.h - template_img.h;
  correlation_img.data = malloc(correlation_img.w * correlation_img.h);
  charimage_crosscorrelate(&correlation_img, &reachable_img, &template_img);
  PROFILE_END(&timer);

  HeapVector hv = heapvector_make();

  PROFILE_START(&timer, "labeling candidate locations");
  charimage_threshold(&correlation_img, 57);
  charimage_spit(&correlation_img, "correlation.csv");

  struct CandidateUdata_ udata = { hv, map, &reachable_img };
  int8_t* label_mem = malloc(tilemap_size(map));
  charimage_label(&correlation_img, label_mem, mark_candidate, &udata);
  free(label_mem);

  int ii;
  int ncivs = HV_SIZE(hv, struct TilePosition_);
  civilizations = malloc(sizeof(struct Civilization_) * ncivs);

  for(ii = 0; ii < ncivs; ++ii) {
    TilePosition pos = HV_GET(hv, struct TilePosition_, ii);
    civilizations[ii].center = tilemap_index(map, pos);
    scatter_buildings(map, pos, &civilizations[ii]);
  }
  PROFILE_END(&timer);
  printf("%d civilizations\n", ncivs);

  PROFILE_START(&timer, "pathfinding");
  civpaths = pathfinder_findpairwise(map, (TilePosition)hv->data, ncivs);
  PROFILE_END(&timer);

  for(ii = 0; ii < civpaths->npaths; ++ii) {
    if(ii % 4 == 0) {
      printf("\n");
    }
    printf("%5d   ", civpaths->lengths[ii]);

    /* show the paths */
    /*
    int kk;
    Path path = &civpaths->paths[ii];
    for(kk = 0; kk < path->nsteps; ++kk) {
      map->tiles[path->steps[kk]] = 4;
    }
    */
  }

  heapvector_free(hv);
  free(reachable_img.data);
  free(correlation_img.data);

  return map;
}
