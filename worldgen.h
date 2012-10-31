#ifndef WORLDGEN_H
#define WORLDGEN_H

#include "pathfinder.h"

typedef struct Civilization_ {
  int center;
  int nbuildings;
  int buildings[6];
} *Civilization;

extern Civilization civilizations;
extern PairwisePaths civpaths;

struct SpriteAtlas_;
struct TileMap_;

struct TileMap_* tilemap_testmake(struct SpriteAtlas_* atlas);

#endif
