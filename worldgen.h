#ifndef WORLDGEN_H
#define WORLDGEN_H

#include "pathfinder.h"
#include "listlib.h"
#include "ooc.h"

typedef struct Civilization_ {
  struct Object _;
  struct DLLNode_ node;
  int index;
  int center;
  int nbuildings;
  int buildings[6];
} *Civilization;

extern void* CivilizationObject;

extern struct DLL_ civilizations;
extern PairwisePaths civpaths;
extern TileMap tiles;
extern SpriteAtlas atlas;

struct SpriteAtlas_;
struct TileMap_;

void worldgen_init();

Civilization node_to_civilization(DLLNode node);

#endif
