#ifndef PATHFINDER_H
#define PATHFINDER_H

#include "tiles.h"
#include "heapvector.h"
#include "memory.h"

struct PathElement_;

typedef struct PathElement_ {
  struct PathElement_* predecessor;
  int score;
  int distance;
  int index;
} *PathElement;

PathElement pathelement_make(StackAllocator allocator);

int* pathfinder_findpath(TileMap map, int p1, int p2, int* count);
PathElement pathfinder_findpath2(TileMap map, int p1, int p2, int8_t* visited,
                                 int8_t key, BinaryHeap candidates, StackAllocator allocator);

#endif
