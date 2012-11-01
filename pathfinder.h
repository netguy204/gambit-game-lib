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

typedef struct Path_ {
  struct TilePosition_ start;
  struct TilePosition_ end;
  int* steps;
  int nsteps;
} *Path;

// find POINT on PATH closest to POS and return the DIST to it.
void path_closest_point(Vector point, TileMap map, Path path, Vector pos, float* dist);

/*
 * Distances:
 *      c1   c2   c3   c4   c5
 * c1   x    1    2    3    4
 * c2   x    x    5    6    7
 * c3   x    x    x    8    9
 * c4   x    x    x    x   10
 * c5   x    x    x    x    x
 *
 * 4 -> 6
 * 5 -> 10
 */

typedef struct PairwisePaths_ {
  int npaths;
  int* lengths;
  struct Path_ paths[0];
} *PairwisePaths;

int* pathfinder_findpath(TileMap map, int p1, int p2, int* count);
PairwisePaths pathfinder_findpairwise(TileMap map, TilePosition positions, int npositions);
PathElement pathfinder_findpath2(TileMap map, int p1, int p2, int8_t* visited,
                                 int8_t key, BinaryHeap candidates, StackAllocator allocator);

#endif
