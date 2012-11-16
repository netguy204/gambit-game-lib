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
  int heuristic;
  int index;
} *PathElement;

PathElement pathelement_make(StackAllocator allocator);

typedef struct Path_ {
  struct TilePosition_ start;
  struct TilePosition_ end;
  int* steps;
  int nsteps;
} *Path;

void vector_path_direction(Vector dir, TileMap map, Path path, int test0, int pathdir);
int path_end_idx(Path path, int pathdir);
int path_begin_idx(Path path, int pathdir);
void vector_path_end(Vector end, Path path, TileMap map, int pathdir);
void vector_path_begin(Vector begin, Path path, TileMap map, int pathdir);

typedef struct PathInstance_ {
  Path path;
  int pathdir;
  int pathpos;
  int max_skip_range;
} *PathInstance;

// -1 on fall off the path
int path_next_idx(Path path, int current_idx, int pathdir);
int pathinstance_next_idx(PathInstance pi);
void vector_pathinstance_direction(Vector dir, TileMap map, PathInstance pi);
int pathinstance_end_idx(PathInstance pi);
int pathinstance_begin_idx(PathInstance pi);
void vector_pathinstance_end(Vector end, PathInstance pi, TileMap map);
void vector_pathinstance_begin(Vector begin, PathInstance pi, TileMap map);

// find POINT on PATH closest to POS and return the DIST to it.
int path_next_closest_point(Vector point, TileMap map, PathInstance pi, Vector pos, float* dist);

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
