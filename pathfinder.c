#include "pathfinder.h"
#include "config.h"

#include <stdint.h>
#include <memory.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

PathElement pathelement_make(StackAllocator allocator) {
  return stack_allocator_alloc(allocator, sizeof(struct PathElement_));
}

void vector_path_direction(Vector dir, TileMap map, Path path, int test0, int pathdir) {
  int test1 = path_next_idx(path, test0, pathdir);
  if(test1 == -1) {
    int temp = test0;
    test0 = path_next_idx(path, test0, -pathdir);
    test1 = temp;
  }

  assert(test1 >= 0 && test1 < path->nsteps);

  struct Vector_ p0;
  struct Vector_ p1;
  vector_tilecenter(&p0, map, path->steps[test0]);
  vector_tilecenter(&p1, map, path->steps[test1]);
  vector_sub(dir, &p1, &p0);
  vector_norm(dir, dir);
}

int path_end_idx(Path path, int pathdir) {
  if(pathdir > 0) {
    return path->nsteps - 1;
  } else {
    return 0;
  }
}

int path_begin_idx(Path path, int pathdir) {
  if(pathdir > 0) {
    return 0;
  } else {
    return path->nsteps - 1;
  }
}

void vector_path_end(Vector end, Path path, TileMap map, int pathdir) {
  int end_idx = path_end_idx(path, pathdir);
  vector_tilecenter(end, map, path->steps[end_idx]);
}

void vector_path_begin(Vector begin, Path path, TileMap map, int pathdir) {
  int begin_idx = path_begin_idx(path, pathdir);
  vector_tilecenter(begin, map, path->steps[begin_idx]);
}

int path_next_idx(Path path, int current_idx, int pathdir) {
  assert(current_idx != -1);

  int next = current_idx + pathdir;
  if(next >= 0 && next < path->nsteps) return next;
  return -1;
}

int pathinstance_next_idx(PathInstance pi) {
  return path_next_idx(pi->path, pi->pathpos, pi->pathdir);
}

void vector_pathinstance_direction(Vector dir, TileMap map, PathInstance pi) {
  vector_path_direction(dir, map, pi->path, pi->pathpos, pi->pathdir);
}

int pathinstance_end_idx(PathInstance pi) {
  return path_end_idx(pi->path, pi->pathdir);
}

int pathinstance_begin_idx(PathInstance pi) {
  return path_begin_idx(pi->path, pi->pathdir);
}

void vector_pathinstance_end(Vector end, PathInstance pi, TileMap map) {
  vector_path_end(end, pi->path, map, pi->pathdir);
}

void vector_pathinstance_begin(Vector begin, PathInstance pi, TileMap map) {
  vector_path_begin(begin, pi->path, map, pi->pathdir);
}

int path_next_closest_point(Vector point, TileMap map, PathInstance pi, Vector pos, float* dist) {
  float closest_dist2 = INFINITY;
  int origin_idx = -1;

  int ii;
  int range;
  for(ii = pi->pathpos; ii != -1; ii = path_next_idx(pi->path, ii, pi->pathdir)) {
    int idx = ii;

    range = abs(idx - pi->pathpos);
    if(range > pi->max_skip_range) break;

    // if we run out of path then back off one. this ensures that
    // we'll get at least one hit if we're queried at the end of the
    // path
    int next_idx = path_next_idx(pi->path, idx, pi->pathdir);
    if(next_idx == -1) {
      next_idx = idx;
      idx = path_next_idx(pi->path, next_idx, -pi->pathdir);
    }

    int test0 = pi->path->steps[idx];
    int test1 = pi->path->steps[next_idx];
    struct Vector_ path_center0;
    struct Vector_ path_center1;
    struct Vector_ tangent;
    struct Vector_ projection;
    struct Vector_ projection_pt;
    struct Vector_ p0_to_pos;
    Vector refpoint;

    float pt0pt1_dist;
    float pathdot;

    vector_tilecenter(&path_center0, map, test0);
    vector_tilecenter(&path_center1, map, test1);
    vector_sub(&tangent, &path_center1, &path_center0);
    vector_sub(&p0_to_pos, pos, &path_center0);
    vector_norm(&tangent, &tangent);
    pt0pt1_dist = sqrtf(vector_dist2(&path_center0, &path_center1));

    // project the query point onto the path
    pathdot = vector_project2(&projection, &p0_to_pos, &tangent);

    if(pathdot < 0) {
      // if the point is behind the starting point then compute distance
      // from the starting point
      refpoint = &path_center0;
    } else if(pathdot > pt0pt1_dist) {
      // if the point is beyond the ending point then compute distance
      // from the ending point
      refpoint = &path_center1;
    } else {
      // if the point is between the start and the end then compute
      // the projection point and find the distance from that
      vector_add(&projection_pt, &path_center0, &projection);
      refpoint = &projection_pt;
    }

    // compute and test the distance
    float dist2 = vector_dist2(pos, refpoint);
    if(dist2 < closest_dist2) {
      closest_dist2 = dist2;
      *point = *refpoint;
      origin_idx = idx;
    }
  }

  assert(dist);
  *dist = sqrtf(closest_dist2);

  return origin_idx;
}

int pathelement_compare(void *a, void *b) {
  PathElement pe_a = *(PathElement*)a;
  PathElement pe_b = *(PathElement*)b;

  if(pe_a->score < pe_b->score) return -1;
  if(pe_a->score > pe_b->score) return 1;
  return 0;
}

int pathfinder_heuristic(TileMap map, int p1, int p2) {
  struct TilePosition_ tp1, tp2;
  tileposition_tilemap(&tp1, map, p1);
  tileposition_tilemap(&tp2, map, p2);

  int dx = tp1.x - tp2.x;
  int dy = tp1.y - tp2.y;

  // manhatten distance
  return abs(dx) + abs(dy); //sqrt(dx*dx + dy*dy);
}

PathElement pathfinder_make_element(StackAllocator allocator, TileMap map,
                                    PathElement predecessor, int p, int target) {
  PathElement result = pathelement_make(allocator);
  result->predecessor = predecessor;
  result->index = p;
  if(predecessor) {
    result->distance = predecessor->distance + 1;
  } else {
    result->distance = 0;
  }
  result->score = result->distance + pathfinder_heuristic(map, p, target);
  return result;
}

int* pathfinder_findpath(TileMap map, int p1, int p2, int* count) {
  int sz = tilemap_size(map);
  int8_t* visited = malloc(sz);
  memset(visited, 0, sz);

  StackAllocator allocator = stack_allocator_make(sizeof(struct PathElement_) * sz,
                                                  "pathfinder_temp_allocator");
  BinaryHeap candidates = binaryheap_make(sizeof(PathElement), pathelement_compare);

  PathElement result = pathfinder_findpath2(map, p1, p2, visited, 1, candidates, allocator);

  int* path = NULL;
  if(result) {
    path = malloc(sizeof(int) * (result->distance + 1));
    int ii = 0;
    while(result) {
      path[ii++] = result->index;
      result = result->predecessor;
    }
    *count = ii;
  } else {
    *count = 0;
  }

  binaryheap_free(candidates);
  stack_allocator_release(allocator);
  free(visited);

  return path;
}

PairwisePaths pathfinder_findpairwise(TileMap map, TilePosition positions, int npositions) {
  int npairwise = (npositions * (npositions - 1)) / 2;
  PairwisePaths result = malloc(sizeof(struct PairwisePaths_) + npairwise * sizeof(struct Path_));
  result->npaths = npairwise;
  result->lengths = malloc(sizeof(int) * npairwise);

  int sz = tilemap_size(map);
  int8_t* visited = malloc(sz);
  memset(visited, 0, sz);

  StackAllocator allocator = stack_allocator_make(sizeof(struct PathElement_) * sz,
                                                  "pathfinder_pairwise_allocator");

  BinaryHeap candidates = binaryheap_make(sizeof(PathElement), pathelement_compare);

  int ii, jj;
  int idx = 0;
  int8_t key = 0;

  for(ii = 0; ii < npositions; ++ii) {
    for(jj = ii + 1; jj < npositions; ++jj) {
      // get our working memory ready to go again
      key++;

      // if our visited key wrapped, clear memory again
      if(key == 0) {
        memset(visited, 0, sz);
        key = 1;
      }

      heapvector_clear((HeapVector)candidates);
      stack_allocator_freeall(allocator);

      TilePosition p1 = &positions[ii];
      TilePosition p2 = &positions[jj];

      int idx1 = tilemap_index(map, p1);
      int idx2 = tilemap_index(map, p2);

      PathElement element = pathfinder_findpath2(map, idx1, idx2, visited, key, candidates, allocator);

      int length = element->distance + 1;
      result->lengths[idx] = length;

      int* path = malloc(sizeof(int) * length);

      int kk = 0;
      while(element) {
        path[kk++] = element->index;
        element = element->predecessor;
      }

      result->paths[idx].start = *p2;
      result->paths[idx].end = *p1;
      result->paths[idx].steps = path;
      result->paths[idx].nsteps = length;
      idx++;
    }
  }

  binaryheap_free(candidates);
  stack_allocator_release(allocator);
  free(visited);

  return result;
}

PathElement pathfinder_findpath2(TileMap map, int p1, int p2, int8_t* visited,
                                 int8_t key, BinaryHeap candidates, StackAllocator allocator) {
  int sz = tilemap_size(map);
  int row = map->width_IT;

  PathElement start_elem = pathfinder_make_element(allocator, map, NULL, p1, p2);
  binaryheap_insert(candidates, &start_elem);

  int count = 0;
  while(binaryheap_size(candidates) > 0) {
    PathElement elem = *(PathElement*)binaryheap_top(candidates);
    binaryheap_remove_top(candidates);

#define IS_CANDIDATE(idx)                               \
    (idx < sz && idx >= 0                               \
     && visited[idx] != key                             \
     && (map->tile_specs[map->tiles[idx]].bitmask & TILESPEC_PASSABLE))

    int start = elem->index;
    if(visited[start] == key) {
      // we already found a better way to this point
      continue;
    }

    // have we found the goal?
    if(start == p2) {
      return elem;
    }

    visited[start] = key;
    count++;

    int above = start + row;
    if(IS_CANDIDATE(above)) {
      PathElement pelem = pathfinder_make_element(allocator, map, elem, above, p2);
      binaryheap_insert(candidates, &pelem);
    }

    int below = start - row;
    if(IS_CANDIDATE(below)) {
      PathElement pelem = pathfinder_make_element(allocator, map, elem, below, p2);
      binaryheap_insert(candidates, &pelem);
    }

    int left = start - 1;
    if(IS_CANDIDATE(left)) {
      PathElement pelem = pathfinder_make_element(allocator, map, elem, left, p2);
      binaryheap_insert(candidates, &pelem);
    }

    int right = start + 1;
    if(IS_CANDIDATE(right)) {
      PathElement pelem = pathfinder_make_element(allocator, map, elem, right, p2);
      binaryheap_insert(candidates, &pelem);
    }
  }

  // no possible path
  return NULL;
}
