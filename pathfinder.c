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

void pathelement_free(PathElement pe) {
  free(pe);
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
      goto end_check;
    }

    // have we found the goal?
    if(start == p2) {
      printf("found after %d steps\n", count);
      return elem;
    }

    visited[start] = key;
    count++;

    // TEMP: mark up the map
    //map->tiles[start] = 4;

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


  end_check:
    assert(1);
    // leak! pathelement_free(elem);
  }

  // no possible path
  return NULL;
}
