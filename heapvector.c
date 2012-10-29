#include "heapvector.h"
#include "config.h"

#include <stdio.h>
#include <assert.h>

void heapvector_init(HeapVector hv) {
  size_t init_alloc = 4096;
  hv->data_bytes = 0;
  hv->alloc_bytes = init_alloc;
  hv->data = malloc(init_alloc);
}

HeapVector heapvector_make() {
  HeapVector result = malloc(sizeof(struct HeapVector_));
  heapvector_init(result);

  return result;
}

void heapvector_clear(HeapVector hv) {
  hv->data_bytes = 0;
}

void heapvector_free(HeapVector hv) {
  free(hv->data);
  free(hv);
}

void heapvector_grow(HeapVector hv, size_t new_alloc) {
  hv->alloc_bytes = new_alloc;
  hv->data = realloc(hv->data, new_alloc);
  assert(hv->data);
}

void heapvector_push(HeapVector hv, void * data, size_t size) {
  size_t new_size = hv->data_bytes + size;

  if(new_size > hv->alloc_bytes) {
    size_t new_alloc = new_size * 2;
    heapvector_grow(hv, new_alloc);
  }

  assert(hv->data);
  memcpy(&hv->data[hv->data_bytes], data, size);
  hv->data_bytes = new_size;
}

void * heapvector_pop(HeapVector hv, size_t size) {
  hv->data_bytes -= size;
  return &hv->data[hv->data_bytes];
}

BinaryHeap binaryheap_make(size_t elem_size, BinaryHeapComparison comparison) {
  BinaryHeap result = malloc(sizeof(struct BinaryHeap_));
  heapvector_init(&result->vector);
  result->comparison = comparison;
  result->element_size = elem_size;
  result->tmp_swap = malloc(elem_size);
  return result;
}

void binaryheap_free(BinaryHeap bh) {
  free(bh->tmp_swap);
  free(bh->vector.data);
  free(bh);
}

void binaryheap_swap(BinaryHeap bh, int posa, int posb) {
  size_t sz = bh->element_size;
  int a = sz * (posa - 1);
  int b = sz * (posb - 1);
  memcpy(bh->tmp_swap, &bh->vector.data[a], sz);
  memcpy(&bh->vector.data[a], &bh->vector.data[b], sz);
  memcpy(&bh->vector.data[b], bh->tmp_swap, sz);
}

int binaryheap_compare(BinaryHeap bh, int posa, int posb) {
  size_t sz = bh->element_size;
  int a = sz * (posa - 1);
  int b = sz * (posb - 1);

  return bh->comparison(&bh->vector.data[a], &bh->vector.data[b]);
}

int binaryheap_size(BinaryHeap bh) {
  return bh->vector.data_bytes / bh->element_size;
}

void binaryheap_insert(BinaryHeap bh, void* value) {
  // inserts start at the bottom and bubble up
  heapvector_push((HeapVector)bh, value, bh->element_size);

  // positions are indices + 1
  int test_pos = binaryheap_size(bh);

  while(test_pos != 1) {
    int parent = test_pos / 2;
    if(binaryheap_compare(bh, test_pos, parent) <= 0) {
      binaryheap_swap(bh, test_pos, parent);
      test_pos = parent;
    } else {
      // we're done!
      break;
    }
  }
}

void* binaryheap_top(BinaryHeap bh) {
  return bh->vector.data;
}

void binaryheap_remove_top(BinaryHeap bh) {
  // remove the top by inserting the bottom element at the top and
  // letting it bubble down
  void* value = heapvector_pop((HeapVector)bh, bh->element_size);
  memcpy(bh->vector.data, value, bh->element_size);

  size_t sz = binaryheap_size(bh);

  int v = 1;
  int u;
  while(1) {
    u = v;

    int left_child = 2 * u;
    int right_child = left_child + 1;
    if(right_child <= sz) {
      // need to pick the smaller of our two children to swap with
      if(binaryheap_compare(bh, u, left_child) >= 0) v = left_child;
      if(binaryheap_compare(bh, v, right_child) >= 0) v = right_child;
    } else if(left_child <= sz) {
      // only have one child to consider
      if(binaryheap_compare(bh, u, left_child) >= 0) v = left_child;
    }

    if(u != v) {
      binaryheap_swap(bh, u, v);
    } else {
      break; // done!
    }
  }
}
