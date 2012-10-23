#include <stdio.h>

#include "heapvector.h"
#include "config.h"

HeapVector heapvector_make(size_t init_alloc) {
  size_t size = sizeof(struct HeapVector_) + init_alloc;
  size = NEXT_ALIGNED_SIZE(size);

  HeapVector result = malloc(size);
  result->data_bytes = 0;
  result->alloc_bytes = size - sizeof(struct HeapVector_);
  return result;
}

void heapvector_free(HeapVector hv) {
  free(hv);
}

HeapVector heapvector_push(HeapVector hv, void * data, size_t size) {
  size_t new_size = hv->data_bytes + size;

  if(new_size > hv->alloc_bytes) {
    size_t new_alloc = sizeof(struct HeapVector_) + (hv->alloc_bytes * 2);
    new_alloc = NEXT_ALIGNED_SIZE(new_alloc);

    // i'm not sure this is correct. I used to get crashes here until
    // I aligned the allocation sizes...
    hv = realloc(hv, new_alloc);
    hv->alloc_bytes = new_alloc - sizeof(struct HeapVector_);
  }
  memcpy(&hv->data[hv->data_bytes], data, size);
  hv->data_bytes = new_size;
  return hv;
}

void * heapvector_pop(HeapVector hv, size_t size) {
  hv->data_bytes -= size;
  return &hv->data[hv->data_bytes];
}
