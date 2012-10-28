#include "heapvector.h"
#include "config.h"

#include <stdio.h>
#include <assert.h>

HeapVector heapvector_make() {
  size_t init_alloc = 4096;
  HeapVector result = malloc(sizeof(struct HeapVector_));
  result->data_bytes = 0;
  result->alloc_bytes = init_alloc;
  result->data = malloc(init_alloc);

  return result;
}

void heapvector_clear(HeapVector hv) {
  hv->data_bytes = 0;
}

void heapvector_free(HeapVector hv) {
  free(hv->data);
  free(hv);
}

void heapvector_push(HeapVector hv, void * data, size_t size) {
  size_t new_size = hv->data_bytes + size;

  if(new_size > hv->alloc_bytes) {
    size_t new_alloc = new_size * 2;

    hv->data = realloc(hv->data, new_alloc);
    assert(data);

    hv->alloc_bytes = new_alloc;
  }

  memcpy(&hv->data[hv->data_bytes], data, size);
  hv->data_bytes = new_size;
}

void * heapvector_pop(HeapVector hv, size_t size) {
  hv->data_bytes -= size;
  return &hv->data[hv->data_bytes];
}
