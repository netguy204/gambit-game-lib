#ifndef HEAPVECTOR_H
#define HEAPVECTOR_H

#include <stdlib.h>
#include <memory.h>

typedef struct HeapVector_ {
  size_t data_bytes;
  size_t alloc_bytes;
  char *data;
} *HeapVector;

HeapVector heapvector_make();
void heapvector_free(HeapVector hv);
void heapvector_clear(HeapVector hv);
void heapvector_push(HeapVector hv, void * data, size_t size);
void * heapvector_pop(HeapVector hv, size_t size);

#define HV_PUSH_VALUE(hv, type, value) (heapvector_push(hv, &value, sizeof(type)))
#define HV_POP_VALUE(hv, type) (*(type*)(heapvector_pop(hv, sizeof(type))))
#define HV_SIZE(hv, type) (hv->data_bytes / sizeof(type))
#define HV_GET(hv, type, index) ((type*)&(hv->data[index * sizeof(type)]))

typedef int(*BinaryHeapComparison)(void* a, void* b);

typedef struct BinaryHeap_ {
  struct HeapVector_ vector;
  BinaryHeapComparison comparison;
  size_t element_size;
  char* tmp_swap;
} *BinaryHeap;

BinaryHeap binaryheap_make(size_t elem_size, BinaryHeapComparison comparison);
void binaryheap_free(BinaryHeap bh);
int binaryheap_size(BinaryHeap bh);
void binaryheap_insert(BinaryHeap bh, void* value);
void* binaryheap_top(BinaryHeap bh);
void binaryheap_remove_top(BinaryHeap bh);

#endif
