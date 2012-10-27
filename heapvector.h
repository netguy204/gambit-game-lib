#ifndef HEAPVECTOR_H
#define HEAPVECTOR_H

#include <stdlib.h>
#include <memory.h>

typedef struct HeapVector_ {
  size_t data_bytes;
  size_t alloc_bytes;
  char data[0];
} *HeapVector;

HeapVector heapvector_make(size_t init_alloc);
void heapvector_free(HeapVector hv);
void heapvector_clear(HeapVector hv);
HeapVector heapvector_push(HeapVector hv, void * data, size_t size);
void * heapvector_pop(HeapVector hv, size_t size);

#define HV_PUSH_VALUE(hv, type, value) (hv = heapvector_push(hv, &value, sizeof(type)))
#define HV_POP_VALUE(hv, type) (*(type*)(heapvector_pop(hv, sizeof(type))))
#define HV_SIZE(hv, type) (hv->data_bytes / sizeof(type))
#define HV_GET(hv, type, index) ((type*)&(hv->data[index * sizeof(type)]))

#endif
