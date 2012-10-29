#include "heapvector.h"

#include <stdio.h>

int int_compare(void* a, void* b) {
  int *ap = (int*)a;
  int *bp = (int*)b;

  if(*ap < *bp) {
    return -1;
  } else if (*ap > *bp) {
    return 1;
  } else {
    return 0;
  }
}


int main(int argc, char *argv[])
{
  BinaryHeap bh = binaryheap_make(sizeof(int), int_compare);

  int value = 4;
  binaryheap_insert(bh, &value);

  value = 10;
  binaryheap_insert(bh, &value);

  value = 3;
  binaryheap_insert(bh, &value);

  value = 15;
  binaryheap_insert(bh, &value);

  value = 27;
  binaryheap_insert(bh, &value);

  while(binaryheap_size(bh) > 0) {
    int *vp = binaryheap_top(bh);
    printf("value: %d\n", *vp);
    binaryheap_remove_top(bh);
  }

  return 0;
}
