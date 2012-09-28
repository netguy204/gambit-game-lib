#include "rect.h"

float rect_width(Rect rect) {
  return rect->maxx - rect->minx;
}

float rect_height(Rect rect) {
  return rect->maxy - rect->miny;
}

int rect_intersect(Rect a, Rect b) {
  if (a->maxx < b->minx) return 0;
  if (a->minx > b->maxx) return 0;
  if (a->maxy < b->miny) return 0;
  if (a->miny > b->maxy) return 0;
  return 1;
}
