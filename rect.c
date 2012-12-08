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

void rect_centered(Rect rect, Vector pos, float w, float h) {
  float hw = w / 2.0f;
  float hh = h / 2.0f;
  rect->minx = pos->x - hw;
  rect->maxx = pos->x + hw;
  rect->miny = pos->y - hh;
  rect->maxy = pos->y + hh;
}
