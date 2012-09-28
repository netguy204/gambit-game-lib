#ifndef RECT_H
#define RECT_H

typedef struct Rect_ {
  float minx, miny, maxx, maxy;
} *Rect;

float rect_width(Rect rect);
float rect_height(Rect rect);
int rect_intersect(Rect a, Rect b);

#endif
