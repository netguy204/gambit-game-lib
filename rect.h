#ifndef RECT_H
#define RECT_H

#include "vector.h"

typedef struct Rect_ {
  float minx, miny, maxx, maxy;
} *Rect;

float rect_width(Rect rect);
float rect_height(Rect rect);
int rect_intersect(Rect a, Rect b);
void rect_centered(Rect rect, Vector pos, float w, float h);
void rect_offset(Rect output, Rect input, Vector offset);
void rect_center(Vector center, Rect rect);
void rect_scaled(Rect output, Rect input, float w, float h);

typedef struct ColoredRect_ {
  struct Rect_ rect;
  float color[4];
} *ColoredRect;


#endif
