#include "testlib.h"

SDL_Surface* load_image(char * file) {
  SDL_Surface *image;
  image = IMG_Load(file);
  if(image == NULL) {
    fprintf(stderr, "failed to load %s\n", file);
    return NULL;
  }

  return SDL_DisplayFormatAlpha(image);
}

void blit_image(SDL_Surface* target, SDL_Surface* src, int x, int y) {
  SDL_Rect dest;
  dest.x = x;
  dest.y = y;
  dest.w = src->w;
  dest.h = src->h;
  SDL_BlitSurface(src, NULL, target, &dest);
  SDL_UpdateRects(target, 1, &dest);
}
