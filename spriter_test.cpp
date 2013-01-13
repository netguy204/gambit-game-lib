#include "spriter.h"
#include "spriteatlas.h"

int main(int argc, char *argv[])
{
  testlib_init();
  SpriteAtlas atlas = spriteatlas_load("resources/images_default", "png");
  Entity* ent = spriter_load("./out", atlas);
  return 0;
}
