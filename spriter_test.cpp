#include "spriter.h"
#include "spriteatlas.h"

int main(int argc, char *argv[])
{
  testlib_init();
  SpriteAtlas atlas = spriteatlas_load("resources/images_default", "png");
  Entity* ent = spriter_load("./out", atlas);
  Animation* anim = spriter_find(ent, "First Animation");
  Vector_ pos = {0.0f, 0.0f};
  BaseSprite list = spriter_append(NULL, anim, &pos, 50);
  return 0;
}
