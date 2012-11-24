#include "items.h"

#include <stdio.h>

int main(int argc, char *argv[])
{
  if(argc != 2) {
    fprintf(stderr, "usage: %s [input]\n", argv[0]);
    return 1;
  }

  listlib_init();
  items_init();

  items_load_xml(argv[1]);

  struct ComponentClass* alpha_class = componentclass_find("alpha-hull");
  ComponentInstance alpha = new(alpha_class);
  ComponentInstance laser = componentinstance_findchild(alpha, "alpha-laser");

  Resources_ fuel = {0, 0, 100, 0};
  push(alpha, fuel);

  int ii;
  for(ii = 0; ii < 10; ++ii) {
    update(alpha, 1.0f);
    tofile(alpha, stdout);
    activate(laser, FIRE);
  }

  delete(alpha);

  return 0;
}
