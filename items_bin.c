#include "items.h"

#include <stdio.h>

void tryAdd(ComponentAssembly assembly, ComponentInstance component) {
  if(!assembly_insert(assembly, component)) {
    tofile(component, stderr);
    fprintf(stderr, ": error adding component to assembly\n");
  }
}

int main(int argc, char *argv[])
{
  if(argc != 2) {
    fprintf(stderr, "usage: %s [input]\n", argv[0]);
    return 1;
  }

  listlib_init();
  items_init();

  items_load_xml(argv[1]);

  ComponentInstance alpha = new(componentclass_find("alpha-hull"));

  ComponentAssembly assembly = new(ComponentAssemblyObject, alpha);

  // add some stuff...
  tryAdd(assembly, new(componentclass_find("fuel-storage")));
  tryAdd(assembly, new(componentclass_find("alpha-laser")));
  tryAdd(assembly, new(componentclass_find("alpha-reactor")));

  int ii;
  for(ii = 0; ii < 10; ++ii) {
    update(assembly, 1.0f);
    tofile(assembly, stdout);
    activate(assembly, ACTIVATION_FIRE);
  }

  delete(assembly);

  return 0;
}
