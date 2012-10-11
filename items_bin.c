#include "items.h"

#include <stdio.h>

char* resources_str(Resources res) {
  static char buffer[80];
  int offset = 0;
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    offset += sprintf(&buffer[offset], "%c=%.0f ", resource_names[ii][0], res[ii]);
  }
  return buffer;
}

char* indentation(int num) {
  static char buffer[80];
  int ii;
  for(ii = 0; ii < num; ++ii) {
    buffer[ii] = ' ';
  }
  buffer[ii] = 0;
  return buffer;
}

void print_component(ComponentInstance inst, int indent) {
  printf("%s%s: s:%s ",
         indentation(indent), inst->klass->name,
         resources_str(inst->stats.storage));
  printf("c:%s ", resources_str(inst->stats.max_capacity));
  printf("pr:%s\n", resources_str(inst->stats.production_rates));

  DLLNode node = inst->children.head;
  while(node) {
    print_component(componentinstance_from_node(node), indent + 1);
    node = node->next;
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

  ComponentClass alpha_class = componentclass_find("alpha-hull");
  ComponentInstance alpha = componentinstance_make(alpha_class);
  ComponentInstance laser = componentinstance_findchild(alpha, "alpha-laser");

  Resources_ fuel = {0, 0, 100, 0};
  component_push(alpha, fuel);

  int ii;
  for(ii = 0; ii < 10; ++ii) {
    component_update(alpha);
    print_component(alpha, 0);
    component_activate(laser, FIRE);
  }

  componentinstance_free(alpha);

  return 0;
}
