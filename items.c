#include "items.h"
#include "xmltools.h"

#include "config.h"

#include <string.h>

struct DLL_ classes;

void items_init() {
  dll_zero(&classes);
}

void resources_zero(Resources attr) {
  memset(attr, 0, sizeof(Resources_));
}

void componentclass_init(ComponentClass klass, char *name) {
  strncpy(klass->name, name, ITEM_MAX_NAME);
}

ComponentClass componentclass_make_(char *name, size_t size) {
  ComponentClass klass = malloc(size);
  memset(klass, 0, size);

  componentclass_init(klass, name);

  dll_add_head(&classes, (DLLNode)klass);
  return klass;
}

ComponentClass componentclass_find(char *name) {
  ComponentClass node = (ComponentClass)classes.head;
  while(node) {
    if(strncmp(node->name, name, ITEM_MAX_NAME) == 0) {
      return node;
    }
    node = (ComponentClass)node->node.next;
  }
  return NULL;
}

float* resource_get_name_ptr(Resources attr, const char* name) {
  if(streq(name, "heat")) {
    return &attr[HEAT];
  } else if(streq(name, "power")) {
    return &attr[POWER];
  } else if(streq(name, "fuel")) {
    return &attr[FUEL];
  } else {
    fprintf(stderr, "unrecognized Attribute: %s\n", name);
    exit(1);
  }
}

void resource_set_name(Resources attr, const char* name, float value) {
  *resource_get_name_ptr(attr, name) = value;
}

void component_fill_from_xml(ComponentClass klass, xmlNode* child) {
  if(streq(child->name, "produces")) {
    resource_set_name(klass->production_rates, node_attr(child, "name", "error"),
                       atof(node_attr(child, "rate", "error")));
  } else if(streq(child->name, "consumes")) {
    resource_set_name(klass->consumption_rates, node_attr(child, "name", "error"),
                       atof(node_attr(child, "rate", "error")));
  } else if(streq(child->name, "maxcapacity")) {
    resource_set_name(klass->max_capacity, node_attr(child, "name", "error"),
                       atof(node_attr(child, "value", "error")));
  } else {
    fprintf(stderr, "unrecognized item subnode: %s\n", child->name);
    exit(1);
  }
}

int component_push(ComponentInstance comp, Resources resources) {
  if(comp) {
    return comp->klass->push(comp, resources);
  } else {
    return 0;
  }
}

int component_pull(ComponentInstance comp, Resources resources) {
  if(comp) {
    return comp->klass->pull(comp, resources);
  } else {
    return 0;
  }
}

void component_update(ComponentInstance comp) {
  if(comp) comp->klass->update(comp);
}

void component_activate(ComponentInstance comp, Activation activation) {
  if(comp) comp->klass->activate(comp, activation);
}

float component_storage_available(ComponentInstance component, Resource resource) {
  return component->max_capacity[resource] - component->storage[resource];
}

int component_pullimpl(ComponentInstance component, Resources resources) {
  // if we can, pull all the resources requested from ourselves
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    if(component->storage[ii] < resources[ii]) break;
  }

  // if we didn't find it all, ask our parent
  if(ii != MAX_RESOURCE) return component_pull(component->parent, resources);

  // pull it all and return success
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    component->storage[ii] -= resources[ii];
  }
  return 1;
}

int component_pushimpl(ComponentInstance component, Resources resources) {
  // take what we can of the push
  int ii = 0;
  int all_taken = 1;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    float cantake = component_storage_available(component, ii);
    if(cantake < resources[ii]) all_taken = 0;

    float willtake = MIN(cantake, resources[ii]);
    component->storage[ii] += willtake;
    resources[ii] -= willtake;
  }

  // if we absorbed it all then we're done
  if(all_taken) return 1;

  // if not, pass to our children
  DLLNode childnode = component->children.head;
  while(childnode) {
    ComponentInstance child = componentinstance_from_node(childnode);
    if(component_push(child, resources)) return 1;
    childnode = childnode->next;
  }

  // if we make it here then we weren't able to push
  // everything... interesting consequences?
  return 0;
}

void item_create_from_xml(xmlNode* child) {
  ComponentClass klass = componentclass_make(node_attr(child, "name", "error"), struct ComponentClass_);
  klass->pull = component_pullimpl;
  klass->push = component_pushimpl;
  klass->update = NULL;
  klass->activate = NULL;

  child = child->children;
  while(child) {
    if(child->type == XML_ELEMENT_NODE) {
      component_fill_from_xml(klass, child);
    }

    child = child->next;
  }

  // make sure we at least have enough capacity to function
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    klass->max_capacity[ii] =
      MAX(klass->max_capacity[ii],
          MAX(klass->production_rates[ii],
              klass->consumption_rates[ii]));
  }
}

void system_create_from_xml(xmlNode* child) {
}

int main(int argc, char *argv[])
{
  if(argc != 2) {
    fprintf(stderr, "usage: %s [input]\n", argv[0]);
    return 1;
  }

  char * input_xml = argv[1];
  xmlNode* children = xml_rootchildren(input_xml);
  xmlNode* child = children;
  while(child) {
    if(child->type == XML_ELEMENT_NODE) {
      if(streq(child->name, "item")) {
        item_create_from_xml(child);
      } else if(streq(child->name, "system")) {
        system_create_from_xml(child);
      }

      printf("name: %s\n", child->name);
    }

    child = child->next;
  }

  xml_free(children);

  return 0;
}
