#include "items.h"
#include "xmltools.h"
#include "memory.h"

#include "config.h"

#include <string.h>
#include <dlfcn.h>

struct DLL_ classes;
void* self_handle;

void items_init() {
  dll_zero(&classes);
  self_handle = dlopen(NULL, RTLD_LAZY);
}

void resources_zero(Resources attr) {
  memset(attr, 0, sizeof(Resources_));
}

void resources_add(Resources target, Resources a, Resources b) {
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    target[ii] = a[ii] + b[ii];
  }
}

void resources_sub(Resources target, Resources a, Resources b) {
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    target[ii] = a[ii] - b[ii];
  }
}

void resources_assign(Resources target, Resources source) {
  memcpy(target, source, sizeof(Resources_));
}

void componentclass_init(ComponentClass klass, char *name) {
  strncpy(klass->name, name, ITEM_MAX_NAME);
  klass->name[ITEM_MAX_NAME - 1] = 0;
  klass->quality = 1.0f;
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
    if(strncmp(node->name, name, ITEM_MAX_NAME - 1) == 0) {
      return node;
    }
    node = (ComponentClass)node->node.next;
  }
  return NULL;
}

void componentinstance_addchild(ComponentInstance parent, ComponentInstance child) {
  SAFETY(if(child->parent) fail_exit("child already has a parent"));
  dll_add_head(&parent->children, &child->node);
  child->parent = parent;
}

void componentinstance_removechild(ComponentInstance child) {
  dll_remove(&child->parent->children, &child->node);
  child->parent = NULL;
}

ComponentInstance componentinstance_make(ComponentClass klass) {
  ComponentInstance inst = malloc(sizeof(struct ComponentInstance_));
  memset(inst, 0, sizeof(struct ComponentInstance_));

  inst->klass = klass;
  resources_assign(inst->max_capacity, klass->max_capacity);
  resources_assign(inst->production_rates, klass->production_rates);
  inst->quality = klass->quality;

  LLNode child = klass->subcomponents;
  ComponentClass child_klass;
  while((child_klass = llentry_nextvalue(&child))) {
    ComponentInstance child_inst = componentinstance_make(child_klass);
    componentinstance_addchild(inst, child_inst);
  }

  return inst;
}

void componentinstance_free(ComponentInstance comp) {
  SAFETY(if(comp->parent) fail_exit("freeing component that has a parent"));

  DLLNode node = comp->children.head;

  // free remaining children
  while(node) {
    ComponentInstance child = componentinstance_from_node(node);
    child->parent = NULL;
    componentinstance_free(child);
    node = node->next;
  }

  free(comp);
}

int component_push(ComponentInstance comp, Resources resources) {
  if(comp && comp->klass->push) {
    return comp->klass->push(comp, resources);
  } else {
    return 0;
  }
}

int component_pull(ComponentInstance comp, Resources resources) {
  if(comp && comp->klass->pull) {
    return comp->klass->pull(comp, resources);
  } else {
    return 0;
  }
}

void component_update(ComponentInstance comp) {
  if(comp && comp->klass->update) comp->klass->update(comp);
}

void component_activate(ComponentInstance comp, Activation activation) {
  if(comp && comp->klass->activate) comp->klass->activate(comp, activation);
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

void system_updateimpl(ComponentInstance component) {
  // update our children
  DLLNode node = component->children.head;
  while(node) {
    ComponentInstance child = componentinstance_from_node(node);
    component_update(child);
    node = node->next;
  }

  // find our deficit materials / do we have capacity for what we'll
  // produce?
  Resources_ deficit;
  int have_deficit = 0;
  int have_capacity = 1;
  int ii;

  resources_zero(deficit);

  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    if(component->production_rates[ii] < 0) {
      deficit[ii] = -component->production_rates[ii];
      have_deficit = 1;
    } else if(component_storage_available(component, ii) < component->production_rates[ii]) {
      have_capacity = 0;
    }
  }

  // don't produce if we can't store
  if(!have_capacity) return;

  // if we have a deficit then we'll need to request from our parent
  Resources_ request;
  resources_assign(request, deficit);

  if(!have_deficit || component_pull(component->parent, request)) {
    // add in whatever we requested
    resources_add(component->storage, component->storage, deficit);

    // add in the result of doing our thing
    resources_add(component->storage, component->storage, component->production_rates);
  }
}

void heatsink_update(ComponentInstance component) {
  // we operate on our parent. give up if there is none
  if(!component->parent) return;

  // take away quality percent of the heat stored by our parent
  component->parent->storage[HEAT] -=
    component->quality * component->parent->storage[HEAT];
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

void resource_add_name(Resources attr, const char* name, float value) {
  *resource_get_name_ptr(attr, name) += value;
}

void resource_sub_name(Resources attr, const char* name, float value) {
  *resource_get_name_ptr(attr, name) -= value;
}

void* find_function(char* fnname) {
  void *fn = dlsym(self_handle, fnname);
  if(fn == NULL) {
    fprintf(stderr, "couldn't find function name: %s\n", fnname);
    exit(1);
  }
  return fn;
}

void component_fill_from_xml(ComponentClass klass, xmlNode* child) {
  if(streq(child->name, "produces")) {
    resource_add_name(klass->production_rates, node_attr(child, "name", "error"),
                      atof(node_attr(child, "rate", "error")));
  } else if(streq(child->name, "consumes")) {
    resource_sub_name(klass->production_rates, node_attr(child, "name", "error"),
                      atof(node_attr(child, "rate", "error")));
  } else if(streq(child->name, "maxcapacity")) {
    resource_add_name(klass->max_capacity, node_attr(child, "name", "error"),
                      atof(node_attr(child, "value", "error")));
  } else if(streq(child->name, "quality")) {
    klass->quality = atof(node_attr(child, "value", "error"));
  } else if(streq(child->name, "updatefn")) {
    klass->update = find_function(node_attr(child, "name", "error"));
  } else {
    fprintf(stderr, "unrecognized item subnode: %s\n", child->name);
    exit(1);
  }
}

void item_create_from_xml(xmlNode* child) {
  ComponentClass klass = componentclass_make(node_attr(child, "name", "error"), struct ComponentClass_);
  klass->pull = component_pullimpl;
  klass->push = component_pushimpl;

  child = child->children;
  while(child) {
    if(child->type == XML_ELEMENT_NODE) {
      component_fill_from_xml(klass, child);
    }

    child = child->next;
  }
}

void system_fill_from_xml(ComponentClass klass, xmlNode* child) {
  if(streq(child->name, "component")) {
    char* comp_name = node_attr(child, "name", "error");
    ComponentClass comp_klass = componentclass_find(comp_name);
    if(comp_klass == NULL) {
      fprintf(stderr, "couldn't find component name: %s\n", comp_name);
      exit(1);
    }

    // we are the sum of our parts
    resources_add(klass->max_capacity, klass->max_capacity, comp_klass->max_capacity);
    resources_add(klass->production_rates, klass->production_rates, comp_klass->production_rates);

    llentry_add(&klass->subcomponents, comp_klass);
  } else {
    // but we can explicitly override capacities
    component_fill_from_xml(klass, child);
  }
}

void system_create_from_xml(xmlNode* child) {
  ComponentClass klass = componentclass_make(node_attr(child, "name", "error"), struct ComponentClass_);
  klass->pull = component_pullimpl;
  klass->push = component_pushimpl;
  klass->update = system_updateimpl;

  child = child->children;
  while(child) {
    if(child->type == XML_ELEMENT_NODE) {
      system_fill_from_xml(klass, child);
    }

    child = child->next;
  }
}

char* resources_str(Resources res) {
  static char buffer[80];
  int offset = 0;
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    offset += sprintf(&buffer[offset], "%s=%.0f ", resource_names[ii], res[ii]);
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
         resources_str(inst->storage));
  printf("c:%s ", resources_str(inst->max_capacity));
  printf("pr:%s\n", resources_str(inst->production_rates));

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

  ComponentClass alpha_class = componentclass_find("alpha-power");
  ComponentInstance alpha = componentinstance_make(alpha_class);

  ComponentClass tank_class = componentclass_find("fuel-storage");
  ComponentInstance tank = componentinstance_make(tank_class);
  tank->storage[FUEL] = 100;

  componentinstance_addchild(tank, alpha);

  component_update(alpha);
  print_component(tank, 0);
  component_update(alpha);
  print_component(tank, 0);
  component_update(alpha);
  print_component(tank, 0);
  component_update(alpha);
  print_component(tank, 0);

  componentinstance_free(tank);

  return 0;
}
