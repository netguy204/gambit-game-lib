#include "items.h"
#include "xmltools.h"
#include "memory.h"

#include "config.h"

#include <string.h>
#include <dlfcn.h>

struct DLL_ classes;
void* self_handle;

char* resource_names[MAX_RESOURCE] = {
  "heat",
  "power",
  "fuel",
  "space"
};

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

void resources_transfer(Resources target, Resources source, Resources limit) {
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    float to_transfer;
    if(limit[ii] < 0) {
      to_transfer = MAX(limit[ii], source[ii]);
    } else {
      to_transfer = MIN(limit[ii], source[ii]);
    }
    target[ii] += to_transfer;
    source[ii] -= to_transfer;
  }
}

void resources_assign(Resources target, Resources source) {
  memcpy(target, source, sizeof(Resources_));
}

void stats_assign(Stats target, Stats source) {
  memcpy(target, source, sizeof(struct Stats_));
}

void stats_transfer(Stats target, Stats source, Stats limit) {
  resources_transfer(target->storage, source->storage, limit->storage);
  resources_transfer(target->max_capacity, source->max_capacity, limit->max_capacity);
  resources_transfer(target->production_rates, source->production_rates, limit->production_rates);
}

void stats_add(Stats target, Stats a, Stats b) {
  resources_add(target->storage, a->storage, b->storage);
  resources_add(target->max_capacity, a->max_capacity, b->max_capacity);
  resources_add(target->production_rates, a->production_rates, b->production_rates);
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

int componentclass_named(ComponentClass klass, char* name) {
  return strncmp(klass->name, name, ITEM_MAX_NAME - 1) == 0;
}

ComponentClass componentclass_find(char *name) {
  ComponentClass node = (ComponentClass)classes.head;
  while(node) {
    if(componentclass_named(node, name)) {
      return node;
    }
    node = (ComponentClass)node->node.next;
  }

  fprintf(stderr, "couldn't find component name: %s\n", name);
  exit(1);
  return NULL;
}

ComponentInstance componentinstance_superparent(ComponentInstance comp) {
  while(comp->parent) {
    comp = comp->parent;
  }
  return comp;
}

void componentinstance_addchild(ComponentInstance parent, ComponentInstance child) {
  SAFETY(if(child->parent) fail_exit("child already has a parent"));
  dll_add_head(&parent->children, &child->node);

  // add resources of child to parent
  stats_transfer(&parent->stats, &child->stats, &child->klass->stats);

  child->parent = parent;
}

void componentinstance_removechild(ComponentInstance child) {
  SAFETY(if(!child->parent) fail_exit("child does not have a parent"));

  dll_remove(&child->parent->children, &child->node);

  // remove resources of child from superparent
  ComponentInstance parent = child->parent;
  stats_transfer(&child->stats, &parent->stats, &child->klass->stats);

  child->parent = NULL;
}

ComponentInstance componentinstance_findchild(ComponentInstance root, char* klass_name) {
  if(componentclass_named(root->klass, klass_name)) return root;

  DLLNode childnode = root->children.head;
  while(childnode) {
    ComponentInstance found = componentinstance_findchild(componentinstance_from_node(childnode), klass_name);
    if(found) return found;
    childnode = childnode->next;
  }

  return NULL;
}

ComponentInstance componentinstance_make(ComponentClass klass) {
  ComponentInstance inst = malloc(sizeof(struct ComponentInstance_));
  memset(inst, 0, sizeof(struct ComponentInstance_));

  inst->klass = klass;
  stats_assign(&inst->stats, &klass->stats);
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
  return component->stats.max_capacity[resource] - component->stats.storage[resource];
}

int component_pullimpl(ComponentInstance component, Resources resources) {
  // all component hierarchies are flat and resource requests always
  // go to the parent so we're the final destination for this request
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    if(component->stats.storage[ii] < resources[ii]) break;
  }

  // if we didn't find it all, return failure
  if(ii != MAX_RESOURCE) return 0;

  // pull it all and return success
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    component->stats.storage[ii] -= resources[ii];
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
    component->stats.storage[ii] += willtake;
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

int component_storagesatisfies(ComponentInstance component, Resources request) {
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    float predicted_storage = component->stats.storage[ii] + request[ii];
    if(predicted_storage < 0 || predicted_storage > component->stats.max_capacity[ii]) {
      // does not satisfy
      return 0;
    }
  }
  return 1;
}

int component_storageput(ComponentInstance component, Resources req) {
  if(!component_storagesatisfies(component, req)) return 0;

  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    component->stats.storage[ii] += req[ii];
  }
  return 1;
}

void system_updateimpl(ComponentInstance component) {
  // update our children
  DLLNode node = component->children.head;
  while(node) {
    ComponentInstance child = componentinstance_from_node(node);
    component_update(child);
    node = node->next;
  }

  // take the storage and do the production if we can
  component_storageput(component, component->stats.production_rates);
}

void heatsink_update(ComponentInstance component) {
  // we operate on our parent. give up if there is none
  if(!component->parent) return;

  // take away quality percent of the heat stored by our parent
  component->parent->stats.storage[HEAT] -=
    component->quality * component->parent->stats.storage[HEAT];
}

void laser_fire(ComponentInstance component, Activation activation) {
  // we should spawn a particle and ... maybe an agent ... and a
  // system to manage the characteristics of the bolt...

  float base_power = 150;
  float base_heat = 5;

  float power = base_power / component->quality;
  float heat = base_heat / component->quality;

  Resources_ req = { heat, -power, 0.0f, 0.0f };
  if(component_storageput(component->parent, req)) {
    printf("fire!\n");
  }
}

float* resource_get_name_ptr(Resources attr, const char* name) {
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    if(streq(name, resource_names[ii])) {
      return &attr[ii];
    }
  }

  fprintf(stderr, "unrecognized Attribute: %s\n", name);
  exit(1);
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
    resource_add_name(klass->stats.production_rates, node_attr(child, "name", "error"),
                      atof(node_attr(child, "rate", "error")));
  } else if(streq(child->name, "consumes")) {
    resource_sub_name(klass->stats.production_rates, node_attr(child, "name", "error"),
                      atof(node_attr(child, "rate", "error")));
  } else if(streq(child->name, "provides")) {
    resource_add_name(klass->stats.storage, node_attr(child, "name", "error"),
                      atof(node_attr(child, "value", "error")));
  } else if(streq(child->name, "requires")) {
    resource_sub_name(klass->stats.storage, node_attr(child, "name", "error"),
                      atof(node_attr(child, "value", "error")));
  } else if(streq(child->name, "maxcapacity")) {
    resource_add_name(klass->stats.max_capacity, node_attr(child, "name", "error"),
                      atof(node_attr(child, "value", "error")));
  } else if(streq(child->name, "quality")) {
    klass->quality = atof(node_attr(child, "value", "error"));
  } else if(streq(child->name, "updatefn")) {
    klass->update = find_function(node_attr(child, "name", "error"));
  } else if(streq(child->name, "activatefn")) {
    klass->activate = find_function(node_attr(child, "name", "error"));
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

    // flatten the hierarchy if there is one
    if(comp_klass->subcomponents) {
      LLNode subnode = comp_klass->subcomponents;
      ComponentClass subklass;
      while((subklass = llentry_nextvalue(&subnode))) {
        llentry_add(&klass->subcomponents, subklass);
      }

      // add the subsystem stats to our own
      stats_add(&klass->stats, &klass->stats, &comp_klass->stats);
    } else {
      llentry_add(&klass->subcomponents, comp_klass);
    }
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

  // make sure our max_capacity accounts for our currently allocated
  // storage
  int ii;
  for(ii = 0; ii < MAX_RESOURCE; ++ii) {
    klass->stats.max_capacity[ii] =
      MAX(klass->stats.max_capacity[ii], klass->stats.storage[ii]);
  }
}

void items_load_xml(char* filename) {
  xmlNode* children = xml_rootchildren(filename);
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
}
