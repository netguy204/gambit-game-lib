#include "items.h"
#include "xmltools.h"
#include "utils.h"

#include "config.h"

#include <string.h>
#include <dlfcn.h>
#include <assert.h>

void* ComponentClass;
void* ComponentObject;
void* ComponentSystemObject;


void activate(void* _self, Activation activation) {
  const struct ComponentClass* class = classOf(_self);
  assert(class->activate);
  class->activate(_self, activation);
}

void super_activate(const void* _class, void* _self, Activation activation) {
  const struct ComponentClass* superclass = super(_class);
  assert(_self && superclass->activate);
  superclass->activate(_self, activation);
}

int push(void* _self, Resources resources) {
  const struct ComponentClass* class = classOf(_self);
  assert(class->push);
  return class->push(_self, resources);
}

int super_push(const void* _class, void* _self, Resources resources) {
  const struct ComponentClass* superclass = super(_class);
  assert(_self && superclass->push);
  return superclass->push(_self, resources);
}

int pull(void* _self, Resources resources) {
  const struct ComponentClass* class = classOf(_self);
  assert(class->pull);
  return class->pull(_self, resources);
}

int super_pull(const void* _class, void* _self, Resources resources) {
  const struct ComponentClass* superclass = super(_class);
  assert(_self && superclass->pull);
  return superclass->pull(_self, resources);
}

struct DLL_ classes;
void* self_handle;

char* resource_names[MAX_RESOURCE] = {
  "heat",
  "power",
  "fuel",
  "space"
};

struct ComponentClass* node_to_componentclass(DLLNode node) {
  return container_of(node, struct ComponentClass, node);
}

ComponentInstance node_to_componentinstance(DLLNode node) {
  return container_of(node, struct ComponentInstance_, node);
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

void resources_transfer(Resources target, Resources source, const Resources_ limit) {
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

void resources_assign(Resources target, const Resources_ source) {
  memcpy(target, source, sizeof(Resources_));
}

void stats_assign(Stats target, const struct Stats_* source) {
  memcpy(target, source, sizeof(struct Stats_));
}

void stats_transfer(Stats target, Stats source, const struct Stats_* limit) {
  resources_transfer(target->storage, source->storage, limit->storage);
  resources_transfer(target->max_capacity, source->max_capacity, limit->max_capacity);
  resources_transfer(target->production_rates, source->production_rates, limit->production_rates);
}

void stats_add(Stats target, Stats a, Stats b) {
  resources_add(target->storage, a->storage, b->storage);
  resources_add(target->max_capacity, a->max_capacity, b->max_capacity);
  resources_add(target->production_rates, a->production_rates, b->production_rates);
}

void* ComponentClass_ctor(void* _self, va_list *app) {
  struct ComponentClass* self = super_ctor(ComponentClass, _self, app);
  va_list ap;
  va_copy(ap, *app);

  while(1) {
    voidf selector = va_arg(ap, voidf);
    if(!((int)selector)) break;

    voidf method = va_arg(ap, voidf);
    if(selector == (voidf)activate) {
      *(voidf*)&self->activate = method;
    } else if(selector == (voidf)push) {
      *(voidf*)&self->push = method;
    } else if(selector == (voidf)pull) {
      *(voidf*)&self->pull = method;
    }
  }

  // zero the data fields
  size_t offset = offsetof(struct ComponentClass, node);
  memset((char*)self + offset, 0, sizeof(struct ComponentClass) - offset);
  self->quality = 1.0f;

  // add to the item-class registry
  dll_add_head(&classes, &self->node);

  return self;
}

struct ComponentClass* componentclass_find(char *name) {
  DLLNode node = classes.head;
  while(node) {
    struct ComponentClass* class = node_to_componentclass(node);
    if(strcmp(className(class), name) == 0) {
      return class;
    }
    node = node->next;
  }

  fprintf(stderr, "couldn't find component name: %s\n", name);
  exit(1);
  return NULL;
}

void componentinstance_addchild(ComponentInstance parent, ComponentInstance child) {
  SAFETY(if(child->parent) fail_exit("child already has a parent"));
  dll_add_head(&parent->children, &child->node);

  // add resources of child to parent
  const struct ComponentClass* child_class = classOf(child);
  stats_transfer(&parent->stats, &child->stats, &child_class->stats);

  child->parent = parent;
}

void componentinstance_removechild(ComponentInstance child) {
  SAFETY(if(!child->parent) fail_exit("child does not have a parent"));
  assert(child->parent);

  dll_remove(&child->parent->children, &child->node);

  // remove resources of child from superparent
  ComponentInstance parent = child->parent;
  const struct ComponentClass* child_class = classOf(child);
  stats_transfer(&child->stats, &parent->stats, &child_class->stats);

  child->parent = NULL;
}

ComponentInstance componentinstance_findchild(ComponentInstance root, char* klass_name) {
  if(strcmp(className(classOf(root)), klass_name) == 0) return root;

  DLLNode childnode = root->children.head;
  while(childnode) {
    ComponentInstance found = componentinstance_findchild(node_to_componentinstance(childnode), klass_name);
    if(found) return found;
    childnode = childnode->next;
  }

  return NULL;
}

void* ComponentInstance_ctor(void* _self, va_list* app) {
  ComponentInstance self = super_ctor(ComponentObject, _self, app);

  // a lot of our init data is carried in the metaclass
  const struct ComponentClass* class = classOf(self);
  stats_assign(&self->stats, &class->stats);
  self->quality = class->quality;

  LLNode child = class->subcomponents;
  struct ComponentClass* child_class;
  while((child_class = llentry_nextvalue(&child))) {
    ComponentInstance child_inst = new(child_class);
    componentinstance_addchild(self, child_inst);
  }

  return self;
}

void* ComponentInstance_dtor(void* _self) {
  ComponentInstance comp = _self;
  SAFETY(if(comp->parent) fail_exit("freeing component that has a parent"));

  DLLNode node = comp->children.head;

  // free remaining children
  while(node) {
    ComponentInstance child = node_to_componentinstance(node);
    child->parent = NULL;
    delete(child);
    node = node->next;
  }

  return super_dtor(ComponentObject, comp);
}

float component_storage_available(ComponentInstance component, Resource resource) {
  return component->stats.max_capacity[resource] - component->stats.storage[resource];
}

int ComponentInstance_pull(void* _self, Resources resources) {
  ComponentInstance component = _self;

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

int ComponentInstance_push(void* _self, Resources resources) {
  ComponentInstance component = _self;

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
    ComponentInstance child = node_to_componentinstance(childnode);

    if(push(child, resources)) return 1;
    childnode = childnode->next;
  }

  // if we make it here then we weren't able to push
  // everything... interesting consequences?
  return 0;
}

void ComponentInstance_update(void* _self, float dt) {
  // nothing
}

void ComponentInstance_activate(void* _self, Activation activation) {
  // nothing
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

void ComponentSystemObject_update(void* _self, float dt) {
  ComponentInstance component = _self;

  // update our children
  DLLNode node = component->children.head;
  while(node) {
    ComponentInstance child = node_to_componentinstance(node);
    update(child, dt);
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

void component_fill_from_xml(struct ComponentClass* klass, xmlNode* child) {
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
    ((struct UpdateableClass_*)klass)->update = find_function(node_attr(child, "name", "error"));
  } else if(streq(child->name, "activatefn")) {
    klass->activate = find_function(node_attr(child, "name", "error"));
  } else {
    fprintf(stderr, "unrecognized item subnode: %s\n", child->name);
    exit(1);
  }
}

void item_create_from_xml(xmlNode* child) {
  struct ComponentClass* class = new(ComponentClass, node_attr(child, "name", "error"),
                                     ComponentObject, sizeof(struct ComponentInstance_),
                                     0);

  child = child->children;
  while(child) {
    if(child->type == XML_ELEMENT_NODE) {
      component_fill_from_xml(class, child);
    }

    child = child->next;
  }
}

void system_fill_from_xml(struct ComponentClass* klass, xmlNode* child) {
  if(streq(child->name, "component")) {
    char* comp_name = node_attr(child, "name", "error");
    struct ComponentClass* comp_klass = componentclass_find(comp_name);

    // flatten the hierarchy if there is one
    if(comp_klass->subcomponents) {
      LLNode subnode = comp_klass->subcomponents;
      struct ComponentClass* subklass;
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
  struct ComponentClass* klass = new(ComponentClass, node_attr(child, "name", "error"),
                                     ComponentSystemObject, sizeof(struct ComponentInstance_),
                                     0);

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

      //printf("name: %s\n", child->name);
    }

    child = child->next;
  }

  xml_free(children);
}

// pretty-print the components
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

void ComponentInstance_tofile(void* _self, FILE* fp) {
  ComponentInstance component = _self;
  fprintf(fp, "%s: %s\n", className(classOf(component)), resources_str(component->stats.storage));
}

void items_init() {
  dll_zero(&classes);
  self_handle = dlopen(NULL, RTLD_LAZY);

  updateable_init();

  ComponentClass = new(UpdateableClass, "ComponentClass",
                       UpdateableClass, sizeof(struct ComponentClass),
                       ctor, ComponentClass_ctor,
                       0);

  ComponentObject = new(ComponentClass, "Component",
                        Object, sizeof(struct ComponentInstance_),
                        ctor, ComponentInstance_ctor,
                        dtor, ComponentInstance_dtor,
                        tofile, ComponentInstance_tofile,
                        update, ComponentInstance_update,
                        activate, ComponentInstance_activate,
                        pull, ComponentInstance_pull,
                        push, ComponentInstance_push,
                        0);

  ComponentSystemObject = new(ComponentClass, "ComponentSystem",
                              ComponentObject, sizeof(struct ComponentInstance_),
                              update, ComponentSystemObject_update,
                              0);
}
