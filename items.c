#include "items.h"
#include "xmltools.h"
#include "utils.h"

#include "config.h"

#include <string.h>
#include <dlfcn.h>
#include <assert.h>

void* ActivatableClass;
void* ComponentClass;
void* ComponentObject;

void activate(void* _self, Activation activation) {
  const struct ActivatableClass* class = classOf(_self);
  assert(class->activate);
  class->activate(_self, activation);
}

void super_activate(const void* _class, void* _self, Activation activation) {
  const struct ActivatableClass* superclass = super(_class);
  assert(_self && superclass->activate);
  superclass->activate(_self, activation);
}

struct DLL_ classes;
void* self_handle;

char* port_names[PORT_MAX] = {
  "north",
  "south",
  "east",
  "west",
  "top",
  "bottom"
};

struct ComponentClass* node_to_componentclass(DLLNode node) {
  return container_of(node, struct ComponentClass, node);
}

void* ActivatableClass_ctor(void* _self, va_list *app) {
  struct ActivatableClass* self = super_ctor(ActivatableClass, _self, app);
  va_list ap;
  va_copy(ap, *app);

  while(1) {
    voidf selector = va_arg(ap, voidf);
    if(!((int)selector)) break;

    voidf method = va_arg(ap, voidf);
    if(selector == (voidf)activate) {
      *(voidf*)&self->activate = method;
    }
  }
  return self;
}

void* ComponentClass_ctor(void* _self, va_list *app) {
  struct ComponentClass* self = super_ctor(ComponentClass, _self, app);

  // zero the data fields
  size_t offset = offsetof(struct ComponentClass, node);
  memset((char*)self + offset, 0, sizeof(struct ComponentClass) - offset);

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

void* ComponentInstance_ctor(void* _self, va_list* app) {
  ComponentInstance self = super_ctor(ComponentObject, _self, app);
  memset(&self->connected, 0, sizeof(self->connected));
  return self;
}

void* ComponentInstance_dtor(void* _self) {
  ComponentInstance comp = _self;
  return super_dtor(ComponentObject, comp);
}

void ComponentInstance_update(void* _self, float dt) {
  // nothing
}

void ComponentInstance_activate(void* _self, Activation activation) {
  // nothing
}

void laser_fire(void* _self, Activation activation) {
  printf("bang!\n");
}

void* find_function(char* fnname) {
  void *fn = dlsym(self_handle, fnname);
  if(fn == NULL) {
    fprintf(stderr, "couldn't find function name: %s\n", fnname);
    exit(1);
  }
  return fn;
}

PortDirection string_to_portdirection(const char* str) {
  int ii;
  for(ii = 0; ii < PORT_MAX; ++ii) {
    if(streq(port_names[ii], str)) {
      return ii;
    }
  }
  fprintf(stderr, "unknown port direction %s\n", str);
  exit(1);
  return PORT_MAX;
}

void component_add_port(struct ComponentClass* klass, xmlNode* child) {
  PortDirection port = string_to_portdirection(node_attr(child, "direction", "error"));
  assert(!klass->ports[port].valid); // make sure it isn't double specified
  klass->ports[port].offsetx = atoi(node_attr(child, "offsetx", "0"));
  klass->ports[port].offsety = atoi(node_attr(child, "offsety", "0"));
  klass->ports[port].type = atoi(node_attr(child, "type", "error"));
}

void component_fill_from_xml(struct ComponentClass* klass, xmlNode* child) {
  if(streq(child->name, "port")) {
    component_add_port(klass, child);
  } else if(streq(child->name, "updatefn")) {
    ((struct UpdateableClass_*)klass)->update = find_function(node_attr(child, "name", "error"));
  } else if(streq(child->name, "activatefn")) {
    ((struct ActivatableClass*)klass)->activate = find_function(node_attr(child, "name", "error"));
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

void items_load_xml(char* filename) {
  xmlNode* children = xml_rootchildren(filename);
  xmlNode* child = children;
  while(child) {
    if(child->type == XML_ELEMENT_NODE) {
      if(streq(child->name, "item")) {
        item_create_from_xml(child);
      }
    }

    child = child->next;
  }

  xml_free(children);
}

void items_init() {
  dll_zero(&classes);
  self_handle = dlopen(NULL, RTLD_LAZY);

  updateable_init();

  ActivatableClass = new(UpdateableClass, "ActivatableClass",
                         UpdateableClass, sizeof(struct ActivatableClass),
                         ctor, ActivatableClass_ctor,
                         0);

  ComponentClass = new(ActivatableClass, "ComponentClass",
                       ActivatableClass, sizeof(struct ComponentClass),
                       ctor, ComponentClass_ctor,
                       0);

  ComponentObject = new(ComponentClass, "Component",
                        Object, sizeof(struct ComponentInstance_),
                        ctor, ComponentInstance_ctor,
                        dtor, ComponentInstance_dtor,
                        update, ComponentInstance_update,
                        activate, ComponentInstance_activate,
                        0);
}
