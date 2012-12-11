#include "items.h"
#include "xmltools.h"
#include "utils.h"
#include "testlib.h"
#include "spriteatlas.h"
#include "vector.h"

#include "config.h"

#include <string.h>
#include <dlfcn.h>
#include <assert.h>

void* ActivatableClass;
void* ComponentClass;
void* ComponentObject;
void* ComponentAssemblyObject;

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

PortDirection compatible_port(PortDirection port) {
  switch(port) {
  case PORT_NORTH: return PORT_SOUTH;
  case PORT_SOUTH: return PORT_NORTH;
  case PORT_EAST: return PORT_WEST;
  case PORT_WEST: return PORT_EAST;
  case PORT_BOTTOM: return PORT_TOP;
  case PORT_TOP: return PORT_BOTTOM;
  default:
    fprintf(stderr, "no port is compatible with idx: %d\n", port);
    exit(1);
  }
}

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

void* ComponentAssemblyObject_ctor(void* _self, va_list* app) {
  ComponentAssembly self = super_ctor(ComponentAssemblyObject, _self, app);

  self->component = va_arg(*app, ComponentInstance);
  memset(self->children, 0, sizeof(self->children));
  return self;
}

void* ComponentAssemblyObject_dtor(void* _self) {
  ComponentAssembly self = _self;
  if(self->component) delete(self->component);

  int ii;
  for(ii = 0; ii < PORT_MAX; ++ii) {
    if(self->children[ii]) delete(self->children[ii]);
  }

  return super_dtor(ComponentAssemblyObject, _self);
}

void ComponentAssemblyObject_update(void* _self, float dt) {
  ComponentAssembly self = _self;
  update(self->component, dt);

  int ii;
  for(ii = 0; ii < PORT_MAX; ++ii) {
    if(self->children[ii]) {
      update(self->children[ii], dt);
    }
  }
}

void ComponentAssemblyObject_activate(void* _self, Activation activation) {
  ComponentAssembly self = _self;
  activate(self->component, activation);

  int ii;
  for(ii = 0; ii < PORT_MAX; ++ii) {
    if(self->children[ii]) {
      activate(self->children[ii], activation);
    }
  }
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
  klass->ports[port].valid = 1;
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

const ComponentPort componentinstance_ports(ComponentInstance component) {
  const struct ComponentClass* class = classOf(component);
  // FIXME: I still don't understand array constness in C
  // correctly. Casting it away.
  return (const ComponentPort)class->ports;
}

const ComponentPort componentassembly_ports(ComponentAssembly assembly) {
  return componentinstance_ports(assembly->component);
}

int assembly_count_components(ComponentAssembly assembly) {
  int count = 1;
  int ii;

  for(ii = 0; ii < PORT_MAX; ++ii) {
    if(assembly->children[ii]) {
      count += assembly_count_components(assembly->children[ii]);
    }
  }

  return count;
}

int assembly_insert(ComponentAssembly assembly, ComponentInstance component) {
  ComponentPort assembly_ports = componentassembly_ports(assembly);
  ComponentPort component_ports = componentinstance_ports(component);

  int ii;

  // first try inserting in this node
  for(ii = 0; ii < PORT_MAX; ++ii) {
    PortDirection compat = compatible_port(ii);
    if(assembly_ports[ii].valid && !assembly->children[ii] && component_ports[compat].valid) {
      // this is a valid insertion point
      assembly->children[ii] = new(ComponentAssemblyObject, component);
      return 1;
    }
  }

  // then try inserting in our children. FIXME: choose the least
  // populated subtree first.
  for(ii = 0; ii < PORT_MAX; ++ii) {
    if(component_ports[ii].valid && assembly->children[ii]) {
      if(assembly_insert(assembly->children[ii], component)) return 1;
    }
  }

  // we failed
  return 0;
}

SpriteAtlasEntry component_image(ComponentInstance component, SpriteAtlas atlas) {
  return spriteatlas_find(atlas, className(classOf(component)));
}

// fixme
void component_offset(Vector offset, ComponentInstance component, PortDirection direction,
                      SpriteAtlas atlas) {
  SpriteAtlasEntry image = component_image(component, atlas);
}

/*
SpriteList assembly_spritelist(SpriteAtlas atlas, ComponentAssembly assembly,
                               SpriteList spritelist, Vector origin) {

  ComponentInstance component = assembly->component;
  SpriteAtlasEntry image = component_image(component, atlas);

  if(image) {
    Sprite sprite = frame_make_sprite();
    sprite_fillfromentry(sprite, image);
    sprite->originX = 0.5;
    sprite->originY = 0.5;
    sprite->displayX = origin->x;
    sprite->displayY = origin->y;
    spritelist = frame_spritelist_append(spritelist, sprite);
  }

  int ii;
  for(ii = 0; ii < PORT_MAX; ++ii) {
    ComponentAssembly child = assembly->children[ii];
    if(child) {
      struct Vector_ offset;
      //if(child->
    }
  }
}
*/

void items_init() {
  dll_zero(&classes);
  self_handle = dlopen(NULL, RTLD_LAZY);

  ActivatableClass = new(UpdateableClass(), "ActivatableClass",
                         UpdateableClass(), sizeof(struct ActivatableClass),
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

  ComponentAssemblyObject = new(ActivatableClass, "ComponentAssembly",
                                Object, sizeof(struct ComponentAssembly_),
                                ctor, ComponentAssemblyObject_ctor,
                                dtor, ComponentAssemblyObject_dtor,
                                update, ComponentAssemblyObject_update,
                                activate, ComponentAssemblyObject_activate,
                                0);
}
