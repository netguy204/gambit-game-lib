#include "gameobject.h"
#include "updateable.h"
#include "memory.h"
#include "config.h"

#include <stdarg.h>

FixedAllocator gameobject_allocator;

void* WorldObject_ctor(void* _self, va_list* app) {
  World world = super_ctor(WorldObject(), _self, app);
  dll_zero(&world->collidables);
  return world;
}

const void* WorldObject() {
  static void* class = NULL;
  if(class) return class;

  class = new(UpdateableClass(), "World",
              CollectiveObject(), sizeof(struct World_),
              ctor, WorldObject_ctor,
              0);
  return class;
}

void world_foreach(World world, Vector pos, float rad, WorldCallback callback, void* udata) {
  DLLNode node = ((Collective)world)->children.head;
  float rad2 = rad * rad;
  while(node) {
    GO go = (GO)container_of(node, struct Agent_, node);
    if(vector_dist2(&go->pos, pos) < rad2 &&  callback(go, udata)) {
      return;
    }
    node = node->next;
  }
}

Component node_to_component(DLLNode node) {
  return container_of(node, struct Component_, node);
}

void* GameObject_alloci(void* _class) {
  return fixed_allocator_alloc(gameobject_allocator);
}

void GameObject_dealloci(void* _self) {
  fixed_allocator_free(gameobject_allocator, _self);
}

void world_add_go(World world, GO go) {
  Message message = message_make(NULL, COLLECTIVE_ADD_AGENT, go);
  message_postinbox((Agent)world, message);
}

void* GameObject_ctor(void* _self, va_list* app) {
  GO go = super_ctor(GameObject(), _self, app);
  go->world = va_arg(*app, World);
  world_add_go(go->world, go);

  go->transform_parent = NULL;
  vector_zero(&go->pos);
  vector_zero(&go->vel);
  dll_zero(&go->components);
  go->ttag = 0;
  return go;
}

void* GameObject_dtor(void* _self) {
  // no need to remove ourselves from the world because the dtor isn't
  // called until the collective has already done that
  GO go = _self;

  DLLNode node = go->components.head;
  while(node) {
    DLLNode next = node->next;
    Component comp = node_to_component(node);
    delete(comp);
    node = next;
  }

  return super_dtor(GameObject(), _self);
}

void GameObject_update(void* _self, float dt) {
  // we're the root of the update heirarchy
  GO go = _self;

  // do an integration step
  struct Vector_ dx;
  vector_scale(&dx, &go->vel, dt);
  vector_add(&go->pos, &go->pos, &dx);

  DLLNode node = go->components.head;
  while(node) {
    DLLNode next = node->next;
    Component comp = node_to_component(node);
    update(comp, dt);
    node = next;
  }

  // clear the inbox and handle terminate messages
  foreach_inboxmessage((Agent)go, NULL, NULL);
}

void go_pos(Vector pos, GO go) {
  *pos = go->pos;
  while(go->transform_parent) {
    go = go->transform_parent;
    vector_add(pos, pos, &go->pos);
  }
}

void go_vel(Vector vel, GO go) {
  *vel = go->vel;
  while(go->transform_parent) {
    go = go->transform_parent;
    vector_add(vel, vel, &go->vel);
  }
}

void go_set_parent(GO child, GO parent) {
  struct Vector_ cpos, ppos;
  struct Vector_ cvel, pvel;

  if(parent) {
    go_pos(&cpos, child);
    go_pos(&ppos, parent);
    vector_sub(&child->pos, &cpos, &ppos);

    go_vel(&cvel, child);
    go_vel(&pvel, parent);
    vector_sub(&child->vel, &cvel, &pvel);
    child->transform_parent = parent;
  } else if(!parent && child->transform_parent) {
    go_pos(&cpos, child);
    go_vel(&cvel, child);
    child->pos = cpos;
    child->vel = cvel;
    child->transform_parent = NULL;
  }
}

World go_world(GO go) {
  return go->world;
}

void* ComponentObject_ctor(void* _self, va_list* app) {
  Component comp = super_ctor(ComponentObject(), _self, app);
  comp->parent_go = va_arg(*app, GO);
  dll_add_head(&comp->parent_go->components, &comp->node);
  return comp;
}

void* ComponentObject_dtor(void* _self) {
  Component comp = _self;

  if(comp->parent_go) {
    dll_remove(&comp->parent_go->components, &comp->node);
  }

  return super_dtor(ComponentObject(), _self);
}

void ComponentObject_update(void* _self, float dt) {
  // this is the root
}

GO component_to_go(void* _comp) {
  Component comp = _comp;
  return comp->parent_go;
}

void* go_find_component(GO go, const void* class) {
  DLLNode node = go->components.head;
  while(node) {
    Component comp = node_to_component(node);
    if(isInstanceOf(class, comp)) {
      return comp;
    }
    node = node->next;
  }
  return NULL;
}

static void go_init() {
  static int initialized = 0;
  if(initialized) return;

  gameobject_allocator = fixed_allocator_make(sizeof(struct GO_),
                                              MAX_NUM_GAMEOBJECTS,
                                              "gameobject_allocator");
  initialized = 1;
}

const void* GameObject() {
  static void* class = NULL;
  if(class) return class;

  go_init();
  class = new(UpdateableClass(), "GameObject",
              AgentObject(), sizeof(struct GO_),
              alloci, GameObject_alloci,
              dealloci, GameObject_dealloci,
              ctor, GameObject_ctor,
              dtor, GameObject_dtor,
              update, GameObject_update,
              0);
  return class;
}

const void* ComponentObject() {
  static void* class = NULL;
  if(class) return class;

  go_init();
  class = new(UpdateableClass(), "ComponentObject",
              Object, sizeof(struct Component_),
              ctor, ComponentObject_ctor,
              dtor, ComponentObject_dtor,
              update, ComponentObject_update,
              0);
  return class;
}
