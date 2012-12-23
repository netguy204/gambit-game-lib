#include "gameobject.h"
#include "memory.h"
#include "config.h"

#include <stdarg.h>

OBJECT_IMPL(World);

World::World() {
  dll_zero(&this->collidables);
}

void world_foreach(World* world, Vector pos, float rad, WorldCallback callback, void* udata) {
  DLLNode node = world->children.head;
  float rad2 = rad * rad;
  while(node) {
    GO* go = (GO*)container_of(node, Agent, node);
    if(vector_dist2(&go->pos, pos) < rad2 &&  callback(go, udata)) {
      return;
    }
    node = node->next;
  }
}

Component* node_to_component(DLLNode node) {
  return container_of(node, Component, node);
}

void world_add_go(World* world, GO* go) {
  Message* message = message_make(NULL, COLLECTIVE_ADD_AGENT, go);
  message_postinbox(world, message);
  go->world = world;
}

OBJECT_IMPL(GO);

GO::GO() {
  this->transform_parent = NULL;
  vector_zero(&this->pos);
  vector_zero(&this->vel);
  dll_zero(&this->components);
  this->ttag = 0;
}

GO::GO(World* world) {
  this->transform_parent = NULL;
  vector_zero(&this->pos);
  vector_zero(&this->vel);
  dll_zero(&this->components);
  this->ttag = 0;
  world_add_go(world, this);
}

GO::~GO() {
  DLLNode node = this->components.head;
  while(node) {
    DLLNode next = node->next;
    Component* comp = node_to_component(node);
    delete comp;
    node = next;
  }
}

void GO::update(float dt) {
  // do an integration step
  struct Vector_ dx;
  vector_scale(&dx, &this->vel, dt);
  vector_add(&this->pos, &this->pos, &dx);

  DLLNode node = this->components.head;
  while(node) {
    DLLNode next = node->next;
    Component* comp = node_to_component(node);
    comp->update(dt);
    node = next;
  }

  // clear the inbox and handle terminate messages
  foreach_inboxmessage(this, NULL, NULL);
}

void go_pos(Vector pos, GO* go) {
  *pos = go->pos;
  while(go->transform_parent) {
    go = go->transform_parent;
    vector_add(pos, pos, &go->pos);
  }
}

void go_vel(Vector vel, GO* go) {
  *vel = go->vel;
  while(go->transform_parent) {
    go = go->transform_parent;
    vector_add(vel, vel, &go->vel);
  }
}

void go_set_parent(GO* child, GO* parent) {
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

World* go_world(GO* go) {
  return go->world;
}

OBJECT_IMPL(Component);

Component::Component()
  : parent_go(NULL) {
}

Component::Component(GO* go)
  : parent_go(NULL) {

  if(go) {
    set_parent(go);
  }
}

Component::~Component() {
  if(this->parent_go) {
    dll_remove(&this->parent_go->components, &this->node);
  }
}

void Component::update(float dt) {
}

void Component::set_parent(GO* go) {
  if(this->parent_go) {
    dll_remove(&this->parent_go->components, &this->node);
  }

  this->parent_go = go;

  if(go) {
    dll_add_head(&go->components, &this->node);
  }
}

GO* component_to_go(Component* comp) {
  return comp->parent_go;
}

Component* go_find_component(GO* go, const TypeInfo* info) {
  DLLNode node = go->components.head;
  while(node) {
    Component* comp = node_to_component(node);
    if(comp->typeinfo()->isInstanceOf(info)) {
      return comp;
    }
    node = node->next;
  }
  return NULL;
}
