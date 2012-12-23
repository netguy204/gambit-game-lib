#include "gameobject.h"
#include "memory.h"
#include "config.h"

#include <stdarg.h>

OBJECT_IMPL(World);

World::World() {
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
  this->ttag = 0;
}

GO::GO(World* world) {
  this->transform_parent = NULL;
  vector_zero(&this->pos);
  vector_zero(&this->vel);
  this->ttag = 0;
  world_add_go(world, this);
}

GO::~GO() {
  DLLNode node = this->components.head;
  while(node) {
    DLLNode next = node->next;
    Component* comp = this->components.to_element(node);
    delete comp;
    node = next;
  }

  node = this->transform_children.head;
  while(node) {
    DLLNode next = node->next;
    GO* child = container_of(node, GO, transform_siblings);
    go_set_parent(child, NULL);
    node = next;
  }

  go_set_parent(this, NULL);
}

void GO::update(float dt) {
  // do an integration step
  struct Vector_ dx;
  vector_scale(&dx, &this->vel, dt);
  vector_add(&this->pos, &this->pos, &dx);

  DLLNode node = this->components.head;
  while(node) {
    DLLNode next = node->next;
    Component* comp = this->components.to_element(node);
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

  if(child->transform_parent) {
    GO* old_parent = child->transform_parent;
    old_parent->transform_children.remove_node(&child->transform_siblings);
  }

  if(parent) {
    go_pos(&cpos, child);
    go_pos(&ppos, parent);
    vector_sub(&child->pos, &cpos, &ppos);

    go_vel(&cvel, child);
    go_vel(&pvel, parent);
    vector_sub(&child->vel, &cvel, &pvel);
    child->transform_parent = parent;
    parent->transform_children.add_head_node(&child->transform_siblings);

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
    this->parent_go->components.remove(this);
  }
}

void Component::update(float dt) {
}

void Component::set_parent(GO* go) {
  if(this->parent_go) {
    this->parent_go->components.remove(this);
  }

  this->parent_go = go;

  if(go) {
    this->parent_go->components.add_head(this);
  }
}

GO* component_to_go(Component* comp) {
  return comp->parent_go;
}

Component* go_find_component(GO* go, const TypeInfo* info) {
  DLLNode node = go->components.head;
  while(node) {
    Component* comp = go->components.to_element(node);
    if(comp->typeinfo()->isInstanceOf(info)) {
      return comp;
    }
    node = node->next;
  }
  return NULL;
}

OBJECT_IMPL(CCollidable);

CCollidable::CCollidable()
  : Component(NULL), w(0), h(0) {
}

CCollidable::CCollidable(GO* go, float w, float h)
  : Component(go), w(w), h(h), mask(MASK_PLATFORMER) {
  if(go) {
    go->world->collidables.add_head(this);
  }
}

CCollidable::~CCollidable() {
  GO* go = this->parent_go;
  if(go) {
    go->world->collidables.remove(this);
  }
}

void CCollidable::rect(Rect rect) {
  GO* go = this->parent_go;

  struct Vector_ pos;
  go_pos(&pos, go);
  rect_centered(rect, &pos, this->w, this->h);
}

int CCollidable::intersect(CCollidable* b) {
  struct Rect_ ra, rb;
  this->rect(&ra);
  b->rect(&rb);
  return rect_intersect(&ra, &rb);
}

void world_notify_collisions(World* world) {
  DLLNode n1 = world->collidables.head;
  while(n1) {
    CCollidable* c1 = world->collidables.to_element(n1);
    GO* g1 = component_to_go(c1);

    DLLNode n2 = n1->next;
    while(n2) {
      CCollidable* c2 = world->collidables.to_element(n2);

      if((c1->mask & c2->mask) && c1->intersect(c2)) {
        GO* g2 = component_to_go(c2);

        Message* m1 = message_make(g2, MESSAGE_COLLIDING, c2);
        m1->data2 = c1;
        message_postinbox(g1, m1);

        Message* m2 = message_make(g1, MESSAGE_COLLIDING, c1);
        m2->data2 = c2;
        message_postinbox(g2, m2);
      }

      n2 = n2->next;
    }
    n1 = n1->next;
  }
}
