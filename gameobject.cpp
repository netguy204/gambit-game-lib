#include "gameobject.h"
#include "memory.h"
#include "config.h"
#include "testlib.h"

#include <stdarg.h>

OBJECT_IMPL(World);

World::World() {
}

void world_add_go(World* world, GO* go) {
  Message* message = message_make(NULL, COLLECTIVE_ADD_AGENT, go);
  message_postinbox(world, message);
  go->world = world;
}

OBJECT_IMPL(GO);

GO::GO() {
  this->transform_parent = NULL;
  vector_zero(&this->_pos);
  vector_zero(&this->_vel);
  this->ttag = 0;
}

GO::GO(World* world) {
  this->transform_parent = NULL;
  vector_zero(&this->_pos);
  vector_zero(&this->_vel);
  this->ttag = 0;
  world_add_go(world, this);
}

GO::~GO() {
  this->components.foreach([](Component* comp) -> int {
      delete comp;
      return 0;
    });

  this->transform_children.foreach_node([](DLLNode node) -> int {
      GO* child = container_of(node, GO, transform_siblings);
      go_set_parent(child, NULL);
      return 0;
    });

  go_set_parent(this, NULL);
}

void GO::update(float dt) {
  // do an integration step
  struct Vector_ dx;
  vector_scale(&dx, &this->_vel, dt);
  vector_add(&this->_pos, &this->_pos, &dx);

  this->components.foreach([=](Component* comp) -> int {
      comp->update(dt);
      return 0;
    });

  // clear the inbox and handle terminate messages
  foreach_inboxmessage(this, NULL, NULL);
}

void GO::pos(Vector pos) {
  GO* go = this;
  *pos = go->_pos;
  while(go->transform_parent) {
    go = go->transform_parent;
    vector_add(pos, pos, &go->_pos);
  }
}

void GO::vel(Vector vel) {
  GO* go = this;
  *vel = go->_vel;
  while(go->transform_parent) {
    go = go->transform_parent;
    vector_add(vel, vel, &go->_vel);
  }
}

Component* GO::find_component(const TypeInfo* info) {
  Component* result = NULL;
  this->components.foreach([&](Component* comp) -> int {
      if(comp->typeinfo()->isInstanceOf(info)) {
        result = comp;
        return 1;
      }
      return 0;
    });
  return result;
}

void go_set_parent(GO* child, GO* parent) {
  struct Vector_ cpos, ppos;
  struct Vector_ cvel, pvel;

  if(child->transform_parent) {
    GO* old_parent = child->transform_parent;
    old_parent->transform_children.remove_node(&child->transform_siblings);
  }

  if(parent) {
    child->pos(&cpos);
    parent->pos(&ppos);
    vector_sub(&child->_pos, &cpos, &ppos);

    child->vel(&cvel);
    parent->vel(&pvel);
    vector_sub(&child->_vel, &cvel, &pvel);
    child->transform_parent = parent;
    parent->transform_children.add_head_node(&child->transform_siblings);

  } else if(!parent && child->transform_parent) {
    child->pos(&cpos);
    child->vel(&cvel);
    child->_pos = cpos;
    child->_vel = cvel;
    child->transform_parent = NULL;
  }
}

OBJECT_IMPL(Component);

Component::Component()
  : go(NULL) {
}

Component::Component(GO* go, ComponentPriority priority)
  : go(NULL), priority(priority) {

  if(go) {
    set_parent(go);
  }
}

Component::~Component() {
  if(this->go) {
    this->go->components.remove(this);
  }
}

void Component::update(float dt) {
}

void Component::set_parent(GO* go) {
  if(this->go) {
    this->go->components.remove(this);
  }

  this->go = go;

  if(go) {
    this->go->components.insert_before_when(this, [this](Component* other) {
        return this->priority < other->priority;
      });
  }
}

OBJECT_IMPL(CCollidable);

CCollidable::CCollidable()
  : Component(NULL, PRIORITY_LEAST), w(0), h(0) {
}

CCollidable::CCollidable(GO* go, float w, float h)
  : Component(go, PRIORITY_LEAST), w(w), h(h), mask(MASK_PLATFORMER) {
  if(go) {
    go->world->collidables.add_head(this);
  }
}

CCollidable::~CCollidable() {
  GO* go = this->go;
  if(go) {
    go->world->collidables.remove(this);
  }
}

void CCollidable::rect(Rect rect) {
  GO* go = this->go;

  struct Vector_ pos;
  go->pos(&pos);
  rect_centered(rect, &pos, this->w, this->h);
}

int CCollidable::intersect(CCollidable* b) {
  struct Rect_ ra, rb;
  this->rect(&ra);
  b->rect(&rb);
  return rect_intersect(&ra, &rb);
}

struct CollisionRecord {
  Rect_ rect;
  CCollidable* collidable;
};

void world_notify_collisions(World* world) {
  const int N = world->collidables.count();
  CollisionRecord* records = (CollisionRecord*)frame_alloc(sizeof(CollisionRecord) * N);
  int ii = 0;
  world->collidables.foreach([&] (CCollidable* collidable) -> int {
      collidable->rect(&records[ii].rect);
      records[ii].collidable = collidable;
      ++ii;
      return 0;
    });

  for(ii = 0; ii < N; ++ii) {
    for(int jj = ii + 1; jj < N; ++jj) {
      if(rect_intersect(&records[ii].rect, &records[jj].rect)) {
        CCollidable* c1 = records[ii].collidable;
        CCollidable* c2 = records[jj].collidable;

        if((c1->mask & c2->mask) && c1->intersect(c2)) {
          GO* g1 = c1->go;
          GO* g2 = c2->go;

          Message* m1 = message_make(g2, MESSAGE_COLLIDING, c2);
          m1->data2 = c1;
          message_postinbox(g1, m1);

          Message* m2 = message_make(g1, MESSAGE_COLLIDING, c1);
          m2->data2 = c2;
          message_postinbox(g2, m2);
        }
      }
    }
  }
}
