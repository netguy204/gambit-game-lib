#ifndef GAMEOBJECT_H
#define GAMEOBJECT_H

#include "agent.h"
#include "vector.h"
#include "rect.h"

typedef enum {
  MASK_NON_COLLIDER = 0,
  MASK_PLATFORM = 1,
  MASK_PLATFORMER = 2,
  MASK_ENEMY_PLATFORM = 4
} CollisionMask;


class GO;

class Component : public Object {
 public:
  OBJECT_PROTO(Component);

  Component();
  Component(GO* go);
  virtual ~Component();

  virtual void update(float dt);

  void set_parent(GO* go);

  struct DLLNode_ node;
  GO* go;
};

// registers a GO with the collidables section of the world. GO
// components will get COLLIDED messages letting them known when they
// hit other GOs that have a collidable component
class CCollidable : public Component {
 public:
  OBJECT_PROTO(CCollidable);

  CCollidable();
  CCollidable(GO* go, float w, float h);
  virtual ~CCollidable();

  void rect(Rect rect);
  int intersect(CCollidable* other);

  struct DLLNode_ collidable_node;
  float w;
  float h;
  int mask;
};

class World : public Collective {
 public:
  OBJECT_PROTO(World);

  World();

  DLL_DECLARE(CCollidable, collidable_node) collidables;
};

typedef int(*WorldCallback)(GO* go, void* udata);
void world_foreach(World* world, Vector pos, float rad,
                   WorldCallback callback, void* udata);

void world_notify_collisions(World* world);


/* GO: GameObject
 *
 * This is an agent that makes all of its messgaes available to its
 * components. If a component ever cares about messages it should
 * search the list of messages in its parent object for whatever its
 * interested in. The parent will clear its message list and handle
 * terminations at the end of the component update cycle.
 */

class GO : public Agent {
 public:
  OBJECT_PROTO(GO);

  GO();
  GO(World* world);
  virtual ~GO();

  virtual void update(float dt);

  struct DLLNode_ transform_siblings;
  struct GO* transform_parent;
  SimpleDLL transform_children;

  DLL_DECLARE(Component, node) components;

  // pos and vel are always relative to the parent if there is one
  struct Vector_ _pos;
  struct Vector_ _vel;

  void pos(Vector p);
  void vel(Vector v);
  Component* find_component(const TypeInfo* info);


  // the world we're a part of
  World* world;

  // type tag, eg. bomb
  int ttag;
};

void go_set_parent(GO* child, GO* parent);

#endif
