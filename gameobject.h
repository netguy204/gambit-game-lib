#ifndef GAMEOBJECT_H
#define GAMEOBJECT_H

#include "agent.h"
#include "vector.h"

class World : public Collective {
 public:
  OBJECT_PROTO(World);

  World();

  struct DLL_ collidables;
};

class GO;

typedef int(*WorldCallback)(GO* go, void* udata);
void world_foreach(World* world, Vector pos, float rad,
                   WorldCallback callback, void* udata);


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

  // beware: death of parent is not handled
  struct GO* transform_parent;

  // pos and vel are always relative to the parent if there is one
  struct Vector_ pos;
  struct Vector_ vel;
  struct DLL_ components;

  // the world we're a part of
  World* world;

  // type tag, eg. bomb
  int ttag;
};

// get the absolute position of a GO
void go_pos(Vector pos, GO* go);
void go_vel(Vector vel, GO* go);
void go_set_parent(GO* child, GO* parent);
World* go_world(GO* go);

class Component : public Object {
 public:
  OBJECT_PROTO(Component);

  Component();
  Component(GO* go);
  virtual ~Component();

  virtual void update(float dt);

  void set_parent(GO* go);

  struct DLLNode_ node;

  GO* parent_go;
};

Component* node_to_component(DLLNode node);
GO* component_to_go(Component* comp);
Component* go_find_component(GO* go, const TypeInfo* info);

#endif
