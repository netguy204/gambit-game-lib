#ifndef GAMEOBJECT_H
#define GAMEOBJECT_H

#include "agent.h"
#include "vector.h"

typedef struct World_ {
  struct Collective_ _;
  struct DLL_ collidables;
} *World;

const void* WorldObject();

struct GO_;

typedef int(*WorldCallback)(struct GO_* go, void* udata);
void world_foreach(World world, Vector pos, float rad,
                   WorldCallback callback, void* udata);


/* GO: GameObject
 *
 * This is an agent that makes all of its messgaes available to its
 * components. If a component ever cares about messages it should
 * search the list of messages in its parent object for whatever its
 * interested in. The parent will clear its message list and handle
 * terminations at the end of the component update cycle.
 */


typedef struct GO_ {
  struct Agent_ _;

  // beware: death of parent is not handled
  struct GO_* transform_parent;

  // pos and vel are always relative to the parent if there is one
  struct Vector_ pos;
  struct Vector_ vel;
  struct DLL_ components;

  // the world we're a part of
  World world;

  // type tag, eg. bomb
  int ttag;
} *GO;

// constructor args: world
const void* GameObject();

// get the absolute position of a GO
void go_pos(Vector pos, GO go);
void go_vel(Vector vel, GO go);
void go_set_parent(GO child, GO parent);
World go_world(GO go);

typedef struct Component_ {
  struct Object _;
  struct DLLNode_ node;
  GO parent_go;
} *Component;

// constructor args: GO
const void* ComponentObject();

Component node_to_component(DLLNode node);
GO component_to_go(void* _comp);
void* go_find_component(GO go, const void* class);

#endif
