#ifndef GAMEOBJECT_H
#define GAMEOBJECT_H

#include "agent.h"
#include "vector.h"
#include "rect.h"
#include "spriteatlas.h"

#include <lua.hpp>
#include <map>

typedef enum {
  MASK_NON_COLLIDER = 0,
  MASK_PLATFORM = 1,
  MASK_PLATFORMER = 2,
  MASK_ENEMY_PLATFORM = 4
} CollisionMask;


class GO;

enum ComponentPriority {
  PRIORITY_THINK,
  PRIORITY_ACT,
  PRIORITY_SHOW,
  PRIORITY_LEAST
};

class Component : public Object {
 public:
  OBJECT_PROTO(Component);

  // invalid constructor, throws
  Component(void* invalid);
  Component(GO* go, ComponentPriority priority);
  virtual ~Component();

  virtual void update(float dt);

  void set_parent(GO* go);

  struct DLLNode_ node;
  GO* go;
  ComponentPriority priority;
  int delete_me;
};

// registers a GO with the collidables section of the world. GO
// components will get COLLIDED messages letting them known when they
// hit other GOs that have a collidable component
class CCollidable : public Component {
 public:
  OBJECT_PROTO(CCollidable);

  CCollidable(void* go);
  virtual ~CCollidable();

  void rect(Rect rect);
  int intersect(CCollidable* other);

  struct DLLNode_ collidable_node;
  float w;
  float h;
  int mask;
};

struct LuaThread {
  LuaThread();

  lua_State* state;
  int refid;
};

class CScripted : public Component {
 public:
  OBJECT_PROTO(CScripted);

  CScripted(void* go);
  virtual ~CScripted();

  virtual void update(float dt);

  LuaThread thread;
};

typedef std::map<const char*, SpriteAtlas, cmp_str> NameToAtlas;

class World : public Collective {
 public:
  OBJECT_PROTO(World);

  World();
  World(void*);
  virtual ~World();

  virtual void update(float dt);

  void load_level(const char* level);

  SpriteAtlas atlas(const char* atlas);
  SpriteAtlasEntry atlas_entry(const char* atlas, const char* entry);

  GO* create_go();

  GO* player;
  GO* camera;

  lua_State* L;
  InputState input_state;

  DLL_DECLARE(CCollidable, collidable_node) collidables;
  NameToAtlas name_to_atlas;
};

int LCpush_world(lua_State *L, World* world);
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
  GO(void*);
  virtual ~GO();

  Component* add_component(TypeInfo* type);

  template<typename T>
  T* add_c() {
    return (T*)add_component(&T::Type);
  }

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
};

void go_set_parent(GO* child, GO* parent);
void LCpush_go(lua_State *L, GO* go);
GO* LCcheck_go(lua_State *L, int pos);
void LCpush_vector(lua_State *L, Vector vector);

template<typename Func>
void world_foreach(World* world, Vector pos, float rad, Func func) {
  DLLNode node = world->children.head;
  float rad2 = rad * rad;
  while(node) {
    GO* go = (GO*)world->children.to_element(node);
    Vector_ p;
    go->pos(&p);
    if(vector_dist2(&p, pos) < rad2 &&  func(go)) {
      return;
    }
    node = node->next;
  }
}

#endif
