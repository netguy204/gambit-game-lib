#ifndef GAMEOBJECT_H
#define GAMEOBJECT_H

#include "vector.h"
#include "rect.h"
#include "spriteatlas.h"
#include "ooc.h"

#include <lua.hpp>
#include <Box2D/Box2D.h>

#include <map>

#define BSCALE 64.0f

typedef enum {
  MASK_NON_COLLIDER = 0,
  MASK_PLATFORM = 1,
  MASK_PLATFORMER = 2,
  MASK_ENEMY_PLATFORM = 4
} CollisionMask;

class GO;

enum MessageKind {
  MESSAGE_COLLIDING,      // agent is colliding with other (cother, cself)
  MESSAGE_TIMER_EXPIRED,  // args (payload)
  MESSAGE_EXPLOSION_NEARBY,
  MESSAGE_PARENT_CHANGE,
};

class Message {
 public:
  Message(GO* source, int kind, void* data);
  void* operator new(size_t size);
  void operator delete(void* obj);

  struct DLLNode_ node;
  GO* source;
  void* data;
  void* data2;
  int kind;
  int read_count;
};

enum RenderLayers {
  LAYER_BACKDROP,
  LAYER_BACKGROUND,
  LAYER_PLAYER,
  LAYER_FOREGROUND,
  LAYER_MAX,
};

class World;

class Scene {
 public:
  SpriteList layers[LAYER_MAX];
  SpriteList particles[LAYER_MAX];

  int dx, dy;
  Rect_ camera_rect;
  World* world;

  Scene(World* world);

  void addRelative(SpriteList* list, Sprite sprite);
  void addAbsolute(SpriteList* list, Sprite sprite);

  void start(); // after the camera is ready
  void enqueue();
};

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

  virtual void init();
  virtual void update(float dt);
  virtual void messages_received();

  Scene* scene();
  GO* player();
  GO* camera();

  void set_parent(GO* go);

  struct DLLNode_ node;
  struct DLLNode_ world_node;

  GO* go;
  ComponentPriority priority;
  int delete_me;
};

void LCpush_component(lua_State *L, Component *comp);

// registers a GO with the collidables section of the world. GO
// components will get COLLIDED messages letting them known when they
// hit other GOs that have a collidable component
class CCollidable : public Component {
 public:
  OBJECT_PROTO(CCollidable);

  CCollidable(void* go);
  virtual ~CCollidable();
  virtual void init();

  b2Fixture* fixture;
  Vector_ offset;
  float w;
  float h;
};

struct LuaThread {
  LuaThread();

  lua_State* state;
  int refid;
  int is_initialized;
  int is_valid;
};

class CScripted : public Component {
 public:
  OBJECT_PROTO(CScripted);

  CScripted(void* go);
  virtual ~CScripted();

  virtual void init();
  virtual void update(float dt);
  virtual void messages_received();

  void set_thread(LuaThread* target, lua_State* thread);
  void set_update_thread(lua_State* thread);
  void set_message_thread(lua_State* thread);
  lua_State* get_update_thread();
  lua_State* get_message_thread();

  void free_thread(LuaThread* thread);
  void step_thread(LuaThread* thread);
  void resume(LuaThread* thread, int args);

  LuaThread update_thread;
  LuaThread message_thread;
};

/* GO: GameObject
 *
 * This is an agent that makes all of its messgaes available to its
 * components. If a component ever cares about messages it should
 * search the list of messages in its parent object for whatever its
 * interested in. The parent will clear its message list and handle
 * terminations at the end of the component update cycle.
 */

class GO : public Object {
 public:
  OBJECT_PROTO(GO);

  GO(void*);
  virtual ~GO();

  Component* add_component(TypeInfo* type);

  template<typename T>
  T* add_c() {
    return (T*)add_component(&T::Type);
  }

  virtual void update(float dt);
  void messages_received();

  Message* create_message(int kind);
  void send_message(Message* message);

  float get_gravity_scale();
  void set_gravity_scale(float scale);

  // fast way (no allocation)
  void pos(Vector p);
  void vel(Vector v);
  void set_pos(Vector p);
  void set_vel(Vector v);

  // script way
  Vector_ slow_get_pos();
  Vector_ slow_get_vel();
  void slow_set_pos(Vector_ p);
  void slow_set_vel(Vector_ p);

  Component* find_component(const TypeInfo* info);
  void print_description();

  struct DLLNode_ messages_waiting_node;
  struct DLLNode_ world_node;

  DLL_DECLARE(Component, node) components;
  DLL_DECLARE(Component, node) uninitialized_components;
  DLL_DECLARE(Message, node) inbox;
  DLL_DECLARE(Message, node) inbox_pending;

  // the world we're a part of
  World* world;
  b2Body* body;

  int delete_me;
};

void LCpush_go(lua_State *L, GO* go);
GO* LCcheck_go(lua_State *L, int pos);
void LCpush_vector(lua_State *L, Vector vector);

typedef std::map<const char*, SpriteAtlas, cmp_str> NameToAtlas;

class World : public Object {
 public:
  OBJECT_PROTO(World);

  World();
  World(void*);
  virtual ~World();

  virtual void update(float dt);

  void load_level(const char* level);

  SpriteAtlas atlas(const char* atlas);
  SpriteAtlasEntry atlas_entry(const char* atlas, const char* entry);
  void broadcast_message(GO* go, float radius, int kind);

  GO* create_go();

  GO* player;
  GO* camera;
  GO* stage;

  Scene scene;

  lua_State* L;
  b2World bWorld;

  InputState input_state;
  float dt;

  DLL_DECLARE(GO, world_node) game_objects;
  DLL_DECLARE(GO, messages_waiting_node) have_waiting_messages;

  DLL_DECLARE(Component, world_node) components;

  NameToAtlas name_to_atlas;
};

int LCpush_world(lua_State *L, World* world);

template <typename Func>
class WorldCallback : public b2QueryCallback {
 public:
 WorldCallback(Func& func) : func(func) {}

  bool ReportFixture(b2Fixture* fixture) {
    return !func((GO*)fixture->GetBody()->GetUserData());
  }

  Func& func;
};

template<typename Func>
void world_foreach(World* world, Vector pos, float rad, Func func) {
  b2AABB aabb;
  aabb.lowerBound.Set((pos->x - rad)/BSCALE, (pos->y - rad)/BSCALE);
  aabb.upperBound.Set((pos->x + rad)/BSCALE, (pos->y + rad)/BSCALE);
  WorldCallback<Func> callback(func);
  world->bWorld.QueryAABB(&callback, aabb);
}

#endif
