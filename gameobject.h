#ifndef GAMEOBJECT_H
#define GAMEOBJECT_H

#include "vector.h"
#include "rect.h"
#include "spriteatlas.h"
#include "ooc.h"

#include <lua.hpp>
#include <map>

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

  struct DLLNode_ node;
  GO* source;
  void* data;
  void* data2;
  int kind;
  int read_count;
};

enum RenderLayers {
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

  GO();
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
  void pos(Vector p);
  void vel(Vector v);
  Component* find_component(const TypeInfo* info);
  void print_description();

  struct DLLNode_ messages_waiting_node;
  struct DLLNode_ world_node;

  struct DLLNode_ transform_siblings;
  struct GO* transform_parent;
  SimpleDLL transform_children;

  DLL_DECLARE(Component, node) components;
  DLL_DECLARE(Component, node) uninitialized_components;
  DLL_DECLARE(Message, node) inbox;
  DLL_DECLARE(Message, node) inbox_pending;

  // pos and vel are always relative to the parent if there is one
  struct Vector_ _pos;
  struct Vector_ _vel;

  // the world we're a part of
  World* world;

  int delete_me;
};

void go_set_parent(GO* child, GO* parent);

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

  GO* create_go();

  GO* player;
  GO* camera;
  Scene scene;

  lua_State* L;
  InputState input_state;
  float dt;

  DLL_DECLARE(GO, world_node) game_objects;
  DLL_DECLARE(GO, messages_waiting_node) have_waiting_messages;

  DLL_DECLARE(Component, world_node) components;
  DLL_DECLARE(CCollidable, collidable_node) collidables;
  NameToAtlas name_to_atlas;
};

int LCpush_world(lua_State *L, World* world);
void world_notify_collisions(World* world);


template<typename Func>
void world_foreach(World* world, Vector pos, float rad, Func func) {
  DLLNode node = world->game_objects.head;
  float rad2 = rad * rad;
  while(node) {
    GO* go = world->game_objects.to_element(node);
    Vector_ p;
    go->pos(&p);
    if(vector_dist2(&p, pos) < rad2 &&  func(go)) {
      return;
    }
    node = node->next;
  }
}

#endif
