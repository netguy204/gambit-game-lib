#include "gameobject.h"
#include "memory.h"
#include "config.h"
#include "testlib.h"
#include "utils.h"

#include <stdarg.h>

template<>
void PropertyTypeImpl<lua_State*>::LCpush_value(const PropertyInfo* info, Object* obj, lua_State* L) {
  luaL_error(L, "cannot read threads");
}

template<>
void PropertyTypeImpl<lua_State*>::LCset_value(const PropertyInfo* info, Object* obj, lua_State* L, int pos) {
  lua_State* state = lua_tothread(L, pos);
  if(!state) {
    luaL_error(L, "position %d does not contain a thread", pos);
  }

  set_value(info, obj, &state);
}

template<>
void PropertyTypeImpl<Vector_>::LCpush_value(const PropertyInfo* info, Object* obj, lua_State* L) {
  Vector_ v;
  get_value(info, obj, &v);

  lua_createtable(L, 2, 0);
  lua_pushnumber(L, v.x);
  lua_rawseti(L, -2, 1);
  lua_pushnumber(L, v.y);
  lua_rawseti(L, -2, 2);
}

template<>
void PropertyTypeImpl<Vector_>::LCset_value(const PropertyInfo* info, Object* obj, lua_State* L, int pos) {
  if(!lua_istable(L, pos)) {
    luaL_argerror(L, pos, "`table' expected");
  }

  Vector_ v;
  lua_rawgeti(L, pos, 1);
  v.x = luaL_checknumber(L, -1);
  lua_rawgeti(L, pos, 2);
  v.y = luaL_checknumber(L, -1);
  lua_pop(L, 2);
  set_value(info, obj, &v);
}

template<>
void PropertyTypeImpl<GO*>::LCpush_value(const PropertyInfo* info, Object* obj, lua_State* L) {
  GO* go;
  get_value(info, obj, &go);
  LCpush_go(L, go);
}


template<>
void PropertyTypeImpl<GO*>::LCset_value(const PropertyInfo* info, Object* obj, lua_State* L, int pos) {
  GO* go = LCcheck_go(L, pos);
  set_value(info, obj, &go);
}

OBJECT_IMPL(GO);
OBJECT_PROPERTY(GO, ttag);
OBJECT_PROPERTY(GO, _pos);
OBJECT_PROPERTY(GO, _vel);
OBJECT_PROPERTY(GO, transform_parent);

GO::GO() {
  this->transform_parent = NULL;
  vector_zero(&this->_pos);
  vector_zero(&this->_vel);
  this->ttag = 0;
}

GO::GO(void* p) {
  throw std::exception();
}

// removal from the world is handled by the terminate message. Users
// shouldn't write "delete go;" as this won't handle world cleanup
// correctly
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

Component* GO::add_component(TypeInfo* type) {
  return (Component*)type->makeInstance(this);
}

void GO::update(float dt) {
  // do an integration step
  struct Vector_ dx;
  vector_scale(&dx, &this->_vel, dt);
  vector_add(&this->_pos, &this->_pos, &dx);

  this->components.foreach([=](Component* comp) -> int {
      if(comp->delete_me) {
        delete(comp);
      } else {
        comp->update(dt);
      }
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
        if(comp->delete_me) {
          result = NULL;
        } else {
          result = comp;
        }
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
    // make sure the parent isn't already a child
    int child_abort = 0;
    child->transform_children.foreach_node([&](DLLNode node) -> int {
        GO* current_child = container_of(node, GO, transform_siblings);
        if(current_child == parent) {
          child_abort = 1;
          return 1;
        }
        return 0;
      });

    // don't allow the parenting
    if(child_abort) return;

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
OBJECT_PROPERTY(Component, delete_me);

Component::Component(void* p)
  : go(NULL), delete_me(0) {
}

Component::Component(GO* go, ComponentPriority priority)
  : go(NULL), priority(priority), delete_me(0) {

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
OBJECT_PROPERTY(CCollidable, w);
OBJECT_PROPERTY(CCollidable, h);
OBJECT_PROPERTY(CCollidable, mask);

CCollidable::CCollidable(void* go)
  : Component((GO*)go, PRIORITY_LEAST), w(0), h(0), mask(MASK_PLATFORMER) {
  if(this->go) {
    this->go->world->collidables.add_head(this);
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

OBJECT_IMPL(CScripted);
OBJECT_PROPERTY(CScripted, thread);

CScripted::CScripted(void* go)
  : Component((GO*)go, PRIORITY_ACT), thread(NULL) {
}

CScripted::~CScripted() {
  if(thread) {
    lua_close(thread);
  }
}

void CScripted::update(float dt) {
  if(!thread) return;

  LCpush_go(thread, go);
  lua_pushnumber(thread, dt);
  int status = lua_resume(thread, NULL, 2);
  if(status != LUA_YIELD) {
    delete_me = 1;
    if(status != LUA_OK) {
      const char* error = lua_tostring(thread, -1);
      fail_exit("lua thread failed: %s", error);
    }
  }
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

#define LUT_WORLD "World"
#define LUT_GO "Go"
#define LUT_COMPONENT "Component"

static void LCpush_lut(lua_State *L, const char* metatable, void* ut) {
  if(!ut) {
    lua_pushnil(L);
  } else {
    void** p = (void**)lua_newuserdata(L, sizeof(void*));
    luaL_setmetatable(L, metatable);
    *p = ut;
  }
}

static void* LCcheck_lut(lua_State *L, const char* metatable, int pos) {
  static char error_msg[256];

  if(lua_isnil(L, pos)) {
    return NULL;
  }

  void **ud;
  if(metatable) {
    ud = (void**)luaL_checkudata(L, pos, metatable);
  } else {
    ud = (void**)lua_touserdata(L, pos);
  }

  if(ud == NULL) {
    snprintf(error_msg, sizeof(error_msg), "`%s' expected", metatable);
    luaL_argcheck(L, ud != NULL, pos, error_msg);
  }
  return *ud;
}

static World* LCcheck_world(lua_State *L, int pos) {
  return (World*)LCcheck_lut(L, LUT_WORLD, pos);
}

int LCpush_world(lua_State *L, World* world) {
  LCpush_lut(L, LUT_WORLD, world);
  return 1;
}

GO* LCcheck_go(lua_State *L, int pos) {
  return (GO*)LCcheck_lut(L, LUT_GO, pos);
}

void LCpush_go(lua_State *L, GO* go) {
  LCpush_lut(L, LUT_GO, go);
}

static int Lworld_create_go(lua_State *L) {
  World* world = LCcheck_world(L, 1);
  LCpush_go(L, world->create_go());
  return 1;
}

static int Lworld_atlas_entry(lua_State *L) {
  World* world = LCcheck_world(L, 1);
  const char* atlas = luaL_checkstring(L, 2);
  const char* entry = luaL_checkstring(L, 3);
  SpriteAtlasEntry result = world->atlas_entry(atlas, entry);
  lua_pushlightuserdata(L, result);
  return 1;
}

static Component* LCcheck_component(lua_State *L, int pos) {
  return (Component*)LCcheck_lut(L, LUT_COMPONENT, pos);
}

static TypeInfo* LCcheck_type(lua_State *L, int pos) {
  const char* name = luaL_checkstring(L, pos);
  TypeInfo* type = TypeRegistry::instance().find_type(name);
  if(type == NULL) {
    luaL_error(L, "`%s' does not name a registered type", name);
  }
  return type;
}

static Object* LCcheck_object(lua_State *L, int pos) {
  return (Object*)LCcheck_lut(L, NULL, pos);
}

static void LCpush_component(lua_State *L, Component *comp) {
  LCpush_lut(L, LUT_COMPONENT, comp);
}

static int Lgo_find_component(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  TypeInfo* type = LCcheck_type(L, 2);

  Component* comp = go->find_component(type);
  if(comp == NULL) {
    lua_pushnil(L);
  } else {
    LCpush_component(L, comp);
  }
  return 1;
}

static int Lgo_add_component(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  TypeInfo* type = LCcheck_type(L, 2);
  Component* comp = go->add_component(type);

  // argument 3 should be an optional table
  if(lua_gettop(L) != 3) {
    LCpush_component(L, comp);
    return 1;
  }

  if(!lua_istable(L, 3)) {
    luaL_error(L, "argument 3 should be a table");
    return 0;
  }

  // iterate the table using the keys as property names
  lua_pushnil(L);
  while(lua_next(L, 3)) {
    const char* pname = luaL_checkstring(L, -2);
    const PropertyInfo* prop = type->property(pname);
    if(!prop) {
      luaL_error(L, "`%s' does not name a property of `%s'",
                 pname, type->name());
    }
    prop->LCset_value(comp, L, -1);
    lua_pop(L, 1); // pop the value, leave the key
  }

  LCpush_component(L, comp);
  return 1;
}

static int Lgo_has_message(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  int type = luaL_checkinteger(L, 2);

  int result = 0;
  go->inbox.foreach([&] (Message* msg) -> int {
      if(msg->kind == type) {
        result = 1;
        return 1;
      }
      return 0;
    });

  if(result) {
    lua_pushinteger(L, 1);
  } else {
    lua_pushnil(L);
  }

  return 1;
}

static int Lgo_send_terminate(lua_State* L) {
  GO* go = LCcheck_go(L, 1);
  agent_send_terminate(go, go->world);
  return 0;
}

static int Lobject_mutate(lua_State* L) {
  const char* name = luaL_checkstring(L, lua_upvalueindex(1));
  Object* obj = LCcheck_object(L, 1);

  const PropertyInfo* prop = obj->typeinfo()->property(name);
  if(prop == NULL) {
    luaL_error(L, "`%s' does not have property `%s'",
               obj->typeinfo()->name(), name);
  }

  if(lua_gettop(L) == 1) {
    prop->LCpush_value(obj, L);
    return 1;
  } else {
    prop->LCset_value(obj, L, 2);
    return 0;
  }
}

static int Lobject_index(lua_State* L) {
  const char* name = luaL_checkstring(L, 2);
  lua_pushcclosure(L, Lobject_mutate, 1);
  return 1;
}

static int Lobject_tostring(lua_State *L) {
  Object* obj = LCcheck_object(L, 1);
  lua_pushstring(L, obj->typeinfo()->name());
  return 1;
}

void LClink_metatable(lua_State *L, const char* name, const luaL_Reg* table) {
  static const luaL_Reg object_m[] = {
    {"__index", Lobject_index},
    {NULL, NULL}};

  luaL_newmetatable(L, name);
  lua_pushstring(L, "__index");
  lua_pushvalue(L, -2);
  lua_settable(L, -3);
  luaL_setfuncs(L, table, 0);

  lua_newtable(L);
  luaL_setfuncs(L, object_m, 0);
  lua_setmetatable(L, -2);
}

OBJECT_IMPL(World);

void init_lua(World* world) {
  world->player = world->create_go();
  world->camera = world->create_go();

  lua_State* L = luaL_newstate();
  world->L = L;

  luaL_openlibs(L);

  static const luaL_Reg world_m[] = {
    {"create_go", Lworld_create_go},
    {"atlas_entry", Lworld_atlas_entry},
    {"__tostring", Lobject_tostring},
    {NULL, NULL}};

  LClink_metatable(L, LUT_WORLD, world_m);

  static const luaL_Reg go_m[] = {
    {"add_component", Lgo_add_component},
    {"find_component", Lgo_find_component},
    {"has_message", Lgo_has_message},
    {"send_terminate", Lgo_send_terminate},
    {"__tostring", Lobject_tostring},
    {NULL, NULL}};

  LClink_metatable(L, LUT_GO, go_m);

  static const luaL_Reg component_m[] = {
    {"__tostring", Lobject_tostring},
    {NULL, NULL}};

  LClink_metatable(L, LUT_COMPONENT, component_m);

  LCpush_world(L, world);
  lua_setglobal(L, "world");

  LCpush_go(L, world->player);
  lua_setglobal(L, "player");

  LCpush_go(L, world->camera);
  lua_setglobal(L, "camera");

  lua_pushnumber(L, screen_width);
  lua_setglobal(L, "screen_width");

  lua_pushnumber(L, screen_height);
  lua_setglobal(L, "screen_height");

  lua_pop(L, lua_gettop(L));
}

World::World(void*p)
  : L(NULL) {
  init_lua(this);
}
World::World()
  : L(NULL) {
  init_lua(this);
}

World::~World() {
  for(NameToAtlas::iterator iter = name_to_atlas.begin();
      iter != name_to_atlas.end(); ++iter) {
    spriteatlas_free(iter->second);
  }
}

void World::load_level(const char* level) {
  if(!luaL_dofile(L, level)) {
    lua_getglobal(L, "level_init");
    if(!lua_isnil(L, -1)) {
      lua_call(L, 0, 0);
    } else {
      lua_pop(L, 1);
      fail_exit("`level_init' was not defined after loading %s", level);
    }
  } else {
    const char* error = lua_tostring(L, -1);
    fail_exit("level loading failed: %s", error);
  }
}

GO* World::create_go() {
  GO* go = new GO();
  Message* message = message_make(NULL, COLLECTIVE_ADD_AGENT, go);
  message_postinbox(this, message);
  go->world = this;
  return go;
}

SpriteAtlas World::atlas(const char* atlas_name) {
  NameToAtlas::iterator iter = name_to_atlas.find(atlas_name);
  SpriteAtlas atlas;

  if(iter == name_to_atlas.end()) {
    atlas = spriteatlas_load(atlas_name, "png");
    name_to_atlas.insert(std::make_pair(atlas_name, atlas));
  } else {
    atlas = iter->second;
  }

  return atlas;
}

SpriteAtlasEntry World::atlas_entry(const char* atlas_name, const char* entry) {
  return spriteatlas_find(atlas(atlas_name), entry);
}
