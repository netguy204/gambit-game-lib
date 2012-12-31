#include "gameobject.h"
#include "memory.h"
#include "config.h"
#include "testlib.h"
#include "utils.h"

#include <math.h>

void LCpush_entry(lua_State* L, SpriteAtlasEntry entry) {
  lua_newtable(L);
  lua_pushlightuserdata(L, entry);
  lua_setfield(L, -2, "entry");
  lua_pushinteger(L, entry->w);
  lua_setfield(L, -2, "w");
  lua_pushinteger(L, entry->h);
  lua_setfield(L, -2, "h");
}

template<>
void PropertyTypeImpl<SpriteAtlasEntry>::LCpush_value(Object* obj, lua_State* L) const {
  SpriteAtlasEntry entry;
  get_value(obj, &entry);
  LCpush_entry(L, entry);
}

template<>
void PropertyTypeImpl<SpriteAtlasEntry>::LCset_value(Object* obj, lua_State* L, int pos) const {
  if(!lua_istable(L, pos)) {
    luaL_error(L, "position %d does not contain a table", pos);
  }

  lua_getfield(L, pos, "entry");
  SpriteAtlasEntry entry = (SpriteAtlasEntry)lua_touserdata(L, -1);
  if(!entry) {
    luaL_error(L, "table at position %d does not have lightuserdata at `entry'", pos);
  }

  lua_pop(L, 1);
  set_value(obj, &entry);
}

template<>
void PropertyTypeImpl<lua_State*>::LCset_value(Object* obj, lua_State* L, int pos) const {
  lua_State* state = lua_tothread(L, pos);
  if(!state) {
    luaL_error(L, "position %d does not contain a thread", pos);
  }

  // special arrangement with our possible set_value client says that
  // we leave the refid on the thread's stack (and clean it off in
  // case it's a trivial client... which will leak since it can't
  // un-retain the thread)
  lua_pushvalue(L, pos);
  int refid = luaL_ref(L, LUA_REGISTRYINDEX);
  lua_pushinteger(state, refid);
  set_value(obj, &state);
  lua_pop(state, 1);
}

void LCpush_vector(lua_State* L, Vector v) {
  lua_createtable(L, 2, 0);
  lua_pushnumber(L, v->x);
  lua_rawseti(L, -2, 1);
  lua_pushnumber(L, v->y);
  lua_rawseti(L, -2, 2);
}

template<>
void PropertyTypeImpl<Vector_>::LCpush_value(Object* obj, lua_State* L) const {
  Vector_ v;
  get_value(obj, &v);
  LCpush_vector(L, &v);
}

template<>
void PropertyTypeImpl<Vector_>::LCset_value(Object* obj, lua_State* L, int pos) const {
  if(!lua_istable(L, pos)) {
    luaL_argerror(L, pos, "`table' expected");
  }

  Vector_ v;
  lua_rawgeti(L, pos, 1);
  v.x = luaL_checknumber(L, -1);
  lua_pop(L, 1);
  lua_rawgeti(L, pos, 2);
  v.y = luaL_checknumber(L, -1);
  lua_pop(L, 1);
  set_value(obj, &v);
}

template<>
void PropertyTypeImpl<GO*>::LCpush_value(Object* obj, lua_State* L) const {
  GO* go;
  get_value(obj, &go);
  LCpush_go(L, go);
}


template<>
void PropertyTypeImpl<GO*>::LCset_value(Object* obj, lua_State* L, int pos) const {
  GO* go = LCcheck_go(L, pos);
  set_value(obj, &go);
}

template<>
void PropertyTypeImpl<InputState>::LCpush_value(Object* obj, lua_State* L) const {
  InputState state;
  get_value(obj, &state);

  lua_newtable(L);
  lua_pushliteral(L, "quit_requested");
  lua_pushinteger(L, state->quit_requested);
  lua_settable(L, -3);

  lua_pushliteral(L, "updown");
  lua_pushnumber(L, state->updown);
  lua_settable(L, -3);

  lua_pushliteral(L, "leftright");
  lua_pushnumber(L, state->leftright);
  lua_settable(L, -3);

  lua_pushliteral(L, "action1");
  lua_pushboolean(L, state->action1);
  lua_settable(L, -3);

  lua_pushliteral(L, "action2");
  lua_pushboolean(L, state->action2);
  lua_settable(L, -3);

  lua_pushliteral(L, "action3");
  lua_pushboolean(L, state->action3);
  lua_settable(L, -3);
}

template<>
void PropertyTypeImpl<Message*>::LCset_value(Object* obj, lua_State* L, int pos) const {
  Message* message = (Message*)lua_touserdata(L, pos);
  luaL_argcheck(L, message != NULL, pos, "`Message' expected");
  set_value(obj, &message);
}

Message::Message(GO* source, int kind, void* data)
  : source(source), kind(kind), data(data) {
}

void* Message::operator new(size_t size) {
  return frame_alloc(size);
}

void Message::operator delete(void* obj) {
  fail_exit("no need to delete messages");
}

Scene::Scene(World* world)
  : world(world) {
  memset(layers, 0, sizeof(layers));
  memset(particles, 0, sizeof(particles));
}

void Scene::addRelative(SpriteList* list, Sprite sprite) {
  sprite->displayX -= dx;
  sprite->displayY -= dy;
  addAbsolute(list, sprite);
}

void Scene::addAbsolute(SpriteList* list, Sprite sprite) {
  if(sprite->displayX + sprite->w < 0
     || sprite->displayX - sprite->w > screen_width) return;

  if(sprite->displayY + sprite->h < 0
     || sprite->displayY - sprite->h > screen_height) return;

  *list = frame_spritelist_append(*list, sprite);
}

void Scene::start() {
  Vector_ cpos;
  world->camera->pos(&cpos);
  dx = floorf(cpos.x);
  dy = floorf(cpos.y);
  camera_rect.minx = dx;
  camera_rect.miny = dy;
  camera_rect.maxx = dx + screen_width;
  camera_rect.maxy = dy + screen_height;
}

void Scene::enqueue() {
  for(int ii = 0; ii < LAYER_MAX; ++ii) {
    if(layers[ii]) {
      spritelist_enqueue_for_screen(layers[ii]);
    }
    if(particles[ii]) {
      spritelist_enqueue_for_screen_colored(particles[ii]);
    }
    layers[ii] = NULL;
    particles[ii] = NULL;
  }
}

OBJECT_IMPL(GO, Object);
OBJECT_ACCESSOR(GO, gravity_scale, get_gravity_scale, set_gravity_scale);
OBJECT_ACCESSOR(GO, pos, slow_get_pos, slow_set_pos);
OBJECT_ACCESSOR(GO, vel, slow_get_vel, slow_set_vel);
OBJECT_PROPERTY(GO, delete_me);

GO::GO(void* _world)
  : world((World*)_world), delete_me(0) {
  world->game_objects.add_head(this);

  // the manual says to avoid setting major body properties (like
  // position or dynamic/static) after creation... i'll need to keep
  // tabs on whether this becomes a real issue.
  b2BodyDef bodyDef;
  bodyDef.fixedRotation = true;
  body = world->bWorld.CreateBody(&bodyDef);
  body->SetUserData(this);
}

GO::~GO() {
  this->components.foreach([](Component* comp) -> int {
      delete comp;
      return 0;
    });

  world->game_objects.remove(this);

  // if we had pending messages we need to leave the world pending
  // list
  if(inbox_pending.head) {
    world->have_waiting_messages.remove(this);
  }

  world->bWorld.DestroyBody(body);
}

Component* GO::add_component(TypeInfo* type) {
  return (Component*)type->makeInstance(this);
}

void GO::update(float dt) {
  // initialize new components
  uninitialized_components.foreach([this](Component* comp) -> int {
      comp->init();
      uninitialized_components.remove(comp);
      components.insert_before_when(comp, [&comp, this](Component* other) {
          return comp->priority < other->priority;
        });
      world->components.insert_before_when(comp, [&](Component* other) {
          return comp->priority < other->priority;
        });
      return 0;
    });
}

void GO::messages_received() {
  // notify components
  components.foreach([](Component* comp) -> int {
      comp->messages_received();
      return 0;
    });

  // clear the inbox. remember, messages are stack allocated so we
  // don't need to free them explicitly
  inbox.zero();
}

float GO::get_gravity_scale() {
  return body->GetGravityScale();
}

void GO::set_gravity_scale(float scale) {
  body->SetGravityScale(scale);
}

void GO::pos(Vector pos) {
  const b2Vec2& bPos = body->GetPosition();
  pos->x = bPos.x * BSCALE;
  pos->y = bPos.y * BSCALE;
}

void GO::vel(Vector vel) {
  const b2Vec2& bVel = body->GetLinearVelocity();
  vel->x = bVel.x * BSCALE;
  vel->y = bVel.y * BSCALE;
}

void GO::set_pos(Vector pos) {
  b2Vec2 bPos;
  bPos.x = pos->x / BSCALE;
  bPos.y = pos->y / BSCALE;
  body->SetTransform(bPos, body->GetAngle());
}

void GO::set_vel(Vector vel) {
  b2Vec2 bVel;
  bVel.x = vel->x / BSCALE;
  bVel.y = vel->y / BSCALE;
  body->SetLinearVelocity(bVel);
}

Vector_ GO::slow_get_pos() {
  Vector_ result;
  pos(&result);
  return result;
}

Vector_ GO::slow_get_vel() {
  Vector_ result;
  vel(&result);
  return result;
}

void GO::slow_set_pos(Vector_ pos) {
  set_pos(&pos);
}

void GO::slow_set_vel(Vector_ vel) {
  set_vel(&vel);
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

  if(result) {
    return result;
  }

  // try the uninitialized components
  this->uninitialized_components.foreach([&](Component* comp) -> int {
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

void GO::print_description() {
  fprintf(stderr, "UNINITIALIZED COMPONENTS\n");
  uninitialized_components.foreach([](Component* c) -> int {
      fprintf(stderr, "%s\n", c->typeinfo()->name());
      return 0;
    });
  fprintf(stderr, "ACTIVE COMPONENTS\n");
  components.foreach([](Component* c) -> int {
      fprintf(stderr, "%s\n", c->typeinfo()->name());
      return 0;
    });
}

Message* GO::create_message(int kind) {
  return new Message(this, kind, NULL);
}

void GO::send_message(Message* message) {
  if(inbox_pending.is_empty()) {
    world->have_waiting_messages.add_head(this);
  }

  inbox_pending.add_tail(message);
}

OBJECT_IMPL(Component, Object);
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
    this->go->world->components.remove(this);
  }
}

void Component::init() {
}

void Component::update(float dt) {
}

void Component::messages_received() {
}

Scene* Component::scene() {
  return &go->world->scene;
}

GO* Component::player() {
  return go->world->player;
}

GO* Component::camera() {
  return go->world->camera;
}

void Component::set_parent(GO* go) {
  if(this->go) {
    this->go->components.remove(this);
  }

  this->go = go;

  if(go) {
    go->uninitialized_components.add_head(this);
  }
}

OBJECT_IMPL(CCollidable, Component);
OBJECT_PROPERTY(CCollidable, offset);
OBJECT_PROPERTY(CCollidable, w);
OBJECT_PROPERTY(CCollidable, h);

CCollidable::CCollidable(void* go)
  : Component((GO*)go, PRIORITY_LEAST), w(0), h(0), fixture(NULL) {
  vector_zero(&offset);
}

CCollidable::~CCollidable() {
  go->body->DestroyFixture(fixture);
}

void CCollidable::init() {
  b2Vec2 center;
  center.x = offset.x / BSCALE;
  center.y = offset.y / BSCALE;

  b2PolygonShape shape;
  shape.SetAsBox((w/2)/BSCALE, (h/2)/BSCALE, center, 0);
  fixture = go->body->CreateFixture(&shape, 0);
}

LuaThread::LuaThread()
  : state(NULL), refid(-1) {
}

OBJECT_IMPL(CScripted, Component);
OBJECT_ACCESSOR(CScripted, update_thread, get_update_thread, set_update_thread);
OBJECT_ACCESSOR(CScripted, message_thread, get_message_thread, set_message_thread);

CScripted::CScripted(void* go)
  : Component((GO*)go, PRIORITY_ACT) {
}

CScripted::~CScripted() {
  // can't do this in the LuaThread destructor because we need to make
  // temporaries
  free_thread(&update_thread);
  free_thread(&message_thread);
}

void CScripted::init() {
  if(!update_thread.state && !message_thread.state) {
    fprintf(stderr, "CScripted initialized with no attached script\n");
    delete_me = 1;
    return;
  }

  step_thread(&update_thread);
  step_thread(&message_thread);
}

void CScripted::free_thread(LuaThread* thread) {
  if(thread->state) {
    luaL_unref(thread->state, LUA_REGISTRYINDEX, thread->refid);
    thread->state = NULL;
  }
}

void CScripted::set_thread(LuaThread* target, lua_State* thread) {
  // release old reference
  if(target->state) {
    luaL_unref(target->state, LUA_REGISTRYINDEX, target->refid);
  }

  // our special arrangement with LCset_value
  // ensures that the top of the lua thread stack contains the thread's
  // refid
  target->state = thread;
  target->refid = lua_tointeger(thread, -1);
  target->is_initialized = 0;
}

void CScripted::set_update_thread(lua_State* thread) {
  set_thread(&update_thread, thread);
}

void CScripted::set_message_thread(lua_State* thread) {
  set_thread(&message_thread, thread);
}

lua_State* CScripted::get_update_thread() {
  return NULL;
}

lua_State* CScripted::get_message_thread() {
  return NULL;
}

void CScripted::step_thread(LuaThread* thread) {
  if(thread->state) {
    if(!thread->is_initialized) {
      thread->is_initialized = 1;
      LCpush_go(thread->state, go);
      LCpush_component(thread->state, this);
      resume(thread, 2);
    }

    resume(thread, 0);
  }
}

void CScripted::update(float dt) {
  // init guaranteed that we always have a script
  step_thread(&update_thread);
}

void CScripted::messages_received() {
  step_thread(&message_thread);
}

void CScripted::resume(LuaThread* thread, int args) {
  int status = lua_resume(thread->state, NULL, args);
  if(status != LUA_YIELD) {
    free_thread(thread);

    if(status != LUA_OK) {
      const char* error = lua_tostring(thread->state, -1);
      fail_exit("lua thread failed: %s", error);
    }

    // when both threads exit, remove ourselves
    if(!message_thread.state && !update_thread.state) {
      delete_me = 1;
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
  if(!result) {
    luaL_error(L, "`%s' is not a valid entry in `%s'", entry, atlas);
  }

  LCpush_entry(L, result);
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

void LCpush_component(lua_State *L, Component *comp) {
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

static int Lgo_create_message(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  int kind = luaL_checkinteger(L, 2);
  Message* message = go->create_message(kind);
  lua_pushlightuserdata(L, message);
  return 1;
}

static int Lgo_send_message(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  Message* message = (Message*)lua_touserdata(L, 2);
  go->send_message(message);
  return 0;
}

static int Lgo_broadcast_message(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  float range = luaL_checknumber(L, 2);
  int kind = luaL_checkinteger(L, 3);

  go->world->broadcast_message(go, range, kind);
  return 0;
}

static int Lgo_has_message(lua_State *L) {
  GO* go = LCcheck_go(L, 1);
  int type = luaL_checkinteger(L, 2);

  Message* found = NULL;
  go->inbox.foreach([&] (Message* msg) -> int {
      if(msg->kind == type) {
        found = msg;
        return 1;
      }
      return 0;
    });

  if(found) {
    lua_pushlightuserdata(L, found);
  } else {
    lua_pushnil(L);
  }

  return 1;
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

OBJECT_IMPL(World, Object);
OBJECT_PROPERTY(World, input_state);
OBJECT_PROPERTY(World, focus);
OBJECT_PROPERTY(World, dt);

void init_lua(World* world) {
  world->player = world->create_go();
  world->camera = world->create_go();
  world->stage = world->create_go();

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
    {"create_message", Lgo_create_message},
    {"send_message", Lgo_send_message},
    {"broadcast_message", Lgo_broadcast_message},
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

  LCpush_go(L, world->stage);
  lua_setglobal(L, "stage");

  lua_pushnumber(L, screen_width);
  lua_setglobal(L, "screen_width");

  lua_pushnumber(L, screen_height);
  lua_setglobal(L, "screen_height");

  lua_pop(L, lua_gettop(L));
}

World::World(void*p)
  : L(NULL), scene(this), bWorld(b2Vec2(0, -50)) {
  init_lua(this);
}
World::World()
  : L(NULL), scene(this), bWorld(b2Vec2(0, -50)) {
  init_lua(this);
}

World::~World() {
  game_objects.foreach([](GO* go) -> int {
      delete go;
      return 0;
    });

  for(NameToAtlas::iterator iter = name_to_atlas.begin();
      iter != name_to_atlas.end(); ++iter) {
    spriteatlas_free(iter->second);
  }

  lua_close(L);
}

void World::update(float dt) {
  this->dt = dt;

  // do an integration step
  bWorld.Step(dt, 6, 2);
  update_camera(dt);

  scene.start();

  // let the game objects initialize any new components
  game_objects.foreach([=](GO* go) -> int {
      if(go->delete_me) {
        delete go;
      } else {
        go->update(dt);
      }
      return 0;
    });

  // update the comonents
  this->components.foreach([=](Component* comp) -> int {
      if(comp->delete_me) {
        delete(comp);
      } else {
        comp->update(dt);
      }
      return 0;
    });

  // run go messages until all messages have been handled
  while(have_waiting_messages.head) {
    have_waiting_messages.foreach([this](GO* go) -> int {
        // move the pending messages into the inbox and remove from
        // waiting list
        go->inbox = go->inbox_pending;
        go->inbox_pending.zero();
        have_waiting_messages.remove(go);

        // tell GO to consume messages
        go->messages_received();
        return 0;
      });
  }
}

void World::update_camera(float dt) {
  if(!focus) return;
  Vector_ offset = {screen_width / 2.0f, screen_height / 2.0f};
  Vector_ desired;
  focus->pos(&desired);
  vector_sub(&desired, &desired, &offset);

  float max_v = 1600;
  const float max_dx = max_v * dt;

  Vector_ cpos;
  camera->pos(&cpos);

  Vector_ delta;
  vector_sub(&delta, &desired, &cpos);
  float mag = vector_mag(&delta);
  if(mag < max_dx) {
    // snap
    camera->set_pos(&desired);
    return;
  }

  vector_scale(&delta, &delta, max_dx / mag);
  vector_add(&cpos, &cpos, &delta);
  camera->set_pos(&cpos);
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
  GO* go = new GO(this);
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

void World::broadcast_message(GO* sender, float radius, int kind) {
  Vector_ pos;
  sender->pos(&pos);

  world_foreach(this, &pos, radius, [=](GO* go) -> int {
      go->send_message(sender->create_message(kind));
      return 0;
    });
}
