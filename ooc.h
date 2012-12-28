#ifndef OOC_H
#define OOC_H

#include <lua.hpp>
#include <stdio.h>
#include <map>

class Object;

typedef Object*(*CtorFn)(void*);

struct cmp_str {
  bool operator()(char const *a, char const *b);
};

class PropertyInfo;
typedef std::map<const char*, PropertyInfo*, cmp_str> NameToProperty;

class TypeInfo {
public:
  TypeInfo(const char* name, CtorFn ctor);
  void register_property(PropertyInfo* property);
  PropertyInfo* property(const char* name);

  const char* name() const;
  Object* makeInstance(void*) const;
  bool isInstanceOf(const TypeInfo* other) const;

private:
  NameToProperty name_to_property;
  const char* m_name;
  CtorFn m_ctor;
};

class PropertyType;

class PropertyInfo {
 public:
  PropertyInfo(TypeInfo* type, PropertyType* ptype, const char* name, size_t offset);

  const char* name() const;

  void set_value(Object* obj, void* value);
  void get_value(Object* obj, void* value);
  void LCpush_value(Object* obj, lua_State* L);
  void LCset_value(Object* obj, lua_State* L, int pos);

  TypeInfo* m_type;
  PropertyType* m_propertyType;
  const char* m_name;
  size_t m_offset;
};

class PropertyType {
 public:
  virtual void set_value(PropertyInfo* info, Object* obj, void* value) = 0;
  virtual void get_value(PropertyInfo* info, Object* obj, void* value) = 0;

  virtual void LCpush_value(PropertyInfo* info, Object* obj, lua_State* L) = 0;
  virtual void LCset_value(PropertyInfo* info, Object* obj, lua_State* L, int pos) = 0;
};

template <typename T>
class PropertyTypeImpl : public PropertyType {
  virtual void set_value(PropertyInfo* info, Object* obj, void* value) {
    memcpy((char*)obj + info->m_offset, value, sizeof(T));
  }

  virtual void get_value(PropertyInfo* info, Object* obj, void* value) {
    memcpy(value, (char*)obj + info->m_offset, sizeof(T));
  }

  virtual void LCpush_value(PropertyInfo* info, Object* obj, lua_State* L) {
    luaL_error(L, "don't know how to maniplate `%s'", info->name());
  }

  virtual void LCset_value(PropertyInfo* info, Object* obj, lua_State* L, int pos) {
    luaL_error(L, "don't know how to maniplate `%s'", info->name());
  }
};

typedef std::map<const char*, TypeInfo*, cmp_str> NameToType;

class TypeRegistry {
 public:
  static TypeRegistry& instance();

  void register_type(TypeInfo* type);
  TypeInfo* find_type(const char* name);

  NameToType name_to_type;
};

#define OBJECT_PROTO(name)                            \
  virtual const TypeInfo* typeinfo();                 \
  static TypeInfo Type;                               \
  static Object* CreateInstance(void*)


#define OBJECT_BIMPL(name)                              \
  TypeInfo name::Type(#name, name::CreateInstance);     \
  const TypeInfo* name::typeinfo() {                    \
    return &(name::Type);                               \
  }

#define OBJECT_IMPL(name)                               \
  Object* name::CreateInstance(void* init) {            \
    return new name(init);                              \
  }                                                     \
  OBJECT_BIMPL(name)

#define OPMTYPE(name, member_name) name ## _ ## member_name ## _ ## MemberType
#define OPTYPE(name, member_name) name ## _ ## member_name ## _ ## Type
#define OP(name, member_name) name ## _ ## member_name ## _ ## Property

#define OBJECT_PROPERTY(name, member_name)                              \
  static PropertyTypeImpl<decltype(((name*)0)->member_name)>            \
    OPTYPE(name, member_name);                                          \
                                                                        \
  static PropertyInfo OP(name, member_name)(                            \
    &name::Type,                                                        \
    &OPTYPE(name, member_name),                                         \
    #member_name,                                                       \
    offsetof(name, member_name));

class Object {
public:
  OBJECT_PROTO(Object);
};

#endif
