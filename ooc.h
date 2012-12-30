#ifndef OOC_H
#define OOC_H

#include <lua.hpp>
#include <stdio.h>
#include <string.h>
#include <map>

class Object;

typedef Object*(*CtorFn)(void*);

struct cmp_str {
  bool operator()(char const *a, char const *b) const;
};

class PropertyInfo;
typedef std::map<const char*, PropertyInfo*, cmp_str> NameToProperty;

class TypeInfo {
public:
  TypeInfo(const char* name, CtorFn ctor, TypeInfo* parent);
  void register_property(PropertyInfo* property);
  const PropertyInfo* property(const char* name) const;

  const char* name() const;
  const TypeInfo* parent() const;
  Object* makeInstance(void*) const;
  bool isInstanceOf(const TypeInfo* other) const;

private:
  NameToProperty name_to_property;
  const char* m_name;
  CtorFn m_ctor;
  TypeInfo* m_parent;
};

class PropertyType;

class PropertyInfo {
 public:
  PropertyInfo(TypeInfo* type, const char* name);

  const char* name() const;

  virtual void set_value(Object* obj, void* value) const = 0;
  virtual void get_value(Object* obj, void* value) const = 0;
  virtual void LCpush_value(Object* obj, lua_State* L) const = 0;
  virtual void LCset_value(Object* obj, lua_State* L, int pos) const = 0;

  TypeInfo* m_type;
  const char* m_name;
};

template <typename T>
class PropertyTypeImpl : public PropertyInfo {
public:
  PropertyTypeImpl(TypeInfo* type, const char* name, size_t offset)
    : PropertyInfo(type, name), m_offset(offset) {
  }

  virtual void set_value(Object* obj, void* value) const {
    memcpy((char*)obj + m_offset, value, sizeof(T));
  }

  virtual void get_value(Object* obj, void* value) const {
    memcpy(value, (char*)obj + m_offset, sizeof(T));
  }

  virtual void LCpush_value(Object* obj, lua_State* L) const {
    luaL_error(L, "don't know how to read `%s'", name());
  }

  virtual void LCset_value(Object* obj, lua_State* L, int pos) const {
    luaL_error(L, "don't know how to write `%s'", name());
  }

  size_t m_offset;
};

template <typename Sig>
class PropertyMethodImpl;

template <typename CLASS, typename T>
class PropertyMethodImpl<void (CLASS::*)(T)> : public PropertyTypeImpl<T> {
public:
  typedef T (CLASS::*Getter)();
  typedef void (CLASS::*Setter)(T);

  PropertyMethodImpl(TypeInfo* type, const char* name,
                     Getter getter, Setter setter)
    : PropertyTypeImpl<T>(type, name, 0), m_setter(setter), m_getter(getter) {
  }

  virtual void set_value(Object* _obj, void* _value) const {
    CLASS* obj = (CLASS*)_obj;
    T* value = (T*)_value;
    (obj->*m_setter)(*value);
  }

  virtual void get_value(Object* _obj, void* _value) const {
    CLASS* obj = (CLASS*)_obj;
    T* value = (T*)_value;
    *value = (obj->*m_getter)();
  }

  Getter m_getter;
  Setter m_setter;
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


#define OBJECT_BIMPL(name, ptype)                               \
  TypeInfo name::Type(#name, name::CreateInstance, ptype);      \
  const TypeInfo* name::typeinfo() {                            \
    return &(name::Type);                                       \
  }

#define OBJECT_IMPL(name, pname)                        \
  Object* name::CreateInstance(void* init) {            \
    return new name(init);                              \
  }                                                     \
  OBJECT_BIMPL(name, &pname::Type)

#define OP(name, member_name) name ## _ ## member_name ## _ ## Property

#define OBJECT_PROPERTY(name, member_name)                              \
  static PropertyTypeImpl<decltype(((name*)0)->member_name)>            \
    OP(name, member_name)(                                              \
      &name::Type,                                                      \
      #member_name,                                                     \
      offsetof(name, member_name));

#define OBJECT_ACCESSOR(name, member_name, getter, setter)           \
  static PropertyMethodImpl<decltype(&name::setter)>                 \
    OP(name, member_name)(                                           \
      &name::Type,                                                   \
      #member_name,                                                  \
      &name::getter,                                                 \
      &name::setter)

class Object {
public:
  OBJECT_PROTO(Object);
};

#endif
