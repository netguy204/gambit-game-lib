#ifndef OOC_H
#define OOC_H

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

class PropertyInfo {
 public:
  PropertyInfo(TypeInfo* type, const char* name, size_t offset);

  const char* name() const;

  float float_value(Object* obj);
  void set_float_value(Object* obj, float value);

  int int_value(Object* obj);
  void set_int_value(Object* obj, int value);

 private:
  TypeInfo* m_type;
  const char* m_name;
  size_t m_offset;
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


#define OBJECT_PROPERTY(name, member_name)                              \
  static PropertyInfo name ## _ ## member_name ## _ ## Property(&name::Type, \
                                                                #member_name, \
                                                                offsetof(name, member_name));
class Object {
public:
  OBJECT_PROTO(Object);
};

#endif
