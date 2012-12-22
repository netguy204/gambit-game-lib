#ifndef OOC_H
#define OOC_H

#include <stdio.h>

class Object;

typedef Object*(*CtorFn)();

class TypeInfo {
public:
  TypeInfo(const char* name, CtorFn ctor);
  const char* name() const;
  Object* makeInstance() const;
  bool isInstanceOf(const TypeInfo* other) const;

private:
  const char* m_name;
  CtorFn m_ctor;
};

#define OBJECT_PROTO(name)                            \
  virtual const TypeInfo* typeinfo();                 \
  static TypeInfo Type;                               \
  static Object* CreateInstance()


#define OBJECT_IMPL(name)                               \
  TypeInfo name::Type(#name, name::CreateInstance);     \
  Object* name::CreateInstance() {                      \
    return new name();                                  \
  }                                                     \
  const TypeInfo* name::typeinfo() {                    \
    return &(name::Type);                               \
  }

class Object {
public:
  OBJECT_PROTO(Object);
};

#endif
