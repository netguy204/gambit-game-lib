#include "ooc.h"

TypeInfo::TypeInfo(const char* name, CtorFn ctor)
  : m_name(name), m_ctor(ctor) {

}

const char* TypeInfo::name() const {
  return m_name;
}

Object* TypeInfo::makeInstance() const {
  return m_ctor();
}

bool TypeInfo::isInstanceOf(const TypeInfo* other) const {
  // add superclass info
  return this == other;
}

OBJECT_IMPL(Object);
