#include "ooc.h"

TypeInfo::TypeInfo(const char* name, CtorFn ctor)
  : m_name(name), m_ctor(ctor) {
  TypeRegistry::instance().register_type(this);
}

void TypeInfo::register_property(PropertyInfo* property) {
  name_to_property.insert(std::make_pair(property->name(), property));
}

PropertyInfo* TypeInfo::property(const char* name) {
  NameToProperty::iterator iter = name_to_property.find(name);
  if(iter == name_to_property.end()) return NULL;
  return iter->second;
}

const char* TypeInfo::name() const {
  return m_name;
}

Object* TypeInfo::makeInstance(void* init) const {
  return m_ctor(init);
}

bool TypeInfo::isInstanceOf(const TypeInfo* other) const {
  // add superclass info
  return this == other;
}

PropertyInfo::PropertyInfo(TypeInfo* type, const char* name, size_t offset)
  : m_type(type), m_name(name), m_offset(offset) {
  m_type->register_property(this);
}

const char* PropertyInfo::name() const {
  return m_name;
}

#define CAST_MEMBER(obj, offset, type) *(type*)((char*)obj + offset)

float PropertyInfo::float_value(Object* obj) {
  return CAST_MEMBER(obj, m_offset, float);
}

void PropertyInfo::set_float_value(Object* obj, float value) {
  CAST_MEMBER(obj, m_offset, float) = value;
}

int PropertyInfo::int_value(Object* obj) {
  return CAST_MEMBER(obj, m_offset, int);
}

void PropertyInfo::set_int_value(Object* obj, int value) {
  CAST_MEMBER(obj, m_offset, int) = value;
}

bool cmp_str::operator()(char const *a, char const *b) {
  return std::strcmp(a, b) < 0;
}

TypeRegistry& TypeRegistry::instance() {
  static TypeRegistry registry;
  return registry;
}

void TypeRegistry::register_type(TypeInfo* type) {
  name_to_type.insert(std::make_pair(type->name(), type));
}

TypeInfo* TypeRegistry::find_type(const char* name) {
  NameToType::iterator iter = name_to_type.find(name);
  if(iter == name_to_type.end()) return NULL;
  return iter->second;
}

OBJECT_BIMPL(Object);

Object* Object::CreateInstance(void* init) {
  return new Object();
}
