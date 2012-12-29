#include "ooc.h"

TypeInfo::TypeInfo(const char* name, CtorFn ctor, TypeInfo* parent)
  : m_name(name), m_ctor(ctor), m_parent(parent) {
  TypeRegistry::instance().register_type(this);
}

void TypeInfo::register_property(PropertyInfo* property) {
  name_to_property.insert(std::make_pair(property->name(), property));
}

const PropertyInfo* TypeInfo::property(const char* name) const {
  NameToProperty::const_iterator iter = name_to_property.find(name);
  if(iter == name_to_property.end()) {
    if(m_parent) return m_parent->property(name);
    return NULL;
  }
  return iter->second;
}

const char* TypeInfo::name() const {
  return m_name;
}

const TypeInfo* TypeInfo::parent() const {
  return m_parent;
}

Object* TypeInfo::makeInstance(void* init) const {
  return m_ctor(init);
}

bool TypeInfo::isInstanceOf(const TypeInfo* other) const {
  // add superclass info
  return this == other;
}

PropertyInfo::PropertyInfo(TypeInfo* type, PropertyType* ptype, const char* name, size_t offset)
  : m_type(type), m_propertyType(ptype), m_name(name), m_offset(offset) {
  m_type->register_property(this);
}

const char* PropertyInfo::name() const {
  return m_name;
}

void PropertyInfo::set_value(Object* obj, void* value) const {
  m_propertyType->set_value(this, obj, value);
}

void PropertyInfo::get_value(Object* obj, void* value) const {
  m_propertyType->get_value(this, obj, value);
}

void PropertyInfo::LCpush_value(Object* obj, lua_State* L) const {
  m_propertyType->LCpush_value(this, obj, L);
}

void PropertyInfo::LCset_value(Object* obj, lua_State* L, int pos) const {
  m_propertyType->LCset_value(this, obj, L, pos);
}

template<>
void PropertyTypeImpl<int>::LCpush_value(const PropertyInfo* info, Object* obj, lua_State* L) {
  int val;
  get_value(info, obj, &val);
  lua_pushinteger(L, val);
}

template<>
void PropertyTypeImpl<int>::LCset_value(const PropertyInfo* info, Object* obj, lua_State* L, int pos) {
  int val = luaL_checkinteger(L, pos);
  set_value(info, obj, &val);
}

template<>
void PropertyTypeImpl<float>::LCpush_value(const PropertyInfo* info, Object* obj, lua_State* L) {
  float val;
  get_value(info, obj, &val);
  lua_pushnumber(L, val);
}

template<>
void PropertyTypeImpl<float>::LCset_value(const PropertyInfo* info, Object* obj, lua_State* L, int pos) {
  float val = luaL_checknumber(L, pos);
  set_value(info, obj, &val);
}

bool cmp_str::operator()(char const *a, char const *b) const {
  return strcmp(a, b) < 0;
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

OBJECT_BIMPL(Object, NULL);

Object* Object::CreateInstance(void* init) {
  return new Object();
}
