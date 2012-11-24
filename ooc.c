#include "ooc.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>

const void* classOf(const void* _self) {
  const struct Object* self = _self;
  assert(self && self->class);
  return self->class;
}

const char* className(const void* _self) {
  const struct Class* self = _self;
  return self->name;
}

size_t sizeOf(const void* _self) {
  const struct Class* class = classOf(_self);
  return class->size;
}

const void* super(const void* _self) {
  const struct Class* self = _self;
  assert(self && self->super);
  return self->super;
}

void delete(void* _self) {
  if(_self) {
    const struct Class* class = classOf(_self);
    dealloci(dtor(_self));
  }
}

void* vinit(const void* _class, void* _self, va_list* app) {
  const struct Class* class = _class;
  struct Object* object = _self;

  assert(class && object);
  object->class = class;
  object = ctor(object, app);
  return object;
}

void* new(const void* _class, ...) {
  const struct Class* class = _class;
  struct Object* object;
  va_list ap;

  assert(class);

  object = alloci(class);
  assert(object);

  va_start(ap, _class);
  vinit(class, object, &ap);
  va_end(ap);
  return object;
}

void* init(const void* _class, void* _self, ...) {
  va_list ap;

  va_start(ap, _self);
  vinit(_class, _self, &ap);
  va_end(ap);
  return _self;
}

void* super_ctor(const void* _class, void* _self, va_list* app) {
  const struct Class* superclass = super(_class);
  assert(_self && superclass->ctor);
  return superclass->ctor(_self, app);
}

void* super_dtor(const void* _class, void* _self) {
  const struct Class* superclass = super(_class);
  assert(_self && superclass->dtor);
  return superclass->dtor(_self);
}

// class method

void* alloci(const void* _class) {
  const struct Class* class = _class;
  assert(class->alloci);
  return class->alloci(_class);
}

// selectors

void dealloci(void* _self) {
  const struct Class* class = classOf(_self);
  assert(class->dealloci);
  class->dealloci(_self);
}

void* dtor(void* _self) {
  const struct Class* class = classOf(_self);
  assert(class->dtor);
  return class->dtor(_self);
}

void* ctor(void* _self, va_list* app) {
  const struct Class* class = classOf(_self);
  assert(class->ctor);
  return class->ctor(_self, app);
}

int differ(const void* _self, const void* b) {
  const struct Class* class = classOf(_self);
  assert(class->differ);
  return class->differ(_self, b);
}

int tofile(const void* _self, FILE* fp) {
  const struct Class* class = classOf(_self);
  assert(class->tofile);
  return class->tofile(_self, fp);
}

// base implementations

static void* Object_alloci(const void* _class) {
  const struct Class* class = _class;
  return calloc(1, class->size);
}

static void Object_dealloci(void* _self) {
  free(_self);
}

static void* Object_ctor(void* _self, va_list* app) {
  return _self;
}

static void* Object_dtor(void* _self) {
  return _self;
}

static int Object_tofile(const void* _self, FILE* fp) {
  const struct Class* class = classOf(_self);
  return fprintf(fp, "%s at %p\n", class->name, _self);
}

static int Object_differ(const void* _self, const void* b) {
  return _self != b;
}

static void* Class_alloci(const void* _self) {
  const struct Class* self = _self;
  return calloc(1, self->size);
}

static void Class_dealloci(void* _self) {
  struct Class* self = _self;
  fprintf(stderr, "%s: cannot dealloc class\n", self->name);
}

static void* Class_dtor(void* _self) {
  struct Class* self = _self;
  fprintf(stderr, "%s: cannot destroy class\n", self->name);
  return 0;
}

static void* Class_ctor(void* _self, va_list* app) {
  struct Class* self = _self;
  self->name = strdup(va_arg(*app, char*));
  self->super = va_arg(*app, struct Class*);
  self->size = va_arg(*app, size_t);
  assert(self->super);

  // get the methods of our superclass
  const size_t offset = offsetof(struct Class, alloci);
  memcpy((char*)self + offset, (char*)self->super + offset,
         sizeOf(self->super) - offset);

  va_list ap;
  va_copy(ap, *app);

  while(1) {
    voidf selector = va_arg(ap, voidf);
    if(0 == ((int)selector)) break;

    voidf method = va_arg(ap, voidf);
    if(selector == (voidf)alloci) {
      *(voidf*)&self->alloci = method;
    } else if(selector == (voidf)dealloci) {
      *(voidf*)&self->dealloci = method;
    } else if(selector == (voidf)ctor) {
      *(voidf*)&self->ctor = method;
    } else if(selector == (voidf)dtor) {
      *(voidf*)&self->dtor = method;
    } else if(selector == (voidf)differ) {
      *(voidf*)&self->differ = method;
    } else if(selector == (voidf)tofile) {
      *(voidf*)&self->tofile = method;
    }
  }
  return self;
}


// initialize the braid

static const struct Class object[] = {
  {
    { object + 1},
    "Object", object, sizeof(struct Object),
    Object_alloci, Object_dealloci, Object_ctor, Object_dtor,
    Object_differ, Object_tofile
  },
  {
    { object + 1 },
    "Class", object, sizeof(struct Class),
    Class_alloci, Class_dealloci, Class_ctor, Class_dtor,
    Object_differ, Object_tofile
  }
};

const void* Object = object;
const void* Class = object + 1;
