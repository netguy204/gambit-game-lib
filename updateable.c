#include "updateable.h"

#include <assert.h>
#include <stdarg.h>

// new selector added in AgentClass metaclass
void update(void* _self, float dt) {
  const struct UpdateableClass_* class = classOf(_self);
  assert(class->update);
  class->update(_self, dt);
}

void super_update(const void* _class, void* _self, float dt) {
  const struct UpdateableClass_* superclass = super(_class);
  assert(superclass && superclass->update);
  superclass->update(_self, dt);
}

static void* UpdateableClass_ctor(void* _self, va_list * app) {
  struct UpdateableClass_* self = super_ctor(UpdateableClass(), _self, app);
  va_list ap;
  va_copy(ap, *app);

  while(1) {
    voidf selector = va_arg(ap, voidf);
    if(!((int)selector)) break;

    voidf method = va_arg(ap, voidf);
    if(selector == (voidf)update) {
      *(voidf*)&self->update = method;
    }
  }

  return self;
}

const void* UpdateableClass() {
  static void* class = NULL;
  if(class) return class;

  class = new(Class, "UpdateableClass",
              Class, sizeof(struct UpdateableClass_),
              ctor, UpdateableClass_ctor,
              0);
  return class;
}
