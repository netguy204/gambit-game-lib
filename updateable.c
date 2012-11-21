#include "updateable.h"

#include <assert.h>
#include <stdarg.h>
void* UpdateableClass = NULL;

// new selector added in AgentClass metaclass
void update(void* _self, float dt) {
  const struct UpdateableClass_* class = classOf(_self);
  assert(class->update);
  class->update(_self, dt);
}

void super_update(const void* _class, void* _self, float dt) {
  const struct UpdateableClass_* superclass = super(_class);
  assert(_self && superclass->update);
  superclass->update(_self, dt);
}

static void* UpdateableClass_ctor(void* _self, va_list * app) {
  struct UpdateableClass_* self = super_ctor(UpdateableClass, _self, app);
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

void updateable_init() {
  if(UpdateableClass) return;

  UpdateableClass = new(Class, "UpdateableClass",
                        Class, sizeof(struct UpdateableClass_),
                        ctor, UpdateableClass_ctor,
                        0);
}
