#ifndef UPDATEABLE_H
#define UPDATEABLE_H

#include "ooc.h"

struct UpdateableClass_ {
  const struct Class _;
  void(*update)(void* self, float dt);
};

extern void* UpdateableClass;
void update(void* self, float dt);
void super_update(const void* _class, void* _self, float dt);

void updateable_init();

#endif
