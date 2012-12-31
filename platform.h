#ifndef PLATFORM_H
#define PLATFORM_H

#include "gameobject.h"
#include "rect.h"

class CPlatformer : public Component {
 public:
  OBJECT_PROTO(CPlatformer);

  CPlatformer(void* go);
  virtual ~CPlatformer();

  virtual void init();
  virtual void update(float dt);

  b2Fixture* fixture;
  GO* parent;

  float w;
  float h;
  float density;
  float friction;
};

#endif
