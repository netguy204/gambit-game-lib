#ifndef PLATFORM_H
#define PLATFORM_H

#include "gameobject.h"
#include "rect.h"

class CPlatformer : public Component {
 public:
  OBJECT_PROTO(CPlatformer);

  CPlatformer();
  CPlatformer(GO* go, float grav_accel);

  virtual void update(float dt);
  void look_for_support();
  void resolve_interpenetration();
  int is_supported();

  float grav_accel;
  float max_speed;
  int platform_mask;
};

int is_supported(Rect supportee, Rect supporter);
void resolve_interpenetration(Vector resolution, Rect minor, Rect major);

#endif
