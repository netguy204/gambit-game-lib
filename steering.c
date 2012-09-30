#include "steering.h"

void steeringresult_complete(SteeringResult result, SteeringParams params) {
  // clamp the force
  vector_clamp(&result->force, &result->force, params->force_max);
}

void particle_applysteering(Particle p, SteeringResult r, SteeringParams params, float dt) {
  struct Vector_ dvdt;
  vector_scale(&dvdt, &r->force, dt);
  vector_add(&p->vel, &p->vel, &dvdt);
  vector_clamp(&p->vel, &p->vel, params->speed_max);

  // no angle change if speed is small
  float mag = vector_mag(&p->vel);
  if(mag < 0.01) {
    return;
  }

  p->angle = vector_angle(&p->vel);
}

void steering_seek(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ desired_vel;
  vector_direction_scaled(&desired_vel, tgt, src, params->speed_max);
  vector_sub(&r->force, &desired_vel, src_vel);
  r->computed = 1;
}

void steering_flee(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ desired_vel;
  vector_direction_scaled(&desired_vel, src, tgt, params->speed_max);
  vector_sub(&r->force, &desired_vel, src_vel);
  r->computed = 1;
}
