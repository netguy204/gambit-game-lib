#include "steering.h"

void steeringresult_complete(SteeringResult result, SteeringParams params) {
  float mag = vector_mag(&result->force);
  if(mag < 0.01) {
    result->angle = params->old_angle;
    return;
  }

  // compute the angle and clamp the force
  result->angle = vector_angle(&result->force);
  vector_scale(&result->force, &result->force, params->force_max / mag);
}

void particle_applysteering(Particle p, SteeringResult r, SteeringParams params, float dt) {
  struct Vector_ dvdt;
  vector_scale(&dvdt, &r->force, dt);
  vector_add(&p->vel, &p->vel, &dvdt);
  vector_clamp(&p->vel, &p->vel, params->speed_max);
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
