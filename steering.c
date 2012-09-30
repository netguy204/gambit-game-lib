#include "steering.h"
#include "config.h"

#include <math.h>

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

void steering_arrival(SteeringResult r, Vector tgt, Vector src, Vector src_vel,
                      float slowing_dist, SteeringParams params) {
  struct Vector_ to_target;
  vector_sub(&to_target, tgt, src);

  float mag = vector_mag(&to_target);
  float speed = MIN(params->speed_max, (mag / slowing_dist) * params->speed_max);

  struct Vector_ desired_vel;
  vector_scale(&desired_vel, &to_target, speed / mag);
  vector_sub(&r->force, &desired_vel, src_vel);
  r->computed = 1;
}

void steering_flee(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ desired_vel;
  vector_direction_scaled(&desired_vel, src, tgt, params->speed_max);
  vector_sub(&r->force, &desired_vel, src_vel);
  r->computed = 1;
}

void steering_predict(Vector prediction, Vector tgt, Vector tgt_vel, Vector src,
                      SteeringParams params) {
  // assume the intercept time is the time it would take us to get to
  // where the target is now
  struct Vector_ to_target;
  vector_sub(&to_target, tgt, src);

  float dt = vector_mag(&to_target) / params->speed_max;

  // project the target to that position
  struct Vector_ dx;
  vector_scale(&dx, tgt_vel, dt);
  vector_add(prediction, tgt, &dx);
}

void steering_pursuit(SteeringResult r, Vector tgt, Vector tgt_vel,
                      Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ prediction;
  steering_predict(&prediction, tgt, tgt_vel, src, params);
  steering_seek(r, &prediction, src, src_vel, params);
}

void steering_evasion(SteeringResult r, Vector tgt, Vector tgt_vel,
                      Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ prediction;
  steering_predict(&prediction, tgt, tgt_vel, src, params);
  steering_flee(r, &prediction, src, src_vel, params);
}

void steering_offsetpursuit(SteeringResult r, Vector tgt, Vector tgt_vel,
                            Vector src, Vector src_vel, float offset,
                            SteeringParams params) {
  struct Vector_ prediction;
  steering_predict(&prediction, tgt, tgt_vel, src, params);

  struct Vector_ p2s;
  vector_sub(&p2s, src, &prediction);
  float mag = vector_mag(&p2s);

  vector_scale(&p2s, &p2s, offset / mag);

  struct Vector_ offset_tgt;
  vector_add(&offset_tgt, &prediction, &p2s);

  steering_seek(r, &offset_tgt, src, src_vel, params);
}

void steering_offsetarrival(SteeringResult r, Vector tgt, Vector src,
                            Vector src_vel, float offset, float slowing_dist,
                            SteeringParams params) {
  struct Vector_ to_src;

  vector_sub(&to_src, src, tgt);
  float mag = vector_mag(&to_src);
  vector_scale(&to_src, &to_src, offset / mag);

  struct Vector_ offset_tgt;
  vector_add(&offset_tgt, tgt, &to_src);

  steering_arrival(r, &offset_tgt, src, src_vel, slowing_dist, params);
}
