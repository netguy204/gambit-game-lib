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

void steering_apply_desired_velocity(SteeringResult r, Vector desired_vel, Vector src_vel) {
  vector_sub(&r->force, desired_vel, src_vel);
  r->computed = 1;
}

void steering_seek(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ desired_vel;
  vector_direction_scaled(&desired_vel, tgt, src, params->speed_max);
  steering_apply_desired_velocity(r, &desired_vel, src_vel);
}

void steering_arrival(SteeringResult r, Vector tgt, Vector src, Vector src_vel,
                      float slowing_dist, SteeringParams params) {
  struct Vector_ to_target;
  vector_sub(&to_target, tgt, src);

  float mag = vector_mag(&to_target);
  if(fabsf(mag) < 0.0001) return;

  float speed = MIN(params->speed_max, (mag / slowing_dist) * params->speed_max);

  struct Vector_ desired_vel;
  vector_scale(&desired_vel, &to_target, speed / mag);
  steering_apply_desired_velocity(r, &desired_vel, src_vel);
}

void steering_flee(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params) {
  struct Vector_ desired_vel;
  vector_direction_scaled(&desired_vel, src, tgt, params->speed_max);
  steering_apply_desired_velocity(r, &desired_vel, src_vel);
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
  if(fabsf(mag) > 0.0001f) {
    vector_scale(&to_src, &to_src, offset / mag);
  } else {
    // pick an arbitrary departure direction
    to_src.x = 0.0f;
    to_src.y = 1.0f;
  }

  struct Vector_ offset_tgt;
  vector_add(&offset_tgt, tgt, &to_src);

  steering_arrival(r, &offset_tgt, src, src_vel, slowing_dist, params);
}

int steering_followpath(SteeringResult r, TileMap map, PathInstance pi, Vector src, Vector src_vel,
                        float max_offset, SteeringParams params) {
  float speed = vector_mag(src_vel);
  struct Vector_ projobj;

  if(speed < 0.1f) {
    // too slow to project to a different future point than we're at
    // now
    projobj = *src;
  } else {
    struct Vector_ src_vel_norm;
    vector_scale(&src_vel_norm, src_vel, 1.0f / speed);
    vector_add(&projobj, src, &src_vel_norm);
  }

  // find the closest point on the path
  struct Vector_ tgt;
  float dist;
  int step = path_next_closest_point(&tgt, map, pi, &projobj, &dist);

  // close enough?
  if(dist <= max_offset) {
    return step;
  }

  // steer towards the point
  steering_seek(r, &tgt, src, src_vel, params);
  return step;
}

void steering_avoidance(SteeringResult r, SteeringObstacle objs, int nobjs,
                        Vector src, Vector src_vel, float src_radius, float src_range,
                        SteeringParams params) {
  if(vector_mag(src_vel) < 0.01) {
    r->computed = 0;
    return; // don't do anything
  }

  struct Vector_ src_vel_norm;
  vector_norm(&src_vel_norm, src_vel);

  int ii;
  SteeringObstacle closest = NULL;
  for(ii = 0; ii < nobjs; ++ii) {
    // put the object in our frame
    SteeringObstacle obj = &objs[ii];

    struct Vector_ objpos;
    vector_sub(&objpos, &obj->center, src);

    // project along our direction of travel. bail if behind us
    struct Vector_ objproj;
    if(vector_project2(&objproj, &objpos, &src_vel_norm) < 0) continue;

    // is that in range?
    float projdist = vector_mag(&objproj);
    if(projdist > src_range) continue;

    // does the cylinder intersect it?
    struct Vector_ perp_offset;
    vector_sub(&perp_offset, &objproj, &objpos);
    if(vector_mag(&perp_offset) > (src_radius + obj->radius)) continue;

    // this is an intersection. store the projection distance
    obj->cylinder_dist = projdist;
    obj->perp_offset = perp_offset;

    if(closest == NULL || obj->cylinder_dist < closest->cylinder_dist) {
      closest = obj;
    }
  }

  if(!closest) {
    // we're done
    r->computed = 0;
    return;
  }

  // steer away from closest collision
  struct Vector_ goal_vel;
  float pmag = vector_mag(&closest->perp_offset);
  if(pmag < 0.01) {
    // obstacle is dead-ahead, just turn
    closest->perp_offset.x = src_vel->y;
    closest->perp_offset.y = -src_vel->x;
    pmag = vector_mag(&closest->perp_offset);
  }
  vector_scale(&goal_vel, &closest->perp_offset, params->speed_max / pmag);
  vector_sub(&r->force, &goal_vel, src_vel);
  r->computed = 1;
}
