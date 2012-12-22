#ifndef STEERING_H
#define STEERING_H

#include "vector.h"
#include "particle.h"
#include "pathfinder.h"

typedef struct SteeringResult_ {
  struct Vector_ force;
  char computed;
} *SteeringResult;

typedef struct SteeringParams_ {
  float force_max;
  float speed_max;
  float old_angle;
  float application_time;
} *SteeringParams;

typedef struct SteeringObstacle_ {
  struct Vector_ center;
  struct Vector_ perp_offset; // internal
  float radius;
  float cylinder_dist; // used internally
} *SteeringObstacle;

void steeringresult_complete(SteeringResult result, SteeringParams params);
void particle_applysteering(Particle* p, SteeringResult r, SteeringParams params, float dt);
void steering_apply_desired_velocity(SteeringResult r, Vector desired_vel, Vector src_vel);

void steering_seek(SteeringResult r, Vector tgt, Vector src, Vector src_vel,
                   SteeringParams params);
void steering_arrival(SteeringResult r, Vector tgt, Vector src, Vector src_vel,
                      float slowing_dist, SteeringParams params);
void steering_flee(SteeringResult r, Vector tgt, Vector src, Vector src_vel,
                   SteeringParams params);
void steering_pursuit(SteeringResult r, Vector tgt, Vector tgt_vel,
                      Vector src, Vector src_vel, SteeringParams params);
void steering_evasion(SteeringResult r, Vector tgt, Vector tgt_vel,
                      Vector src, Vector src_vel, SteeringParams params);
void steering_offsetpursuit(SteeringResult r, Vector tgt, Vector tgt_vel,
                            Vector src, Vector src_vel, float offset,
                            SteeringParams params);
void steering_offsetarrival(SteeringResult r, Vector tgt, Vector src,
                            Vector src_vel, float offset, float slowing_dist,
                            SteeringParams params);
void steering_avoidance(SteeringResult r, SteeringObstacle objs, int nobjs,
                        Vector src, Vector src_vel, float src_radius, float src_range,
                        SteeringParams params);
int steering_followpath(SteeringResult r, TileMap map, PathInstance path, Vector src, Vector src_vel,
                        float max_offset, SteeringParams params);

#endif
