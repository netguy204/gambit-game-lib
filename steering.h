#ifndef STEERING_H
#define STEERING_H

#include "vector.h"
#include "particle.h"

typedef struct SteeringResult_ {
  struct Vector_ force;
  float angle;
  char computed;
} *SteeringResult;

typedef struct SteeringParams_ {
  float force_max;
  float speed_max;
  float old_angle;
} *SteeringParams;


void steeringresult_complete(SteeringResult result, SteeringParams params);
void particle_applysteering(Particle p, SteeringResult r, SteeringParams params, float dt);
void steering_seek(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params);
void steering_flee(SteeringResult r, Vector tgt, Vector src, Vector src_vel, SteeringParams params);

#endif
