#ifndef GAME_H
#define GAME_H

#include "testlib.h"
#include "particle.h"
#include "agent.h"
#include "spriteatlas.h"
#include "pathfinder.h"
#include "steering.h"

struct InputState_;

void game_init();
void game_step(long delta, struct InputState_* state);
void game_shutdown();

// implementation specific stuff
typedef struct VisibleAgent_ {
  struct Agent_ _;
  struct Particle_ particle;
} *VisibleAgent;

typedef struct EnemyAgent_ {
  struct VisibleAgent_ _;
  struct PathInstance_ pi;
  struct SteeringResult_ last_result;
  int hp;
} *EnemyAgent;

typedef struct TimedParticle_ {
  struct Particle_ _;
  long end_time;
} *TimedParticle;

extern float player_speed;
extern float player_bullet_speed;
extern float enemy_speed;
extern float enemy_bullet_speed;
extern float enemy_fire_rate;
extern Particle player;
extern struct DLL_ enemies;
extern struct DLL_ player_bullets;
extern struct DLL_ enemy_bullets;
extern struct DLL_ pretty_particles;

extern ImageResource stars;
extern SpriteAtlasEntry image_enemy;
extern SpriteAtlasEntry image_player_bullet;
extern SpriteAtlasEntry image_enemy_bullet;
extern SpriteAtlasEntry image_smoke;

extern Clock main_clock;

TimedParticle spawn_smoke(Vector pos, Vector vel);
Particle spawn_bullet(Vector pos, Vector vel, SpriteAtlasEntry image);
void spawn_enemy_fire(Particle enemy);

typedef struct CollisionRecord_ {
  struct Rect_ rect;
  void * data;
  char skip;
} *CollisionRecord;

typedef void(*OnCollision)(CollisionRecord, CollisionRecord, void*);

void collide_arrays(CollisionRecord as, int na, CollisionRecord bs, int nb,
                    OnCollision on_collision, void * udata);

CollisionRecord particles_collisionrecords(DLL list, int* count, float scale);

#endif
