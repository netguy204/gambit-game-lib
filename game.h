#ifndef GAME_H
#define GAME_H

#include "testlib.h"
#include "particle.h"
#include "agent.h"
#include "spriteatlas.h"
#include "pathfinder.h"

struct InputState_;

void game_init();
void game_step(long delta, struct InputState_* state);
void game_shutdown();

// implementation specific stuff
typedef struct EnemyAgent_ {
  struct Agent_ agent;
  Path path;
  int path_dir;
  int hp;
} *EnemyAgent;

typedef struct Enemy_ {
  struct Particle_ particle;
  struct EnemyAgent_ agent;
} *Enemy;

typedef struct PrettyParticle_ {
  struct Particle_ particle;
  long end_time;
} *PrettyParticle;

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

Particle particle_make();
void particle_free(Particle particle);
void particle_remove(DLL list, Particle particle);

Particle enemyagent_particle(EnemyAgent enemyagent);
Enemy spawn_enemy();
PrettyParticle spawn_smoke(Vector pos, Vector vel);
void enemy_free(Enemy enemy);
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
