#ifndef GAME_H
#define GAME_H

#include "testlib.h"
#include "particle.h"

struct InputState_;

void game_init();
void game_step(long delta, struct InputState_* state);
void game_shutdown();

// implementation specific stuff

typedef struct GameParticle_ {
  struct Particle_ particle;
  struct Rect_ rect;
} *GameParticle;

typedef struct PrettyParticle_ {
  struct Particle_ particle;
  long end_time;
} *PrettyParticle;

extern float player_speed;
extern float player_bullet_speed;
extern float enemy_speed;
extern GameParticle player;
extern struct DLL_ enemies;
extern struct DLL_ player_bullets;
extern struct DLL_ pretty_particles;

extern ImageResource stars;
extern ImageResource image_enemy;
extern ImageResource image_player_bullet;
extern ImageResource image_smoke;

extern Clock main_clock;

GameParticle gameparticle_make();
void gameparticle_free(GameParticle particle);
void gameparticle_remove(DLL list, GameParticle particle);

GameParticle spawn_enemy();
GameParticle spawn_bullet(Vector pos, Vector vel, ImageResource image);

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
