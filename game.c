#include "game.h"
#include "testlib.h"
#include "vector.h"
#include "listlib.h"
#include "memory.h"
#include "particle.h"
#include "rect.h"
#include "controls.h"
#include "agent.h"
#include "steering.h"
#include "tiles.h"
#include "worldgen.h"
#include "random.h"
#include "updateable.h"

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

float player_speed = 600;
float player_bullet_speed = 1200;
float enemy_speed = 250;
float enemy_bullet_speed = 400;
float enemy_fire_rate = 1;

Particle player;
struct RepeatingLatch_ player_gun_latch;

const void* ParticleObject;
const void* BulletObject;
const void* TimedParticleObject;

const void* EnemyObject;
const void* EnemyCoordinatorObject;
const void* CollisionObject;

Collective collective;
struct DLL_ enemies;
struct DLL_ player_bullets;
struct DLL_ enemy_bullets;
struct DLL_ pretty_particles;
struct Random_ rgen;

SpriteAtlas atlas;
TileMap tiles;
SpriteAtlasEntry image_enemy;
SpriteAtlasEntry image_ally;
SpriteAtlasEntry image_player_bullet;
SpriteAtlasEntry image_enemy_bullet;
SpriteAtlasEntry image_smoke;
ImageResource image_stars;

Clock main_clock;

FixedAllocator particle_allocator;
FixedAllocator prettyparticle_allocator;
FixedAllocator enemy_allocator;

void* ParticleObject_alloci(const void* _class) {
  return fixed_allocator_alloc(particle_allocator);
}

void* ParticleObject_ctor(void* _self, va_list* app) {
  Particle particle = super_ctor(ParticleObject, _self, app);
  particle->containing_list = va_arg(*app, DLL);
  if(particle->containing_list) {
    dll_add_head(particle->containing_list, &particle->node);
  }

  particle->scale = 1.0f;
  particle->angle = 0.0f;
  particle->dsdt = 0.0f;
  particle->dadt = 0.0f;
  return particle;
}

void* ParticleObject_dtor(void* _self) {
  Particle particle = _self;
  if(particle->containing_list) {
    dll_remove(particle->containing_list, &particle->node);
  }
  return super_dtor(ParticleObject, _self);
}

void ParticleObject_dealloci(void* _self) {
  fixed_allocator_free(particle_allocator, _self);
}

void ParticleObject_update(void* _self, float dt) {
  Particle particle = _self;
  particle_integrate(particle, dt);
}

void* TimedParticleObject_alloci(const void* _class) {
  return fixed_allocator_alloc(prettyparticle_allocator);
}

void TimedParticleObject_dealloci(void* _self) {
  fixed_allocator_free(prettyparticle_allocator, _self);
}

static void* EnemyObject_alloci(void* _self) {
  return fixed_allocator_alloc(enemy_allocator);
}

static void EnemyObject_dealloci(const void* _class, void* _self) {
  fixed_allocator_free(enemy_allocator, _self);
}

static void* EnemyObject_ctor(void* _self, va_list* app) {
  EnemyAgent enemyagent = super_ctor(EnemyObject, _self, app);
  init(ParticleObject, &enemyagent->particle, &enemies);
  return enemyagent;
}

static void* EnemyObject_dtor(void* _self) {
  EnemyAgent enemyagent = _self;
  dtor(&enemyagent->particle);
  return super_dtor(EnemyObject, _self);
}

Sprite frame_resource_sprite(ImageResource resource) {
  Sprite sprite = frame_make_sprite();
  sprite->resource = resource;
  sprite->w = resource->w;
  sprite->h = resource->h;
  return sprite;
}

// coordinate joint enemy behavior
void EnemyCoordinatorObject_update(void* _self, float dt) {
  Dispatcher dispatcher = _self;

  // drain and ignore our inbox
  foreach_inboxmessage((Agent)dispatcher, NULL, NULL);

  // drain our dispatchees
  foreach_dispatcheemessage(dispatcher, NULL, NULL);

  // see if we can issue new commands yet
  if(clock_time(main_clock) < ((Agent)dispatcher)->next_timer) return;

  // schedule our next update
  ((Agent)dispatcher)->next_timer = clock_time(main_clock) +
    clock_seconds_to_cycles(10 * enemy_fire_rate);

  // send attack command to n agents, also transition non-idlers
  int n = 2;

  LLNode node = dispatcher->dispatchees;
  Agent subagent;
  while((subagent = llentry_nextvalue(&node))) {
    if(n > 0 && subagent->state == ENEMY_IDLE) {
      Message command = message_make(subagent, AGENT_START_ATTACK, NULL);
      message_postinbox(subagent, command);
      n--;
    }
    else if(subagent->state == ENEMY_ATTACKING) {
      Message command = message_make(subagent, AGENT_FLEE, NULL);
      message_postinbox(subagent, command);
    } else if(subagent->state == ENEMY_FLEEING) {
      Message command = message_make(subagent, AGENT_IDLE, NULL);
      message_postinbox(subagent, command);
    }
  }
}

void enemyagent_process_inbox(Agent agent, Message message, void * udata) {
  Message reply;
  Particle p = &((EnemyAgent)agent)->particle;

  switch(message->kind) {
  case MESSAGE_TERMINATE:
    reply = message_make(agent, MESSAGE_TERMINATING, NULL);
    agent->state = ENEMY_DYING;
    message_postoutbox(agent, reply, agent_terminate_report_complete);
    spawn_smoke(&p->pos, &p->vel);
    break;
  case AGENT_START_ATTACK:
    agent->state = ENEMY_ATTACKING;
    p->image = image_enemy;
    break;
  case AGENT_FLEE:
    agent->state = ENEMY_FLEEING;
    p->image = image_ally;
    break;
  case AGENT_IDLE:
    agent->state = ENEMY_IDLE;
    p->image = image_ally;
    break;
  default:
    printf("enemyagent: unrecognized message %d\n", message->kind);
  }
}

void EnemyObject_update(void* _self, float dt) {
  super_update(EnemyObject, _self, dt);

  Agent agent = _self;
  EnemyAgent enemyagent = _self;

  // drain our inbox
  foreach_inboxmessage(agent, enemyagent_process_inbox, NULL);

  if(agent->state == ENEMY_DYING) return;

  Particle p = &enemyagent->particle;
  struct SteeringParams_ params = { 50.0, enemy_speed, p->angle, 0.08 };
  SteeringResult result = &enemyagent->last_result;

  // then, only make adjustments once every few ticks
  if(clock_time(main_clock) < agent->next_timer) {
    if(result->computed) {
      particle_applysteering(p, result, &params, dt);
    }
    return;
  }
  agent->next_timer = clock_time(main_clock) +
    clock_seconds_to_cycles(params.application_time);

  // zero the result
  memset(result, 0, sizeof(struct SteeringResult_));

  // idle mode agents avoid the player
  if(agent->state == ENEMY_IDLE) {
    struct SteeringObstacle_ pobjs;

    pobjs.center = player->pos;
    pobjs.radius = particle_width(player) * 0.5;

    steering_avoidance(result, &pobjs, 1, &p->pos, &p->vel,
                       particle_width(p) * 0.4, 200, &params);

    if(result->computed) {
      steeringresult_complete(result, &params);
      particle_applysteering(p, result, &params, dt);
      return;
    }
  }

  struct Vector_ path_end;
  vector_pathinstance_end(&path_end, &enemyagent->pi, tiles);

  // move along the path, bounce at the end
  float end_dist = vector_dist(&p->pos, &path_end);
  if(end_dist < 32) {
    enemyagent->pi.pathdir = -enemyagent->pi.pathdir;
  }

  switch(agent->state) {
  case ENEMY_IDLE:
    // if we're not attacking... then follow our path
    enemyagent->pi.pathpos =
      steering_followpath(result, tiles, &enemyagent->pi, &p->pos, &p->vel, 32, &params);
    if(!result->computed) {
      // no steering force needed to get back to the path, so we'll
      // just fly along it
      struct Vector_ desired_vel;
      vector_pathinstance_direction(&desired_vel, tiles, &enemyagent->pi);
      vector_scale(&desired_vel, &desired_vel, enemy_speed);
      steering_apply_desired_velocity(result, &desired_vel, &p->vel);
    }
    break;

  case ENEMY_ATTACKING:
    steering_offsetarrival(result, &player->pos, &p->pos, &p->vel,
                           2.0f * particle_width(player), 3.0f * particle_width(player),
                           &params);
    /*
    // use our bullet speed as the predictor
    params.speed_max = enemy_bullet_speed;
    steering_offsetpursuit(&result, &player->pos, &player->vel, &p->pos, &p->vel,
                           2.0f * particle_width(player), &params);
    params.speed_max = enemy_speed;
    */
    break;

  case ENEMY_FLEEING:
    steering_flee(result, &player->pos, &p->pos, &p->vel, &params);
    break;

  default:
    printf("enemyagent_update unknown state: %d\n", agent->state);
  }

  if(result->computed) {
    steeringresult_complete(result, &params);
    particle_applysteering(p, result, &params, dt);
  }

  /*
  if(clock_time(main_clock) < agent->next_timer) return;
  agent->next_timer = clock_time(main_clock) +
    clock_seconds_to_cycles(enemy_fire_rate);

  if(agent->state == ENEMY_ATTACKING) {
    spawn_enemy_fire(p);
  }
  */
}

EnemyAgent spawn_enemy() {
  EnemyAgent enemyagent = new(EnemyObject, ENEMY_IDLE);

  enemyagent->particle.image = image_ally;
  enemyagent->particle.scale = 1.0f;

  // pick a path
  int pathidx = rand_in_range(&rgen, 0, civpaths->npaths);
  Path path = &civpaths->paths[pathidx];
  enemyagent->pi.path = path;
  enemyagent->pi.pathpos = 0;
  enemyagent->pi.pathdir = 1;
  enemyagent->pi.max_skip_range = 3;
  enemyagent->hp = 100;

  // start at its beginning
  vector_tilecenter(&enemyagent->particle.pos, tiles, path->steps[enemyagent->pi.pathpos]);

  // velocity aligned with the path
  vector_path_direction(&enemyagent->particle.vel, tiles, path,
                        enemyagent->pi.pathpos, enemyagent->pi.pathdir);
  vector_scale(&enemyagent->particle.vel, &enemyagent->particle.vel, enemy_speed);

  // not scaling or spinning, let the steering method figure out the
  // right orientation
  enemyagent->particle.angle = M_PI;
  enemyagent->particle.dsdt = 0.0f;
  enemyagent->particle.dadt = 0.0f;

  return enemyagent;
}

Particle bullet_make(Vector pos, Vector vel, SpriteAtlasEntry image, DLL list) {
  Particle bullet = new(BulletObject, list);
  bullet->image = image;
  bullet->pos = *pos;
  bullet->vel = *vel;
  bullet->scale = 0.25f;
  bullet->dsdt = 0.5;
  bullet->angle = IN_RADIANS(rand_in_range(&rgen, 0, 360));
  bullet->dadt = IN_RADIANS(500);
  return bullet;
}

TimedParticle spawn_smoke(Vector pos, Vector vel) {
  TimedParticle smoke = new(TimedParticleObject, &pretty_particles);
  Particle particle = (Particle)smoke;

  particle->image = image_smoke;
  particle->pos = *pos;
  particle->vel = *vel;
  particle->angle = IN_RADIANS(rand_in_range(&rgen, 0, 360));
  particle->dsdt = 0.5f * rand_in_range(&rgen, 1, 4);
  particle->dadt = IN_RADIANS(rand_in_range(&rgen, -20, 20));
  smoke->end_time =
    clock_time(main_clock) +
    clock_seconds_to_cycles(rand_in_range(&rgen, 500, 3500) / 1000.0f);

  return smoke;
}

void particles_update(DLL list, float dt) {
  DLLNode node = list->head;

  while(node) {
    DLLNode next = node->next;
    Particle p = node_to_particle(node);

    update(p, dt);

    node = next;
  }
}

void enemies_update(float dt) {
  particles_update(&enemies, dt);

  if(dll_count(&enemies) < 30) {
    EnemyAgent enemyagent = spawn_enemy();
    Message spawn = message_make(NULL, COLLECTIVE_ADD_AGENT, enemyagent);
    message_postinbox((Agent)collective, spawn);
  }
}

int staying_onscreen_test(Particle p) {
  float hw = particle_width(p) / 2.0f;
  float hh = particle_height(p) / 2.0f;
  struct Rect_ scr;
  screen_rect(&scr);

  if(p->vel.x > 0) {
    if(p->pos.x > scr.maxx + hw) return 0;
  } else {
    if(p->pos.x < scr.minx - hw) return 0;
  }

  if(p->vel.y > 0) {
    if(p->pos.y > scr.maxy + hh) return 0;
  } else {
    if(p->pos.y < scr.miny - hh) return 0;
  }

  return 1;
}

void BulletObject_update(void* _self, float dt) {
  super_update(BulletObject, _self, dt);

  Particle particle = _self;
  if(!staying_onscreen_test(particle)) {
    delete(particle);
  }
}

void TimedParticleObject_update(void* _self, float dt) {
  super_update(TimedParticleObject, _self, dt);

  TimedParticle particle = _self;
  if(clock_time(main_clock) >= particle->end_time) {
    delete(particle);
  }
}

void player_bullets_update(float dt) {
  particles_update(&player_bullets, dt);
}

void enemy_bullets_update(float dt) {
  particles_update(&enemy_bullets, dt);
}

void prettyparticles_update(float dt) {
  particles_update(&pretty_particles, dt);
}

void collide_arrays(CollisionRecord as, int na, CollisionRecord bs, int nb,
                    OnCollision on_collision, void * udata) {
  int ia;
  int ib;

  for(ia = 0; ia < na; ++ia) {
    CollisionRecord a = &as[ia];
    if(a->skip) continue;

    for(ib = 0; ib < nb; ++ib) {
      CollisionRecord b = &bs[ib];
      if(a->skip || b->skip) break;

      if(rect_intersect(&a->rect, &b->rect)) {
        on_collision(a, b, udata);
      }
    }
  }
}

CollisionRecord particles_collisionrecords(DLL list, int* count, float scale) {
  *count = dll_count(list);
  CollisionRecord crs = frame_alloc(sizeof(struct CollisionRecord_) *
                                    *count);
  DLLNode node = list->head;
  int ii = 0;
  while(node) {
    Particle p = node_to_particle(node);
    rect_for_particle(&(crs[ii].rect), p, scale);
    crs[ii].data = node;
    crs[ii].skip = 0;
    node = node->next;
    ++ii;
  }

  return crs;
}

CollisionRecord enemies_collisionrecords(Dispatcher dispatcher, int* count, float scale) {
  *count = ll_count(dispatcher->dispatchees);
  CollisionRecord crs = frame_alloc(sizeof(struct CollisionRecord_) * (*count));
  LLNode node = dispatcher->dispatchees;
  EnemyAgent enemyagent;
  int ii = 0;
  while((enemyagent = llentry_nextvalue(&node))) {
    Particle ep = &enemyagent->particle;

    rect_for_particle(&(crs[ii].rect), ep, scale);
    crs[ii].data = enemyagent;
    crs[ii].skip = 0;
    ++ii;
  }

  return crs;
}

void bullet_vs_agent(CollisionRecord bullet, CollisionRecord enemy, void* dispatcher_) {
  Dispatcher dispatcher = (Dispatcher)dispatcher_;
  delete(node_to_particle(bullet->data));

  Agent enemyagent = enemy->data;
  agent_send_terminate(enemyagent, (Agent)dispatcher);

  bullet->skip = 1;
  enemy->skip = 1;
}

// -1 is false
int particle_vs_map(Particle particle) {
  float hw = particle_width(particle) / 2.0f;
  float hh = particle_height(particle) / 2.0f;
  int max = tilemap_size(tiles);

  struct Vector_ corners[] = {
    { particle->pos.x - hw,
      particle->pos.y - hh },
    { particle->pos.x + hw,
      particle->pos.y - hh },
    { particle->pos.x + hw,
      particle->pos.y + hh },
    { particle->pos.x - hw,
      particle->pos.y + hh }
  };

  int ii;
  for(ii = 0; ii < array_size(corners); ++ii) {
    int index = tilemap_index_vector(tiles, &corners[ii]);
    if(index >= max) continue;

    int kind = tiles->tiles[index];

    if(tiles->tile_specs[kind].bitmask & TILESPEC_COLLIDABLE) {
      return index;
    }
  }
  return -1;
}

void bullet_vs_map(CollisionRecord bullet, DLL list) {
  Particle pbullet = node_to_particle(bullet->data);

  int index = particle_vs_map(pbullet);
  if(index == -1) return;

  delete(pbullet);
  bullet->skip = 1;
  tiles->tiles[index] = 0;
}

void CollisionObject_update(void* _self, float dt) {
  Dispatcher dispatcher = _self;

  // drain our inbox
  foreach_inboxmessage((Agent)dispatcher, NULL, NULL);

  // drain the outboxes of our dispatchees
  foreach_dispatcheemessage(dispatcher, NULL, NULL);

  int num_pbullets;
  int num_ebullets;
  int num_enemies;

  CollisionRecord pbs =
    particles_collisionrecords(&player_bullets, &num_pbullets, 0.7f);
  CollisionRecord ebs =
    particles_collisionrecords(&enemy_bullets, &num_ebullets, 0.5f);
  CollisionRecord es =
    enemies_collisionrecords(dispatcher, &num_enemies, 0.8f);

  collide_arrays(pbs, num_pbullets, es, num_enemies, &bullet_vs_agent, dispatcher);

  int ii;
  for(ii = 0; ii < num_pbullets; ++ii) {
    CollisionRecord rec = &pbs[ii];
    if(rec->skip) continue;

    bullet_vs_map(rec, &player_bullets);
  }

  for(ii = 0; ii < num_ebullets; ++ii) {
    CollisionRecord rec = &ebs[ii];
    if(rec->skip) continue;

    bullet_vs_map(rec, &enemy_bullets);
  }
}

void sprite_submit(Sprite sprite) {
  if(!sprite) return;

  SpriteList sl = frame_spritelist_append(NULL, sprite);
  spritelist_enqueue_for_screen(sl);
}

void game_init() {
  agent_init();

  particle_allocator =
    fixed_allocator_make(sizeof(struct Particle_),
                         MAX_NUM_PARTICLES,
                         "particle_allocator");

  prettyparticle_allocator =
    fixed_allocator_make(sizeof(struct TimedParticle_),
                         MAX_NUM_PRETTYPARTICLES,
                         "prettyparticle_allocator");

  enemy_allocator =
    fixed_allocator_make(sizeof(struct EnemyAgent_),
                         MAX_NUM_PARTICLES,
                         "enemy_allocator");

  main_clock = clock_make();

  atlas = spriteatlas_load("resources/images_default.dat", "resources/images_default.png");
  image_enemy = spriteatlas_find(atlas, "ship-right.png");
  image_ally = spriteatlas_find(atlas, "ship-right-good.png");
  image_player_bullet = spriteatlas_find(atlas, "plasma.png");
  image_enemy_bullet = spriteatlas_find(atlas, "enemy-bullet.png");
  image_smoke = spriteatlas_find(atlas, "smoke.png");

  ParticleObject = new(UpdateableClass, "Particle",
                       Object, sizeof(struct Particle_),
                       alloci, ParticleObject_alloci,
                       dealloci, ParticleObject_dealloci,
                       ctor, ParticleObject_ctor,
                       dtor, ParticleObject_dtor,
                       update, ParticleObject_update,
                       0);

  BulletObject = new(UpdateableClass, "Bullet",
                     ParticleObject, sizeof(struct Particle_),
                     update, BulletObject_update,
                     0);

  TimedParticleObject = new(UpdateableClass, "TimedParticle",
                            ParticleObject, sizeof(struct TimedParticle_),
                            alloci, TimedParticleObject_alloci,
                            dealloci, TimedParticleObject_dealloci,
                            update, TimedParticleObject_update,
                            0);

  EnemyObject = new(UpdateableClass, "Enemy",
                    AgentObject, sizeof(struct EnemyAgent_),
                    alloci, EnemyObject_alloci,
                    dealloci, EnemyObject_dealloci,
                    ctor, EnemyObject_ctor,
                    dtor, EnemyObject_dtor,
                    update, EnemyObject_update,
                    0);

  EnemyCoordinatorObject = new(UpdateableClass, "EnemyCoordinator",
                               DispatcherObject, sizeof(struct Dispatcher_),
                               update, EnemyCoordinatorObject_update,
                               0);

  CollisionObject = new(UpdateableClass, "Collision",
                        DispatcherObject, sizeof(struct Dispatcher_),
                        update, CollisionObject_update,
                        0);

  dll_zero(&enemies);
  dll_zero(&player_bullets);
  dll_zero(&enemy_bullets);
  dll_zero(&pretty_particles);

  // worldgen
  random_init(&rgen, 1234);
  image_stars = image_load("resources/night-sky-stars.jpg");
  tiles = tilemap_testmake(atlas);

  player = new(ParticleObject, NULL);
  player->image = spriteatlas_find(atlas, "hero.png");
  player->pos.x = 500 * 64;
  player->pos.y = 100 * 64;
  player->vel.x = 0;
  player->vel.y = 0;

  player_gun_latch.period = 0.2;
  player_gun_latch.latch_value = 0;
  player_gun_latch.last_time = 0;
  player_gun_latch.last_state = 0;

  collective = new(CollectiveObject, COLLECTIVE_IDLE,
                   new(EnemyCoordinatorObject, DISPATCHER_IDLE),
                   new(CollisionObject, DISPATCHER_IDLE));
}

void spawn_player_fire() {
  struct Vector_ v = { player_bullet_speed, 0.0f };
  Particle bullet = bullet_make(&player->pos, &v,
                                image_player_bullet,
                                &player_bullets);
}

void spawn_enemy_fire(Particle enemy) {
  struct Vector_ v;
  vector_norm(&v, &enemy->vel);
  vector_scale(&v, &v, enemy_bullet_speed);

  Particle bullet = bullet_make(&enemy->pos, &v,
                                image_enemy_bullet,
                                &enemy_bullets);
}

void handle_input(InputState state) {
  player->vel.x = state->leftright * player_speed;
  player->vel.y = state->updown * player_speed;
  if(repeatinglatch_state(&player_gun_latch, main_clock, state->action1)) {
    spawn_player_fire();
  }
}

long step_number = 0;

extern StackAllocator frame_allocator;

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);
  ++step_number;

  // center the screen on the player
  screen_x_br = player->pos.x - screen_width / 2;
  screen_y_br = player->pos.y - screen_height / 2;

  // draw stars
  Sprite background = frame_resource_sprite(image_stars);
  background->displayX = 0;
  background->displayY = 0;
  background->w = screen_width;
  background->h = screen_height;
  sprite_submit(background);

  // draw the tile background
  spritelist_enqueue_for_screen(tilemap_spritelist(tiles, screen_x_br, screen_y_br,
                                                   screen_width, screen_height));

  // read player input
  handle_input(state);

  // update player position
  particle_integrate((Particle)player, dt);

  // update enemies
  enemies_update(dt);

  // update bullets
  player_bullets_update(dt);
  enemy_bullets_update(dt);

  // update particles
  prettyparticles_update(dt);

  // AI and collision detection
  update((Agent)collective, dt);

  // draw the particles
  spritelist_enqueue_for_screen(particles_spritelist(&pretty_particles));

  // draw the enemies
  spritelist_enqueue_for_screen(particles_spritelist(&enemies));

  // draw the bullets
  spritelist_enqueue_for_screen(particles_spritelist(&player_bullets));
  spritelist_enqueue_for_screen(particles_spritelist(&enemy_bullets));

  // draw the player
  sprite_submit(particle_sprite((Particle)player));

  /*
  if(step_number % 100 == 0) {
    printf("player: %f, %f ", player->pos.x, player->pos.y);
    printf("particle_allocator: %ld (%ld) ; stack_allocator: (%ld of %ld)\n",
           enemy_allocator->inflight,
           enemy_allocator->max_inflight,
           frame_allocator->max_alloced,
           (char*)frame_allocator->stack_max - (char*)frame_allocator->stack_bottom);
  }
  */
}

void game_shutdown() {

}
