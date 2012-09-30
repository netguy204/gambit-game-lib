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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float player_speed = 600;
float player_bullet_speed = 1200;
float enemy_speed = 50;
float enemy_bullet_speed = 400;
float enemy_fire_rate = 1;

Particle player;
struct RepeatingLatch_ player_gun_latch;

Collective collective;
struct DLL_ enemies;
struct DLL_ player_bullets;
struct DLL_ enemy_bullets;
struct DLL_ pretty_particles;

ImageResource stars;
ImageResource image_enemy;
ImageResource image_player_bullet;
ImageResource image_enemy_bullet;
ImageResource image_smoke;

Clock main_clock;

FixedAllocator particle_allocator;
FixedAllocator prettyparticle_allocator;
FixedAllocator enemy_allocator;

Particle particle_make() {
  Particle particle = fixed_allocator_alloc(particle_allocator);
  particle->scale = 1.0f;
  particle->angle = 0.0f;
  particle->dsdt = 0.0f;
  particle->dadt = 0.0f;
  return particle;
}

void particle_free(Particle particle) {
  fixed_allocator_free(particle_allocator, particle);
}

void particle_remove(DLL list, Particle particle) {
  dll_remove(list, (DLLNode)particle);
  particle_free(particle);
}

PrettyParticle prettyparticle_make() {
  PrettyParticle pp = fixed_allocator_alloc(prettyparticle_allocator);
  Particle p = (Particle)pp;
  p->scale = 1.0f;
  p->angle = 0.0f;
  p->dsdt = 0.0f;
  p->dadt = 0.0f;
  return pp;
}

void prettyparticle_free(PrettyParticle particle) {
  fixed_allocator_free(prettyparticle_allocator, particle);
}

void prettyparticle_remove(DLL list, PrettyParticle particle) {
  dll_remove(list, (DLLNode)particle);
  prettyparticle_free(particle);
}

Enemy enemy_make() {
  return fixed_allocator_alloc(enemy_allocator);
}

void enemy_free(Enemy enemy) {
  fixed_allocator_free(enemy_allocator, enemy);
}

int rand_in_range(int lower, int upper) {
  int range = upper - lower;
  return lower + (rand() % range);
}

Sprite frame_resource_sprite(ImageResource resource) {
  Sprite sprite = frame_make_sprite();
  sprite->resource = resource;
  sprite->w = resource->w;
  sprite->h = resource->h;
  return sprite;
}

Particle enemyagent_particle(EnemyAgent enemyagent) {
  Enemy enemy = container_of(enemyagent, struct Enemy_, agent);
  return &enemy->particle;
}

void enemyagent_free(Agent agent) {
  EnemyAgent enemyagent = (EnemyAgent)agent;
  Enemy enemy = container_of(enemyagent, struct Enemy_, agent);
  dll_remove(&enemies, (DLLNode)&enemy->particle);
  enemy_free(enemy);
}

// coordinate joint enemy behavior
void coordinator_update(Agent agent, float dt) {
  Dispatcher dispatcher = (Dispatcher)agent;

  // drain and ignore our inbox
  foreach_inboxmessage(agent, NULL, NULL);

  // drain our dispatchees
  foreach_dispatcheemessage(dispatcher, NULL, NULL);

  // see if we can issue new commands yet
  if(clock_time(main_clock) < agent->next_timer) return;

  // schedule our next update
  agent->next_timer = clock_time(main_clock) +
    clock_seconds_to_cycles(10 * enemy_fire_rate);

  // send attack command to n agents, also transition non-idlers
  int n = 3;

  // feat roll... can we do it?
  /*
  if(rand_in_range(0, 10) > 3) {
    n = 0;
  }
  */

  Dispatchee entry = (Dispatchee)dispatcher->dispatchees.head;
  while(entry) {
    if(n >= 0 && entry->agent->state == ENEMY_IDLE) {
      Message command = message_make(agent, AGENT_START_ATTACK, NULL);
      message_postinbox(entry->agent, command);
      n--;
    } else if(entry->agent->state == ENEMY_ATTACKING) {
      Message command = message_make(agent, AGENT_FLEE, NULL);
      message_postinbox(entry->agent, command);
    } else if(entry->agent->state == ENEMY_FLEEING) {
      Message command = message_make(agent, AGENT_IDLE, NULL);
      message_postinbox(entry->agent, command);
    }
    entry = (Dispatchee)entry->node.next;
  }
}

void enemyagent_process_inbox(Agent agent, Message message, void * udata) {
  Message reply;
  Particle p = enemyagent_particle((EnemyAgent)agent);

  switch(message->kind) {
  case MESSAGE_TERMINATE:
    reply = message_make(agent, MESSAGE_TERMINATING, NULL);
    agent->state = ENEMY_DYING;
    message_postoutbox(agent, reply, agent_terminate_report_complete);
    PrettyParticle smoke = spawn_smoke(&p->pos, &p->vel);
    dll_add_head(&pretty_particles, (DLLNode)smoke);
    break;
  case AGENT_START_ATTACK:
    agent->state = ENEMY_ATTACKING;
    break;
  case AGENT_FLEE:
    agent->state = ENEMY_FLEEING;
    break;
  case AGENT_IDLE:
    agent->state = ENEMY_IDLE;
    break;
  default:
    printf("enemyagent: unrecognized message %d\n", message->kind);
  }
}

void vector_tween(Vector dst, Vector a, Vector b, float deltamax) {
  struct Vector_ ab;
  if(vector_direction_scaled(&ab, b, a, deltamax)) {
    vector_add(dst, dst, &ab);
  }
}

void enemyagent_update(Agent agent, float dt) {
  EnemyAgent enemyagent = (EnemyAgent)agent;

  // drain our inbox
  foreach_inboxmessage(agent, enemyagent_process_inbox, NULL);

  if(agent->state == ENEMY_DYING) return;

  Particle p = enemyagent_particle(enemyagent);

  // nudge ourselves away from our neighbors
  Particle np = (Particle)enemies.head;
  while(np) {
    if(np != p) {
      struct Vector_ away;
      vector_sub(&away, &p->pos, &np->pos);
      float mag = vector_mag(&away);
      if(mag < particle_width(p) * 0.8) {
        vector_scale(&away, &away, (enemy_speed * 0.1) / mag);
        vector_add(&p->vel, &p->vel, &away);
      }
    }
    np = (Particle)np->node.next;
  }

  // we are attacking or fleeing
  struct Vector_ idle_target = {-enemy_speed, 0};
  struct SteeringResult_ result = { {0, 0}, 0 };
  struct SteeringParams_ params = { 100.0, enemy_speed, p->angle };

  switch(agent->state) {
  case ENEMY_IDLE:
    // if we're not attacking... then turn to horizontal. also prevent
    // backtracking
    vector_tween(&p->vel, &p->vel, &idle_target, 0.5);
    p->angle = vector_angle(&p->vel);
    break;

  case ENEMY_ATTACKING:
    steering_seek(&result, &player->pos, &p->pos, &p->vel, &params);
    break;

  case ENEMY_FLEEING:
    steering_flee(&result, &player->pos, &p->pos, &p->vel, &params);
    break;

  default:
    printf("enemyagent_update unknown state: %d\n", agent->state);
  }

  if(result.computed) {
    steeringresult_complete(&result, &params);
    particle_applysteering(p, &result, &params, dt);
  }

  if(clock_time(main_clock) < agent->next_timer) return;
  agent->next_timer = clock_time(main_clock) +
    clock_seconds_to_cycles(enemy_fire_rate);

  if(agent->state == ENEMY_ATTACKING) {
    enemy_fire(p);
  }
}

Enemy spawn_enemy() {
  Enemy enemy = enemy_make();
  enemy->particle.image = image_enemy;
  enemy->particle.scale = 1.0f;

  int nrows = floor(screen_height / image_enemy->h);
  enemy->particle.pos.x = screen_width + (image_enemy->w / 2);
  enemy->particle.pos.y =
    image_enemy->h * rand_in_range(0, nrows)
    + (image_enemy->h / 2);
  enemy->particle.vel.x = -rand_in_range(enemy_speed, 2*enemy_speed);
  enemy->particle.vel.y = 0;
  enemy->particle.angle = 180.0;
  enemy->particle.dsdt = 0.0f;
  enemy->particle.dadt = 0.0f;
  enemy->agent.hp = 100;
  enemyagent_fill(&enemy->agent, enemyagent_update, enemyagent_free);
  return enemy;
}

Particle spawn_bullet(Vector pos, Vector vel, ImageResource image) {
  Particle bullet = particle_make();
  bullet->image = image;
  bullet->pos = *pos;
  bullet->vel = *vel;
  bullet->scale = 0.25f;
  bullet->dsdt = 0.5;
  bullet->angle = rand_in_range(0, 360);
  bullet->dadt = 500;
  return bullet;
}

PrettyParticle spawn_smoke(Vector pos, Vector vel) {
  PrettyParticle smoke = prettyparticle_make();
  smoke->particle.image = image_smoke;
  smoke->particle.pos = *pos;
  smoke->particle.vel = *vel;
  smoke->particle.angle = rand_in_range(0, 360);
  smoke->particle.dsdt = 0.5f * rand_in_range(1, 4);
  smoke->particle.dadt = rand_in_range(-20, 20);
  smoke->end_time =
    clock_time(main_clock) +
    clock_seconds_to_cycles(rand_in_range(500, 3500) / 1000.0f);

  return smoke;
}

typedef int(*ParticleTest)(Particle p);
typedef void(*ParticleRemove)(DLL, Particle);

// step all particles forward and remove those that fail the test
void particles_update(DLL list, float dt,
                      ParticleTest test, ParticleRemove remove) {
  Particle p = (Particle)list->head;

  while(p != NULL) {
    particle_integrate(p, dt);
    Particle next = (Particle)p->node.next;

    if(!test(p)) {
      remove(list, p);
    }

    p = next;
  }
}

void enemies_update(float dt) {
  Particle p = (Particle)enemies.head;
  while(p != NULL) {
    particle_integrate(p, dt);
    p = (Particle)p->node.next;
  }

  if(dll_count(&enemies) < 10) {
    Enemy enemy = spawn_enemy();
    dll_add_head(&enemies, (DLLNode)&enemy->particle);

    Message spawn = message_make(NULL, COLLECTIVE_ADD_AGENT, &enemy->agent);
    message_postinbox((Agent)collective, spawn);
  }
}

int staying_onscreen_test(Particle p) {
  float hw = particle_width(p) / 2.0f;
  float hh = particle_height(p) / 2.0f;

  if(p->vel.x > 0) {
    if(p->pos.x > screen_width + hw) return 0;
  } else {
    if(p->pos.x < -hw) return 0;
  }

  if(p->vel.y > 0) {
    if(p->pos.y > screen_height + hh) return 0;
  } else {
    if(p->pos.y < -hh) return 0;
  }

  return 1;
}

void player_bullets_update(float dt) {
  particles_update(&player_bullets, dt, staying_onscreen_test,
                   particle_remove);
}

void enemy_bullets_update(float dt) {
  particles_update(&enemy_bullets, dt, staying_onscreen_test,
                   particle_remove);
}

int particle_timeout_test(Particle p) {
  PrettyParticle smoke = (PrettyParticle)p;
  return clock_time(main_clock) < smoke->end_time;
}

void prettyparticles_update(float dt) {
  particles_update(&pretty_particles, dt, &particle_timeout_test,
                       (ParticleRemove)&prettyparticle_remove);
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
    rect_for_particle(&(crs[ii].rect), (Particle)node, scale);
    crs[ii].data = node;
    crs[ii].skip = 0;
    node = node->next;
    ++ii;
  }

  return crs;
}

CollisionRecord enemies_collisionrecords(DLL list, int* count, float scale) {
  *count = dll_count(list);
  CollisionRecord crs = frame_alloc(sizeof(struct CollisionRecord_) *
                                    *count);
  DLLNode node = list->head;
  int ii = 0;
  while(node) {
    EnemyAgent enemyagent = (EnemyAgent)(((Dispatchee)node)->agent);
    Particle ep = enemyagent_particle(enemyagent);

    rect_for_particle(&(crs[ii].rect), ep, scale);
    crs[ii].data = enemyagent;
    crs[ii].skip = 0;
    node = node->next;
    ++ii;
  }

  return crs;
}

void bullet_vs_agent(CollisionRecord bullet, CollisionRecord enemy, void* dispatcher_) {
  Dispatcher dispatcher = (Dispatcher)dispatcher_;
  particle_remove(&player_bullets, bullet->data);

  Agent enemyagent = (Agent)enemy->data;
  agent_send_terminate(enemyagent, (Agent)dispatcher);

  bullet->skip = 1;
  enemy->skip = 1;
}

void collision_dispatcher_update(Agent agent, float dt) {
  Dispatcher dispatcher = (Dispatcher)agent;

  // drain our inbox
  foreach_inboxmessage(agent, NULL, NULL);

  // drain the outboxes of our dispatchees
  foreach_dispatcheemessage(dispatcher, NULL, NULL);

  int num_bullets;
  int num_enemies;

  CollisionRecord pbs =
    particles_collisionrecords(&player_bullets, &num_bullets, 0.7f);
  CollisionRecord es =
    enemies_collisionrecords(&dispatcher->dispatchees, &num_enemies, 0.8f);

  // disregard any enemies that have crossed the screen
  int ii;
  for(ii = 0; ii < num_enemies; ++ii) {
    CollisionRecord rec = &es[ii];
    EnemyAgent enemyagent = (EnemyAgent)rec->data;
    Particle p = enemyagent_particle(enemyagent);
    if(p->pos.x < -(particle_width(p) / 2.0f)) {
      agent_send_terminate((Agent)enemyagent, (Agent)dispatcher);
    }
  }

  collide_arrays(pbs, num_bullets, es, num_enemies, &bullet_vs_agent, dispatcher);
}

void sprite_submit(Sprite sprite) {
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
    fixed_allocator_make(sizeof(struct PrettyParticle_),
                         MAX_NUM_PRETTYPARTICLES,
                         "prettyparticle_allocator");

  enemy_allocator =
    fixed_allocator_make(sizeof(struct Enemy_),
                         MAX_NUM_PARTICLES,
                         "enemy_allocator");

  main_clock = clock_make();

  stars = image_load("spacer/night-sky-stars.jpg");
  image_enemy = image_load("spacer/ship-right.png");
  image_player_bullet = image_load("spacer/plasma.png");
  image_enemy_bullet = image_load("spacer/enemy-bullet.png");
  image_smoke = image_load("spacer/smoke.png");

  dll_zero(&enemies);
  dll_zero(&player_bullets);
  dll_zero(&enemy_bullets);
  dll_zero(&pretty_particles);

  player = particle_make();
  player->image = image_load("spacer/hero.png");
  player->pos.x = player->image->w;
  player->pos.y = screen_height / 2;
  player->vel.x = 0;
  player->vel.y = 0;

  player_gun_latch.period = 0.2;
  player_gun_latch.latch_value = 0;
  player_gun_latch.last_time = 0;
  player_gun_latch.last_state = 0;

  Dispatcher dispatchers[COLLECTIVE_SUB_DISPATCHERS] = {
    dispatcher_make(collision_dispatcher_update),
    dispatcher_make(coordinator_update),
  };

  collective = collective_make(dispatchers);
}

void player_fire() {
  struct Vector_ v = { player_bullet_speed, 0.0f };
  Particle bullet = spawn_bullet(&player->pos, &v,
                                     image_player_bullet);
  dll_add_head(&player_bullets, (DLLNode)bullet);
}

void enemy_fire(Particle enemy) {
  struct Vector_ v;
  vector_norm(&v, &enemy->vel);
  vector_scale(&v, &v, enemy_bullet_speed);

  Particle bullet = spawn_bullet(&enemy->pos, &v,
                                 image_enemy_bullet);
  dll_add_head(&enemy_bullets, (DLLNode)bullet);
}

void handle_input(InputState state) {
  player->vel.x = state->leftright * player_speed;
  player->vel.y = state->updown * player_speed;
  if(repeatinglatch_state(&player_gun_latch, main_clock, state->action1)) {
    player_fire();
  }
}

long step_number = 0;

void game_step(long delta, InputState state) {
  float dt = clock_update(main_clock, delta / 1000.0);
  ++step_number;

  Sprite background = frame_resource_sprite(stars);
  background->displayX = 0;
  background->displayY = 0;
  background->w = screen_width;
  background->h = screen_height;
  sprite_submit(background);

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
  agent_update((Agent)collective, dt);

  // draw the particles
  spritelist_enqueue_for_screen(particles_spritelist(&pretty_particles));

  // draw the enemies
  spritelist_enqueue_for_screen(particles_spritelist(&enemies));

  // draw the bullets
  spritelist_enqueue_for_screen(particles_spritelist(&player_bullets));
  spritelist_enqueue_for_screen(particles_spritelist(&enemy_bullets));

  // draw the player
  sprite_submit(particle_sprite((Particle)player));

  if(step_number % 100 == 0) {
    printf("particle_allocator: %ld (%ld)\n",
           particle_allocator->inflight,
           particle_allocator->max_inflight);
  }
}

void game_shutdown() {

}
