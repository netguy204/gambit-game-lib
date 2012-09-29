#include "agent.h"
#include "memory.h"
#include "config.h"
#include "game.h"
#include "particle.h"

#include <stdio.h>

FixedAllocator message_allocator;
FixedAllocator agent_allocator;
FixedAllocator dispatchee_allocator;

void agent_init() {
  message_allocator =
    fixed_allocator_make(sizeof(struct Message_),
                         MAX_NUM_MESSAGES,
                         "message_allocator");

  agent_allocator =
    fixed_allocator_make(MAX(sizeof(struct Dispatcher_),
                             sizeof(struct Collective_)),
                         MAX_NUM_AGENTS,
                         "agent_allocator");

  dispatchee_allocator =
    fixed_allocator_make(sizeof(struct Dispatchee_),
                         MAX_NUM_DISPATCHEES,
                         "dispatchee_allocator");
}

Message message_make(Agent source, int kind, void* data) {
  Message message = fixed_allocator_alloc(message_allocator);
  message->report_completed = NULL;
  message->source = source;
  message->data = data;
  message->kind = kind;
  message->read_count = 0;
  return message;
}

void message_free(Message message) {
  fixed_allocator_free(message_allocator, message);
}

void message_report_read(Message message) {
  message->read_count += 1;
  if(message->read_count >= message->source->subscribers) {
    if(message->report_completed) {
      message->report_completed(message);
    }
    message_free(message);
  }
}

void message_postinbox(Agent dst, Message message) {
  dll_add_head(&dst->inbox, (DLLNode)message);
}

void message_postoutbox(Agent src, Message message, ReportCompleted report_completed) {
  message->report_completed = report_completed;
  dll_add_head(&src->outbox, (DLLNode)message);
}

void messages_drop(DLL list) {
  Message message = (Message)dll_remove_tail(list);
  while(message) {
    message_free(message);
    message = (Message)dll_remove_tail(list);
  }
}

void messages_dropall(Agent agent) {
  messages_drop(&agent->inbox);
  messages_drop(&agent->outbox);
}

void agent_fill(Agent agent, AgentUpdate update, int state) {
  dll_zero(&agent->inbox);
  dll_zero(&agent->outbox);
  agent->update = update;
  agent->subscribers = 0;
  agent->delta_subscribers = 0;
  agent->state = state;
}

void agent_update(Agent agent) {
  // update the subscriber count
  agent->subscribers += agent->delta_subscribers;
  agent->delta_subscribers = 0;
  agent->update(agent);
}

CollisionRecord enemies_collisionrecords(DLL list, int* count, float scale) {
  *count = dll_count(list);
  CollisionRecord crs = frame_alloc(sizeof(struct CollisionRecord_) *
                                    *count);
  DLLNode node = list->head;
  int ii = 0;
  while(node) {
    EnemyAgent enemyagent = (EnemyAgent)(((Dispatchee)node)->agent);
    Enemy enemy = container_of(enemyagent, struct Enemy_, agent);

    rect_for_particle(&(crs[ii].rect), &enemy->particle, scale);
    crs[ii].data = enemyagent;
    crs[ii].skip = 0;
    node = node->next;
    ++ii;
  }

  return crs;
}

void agent_send_terminate(Agent agent, Agent source) {
  Message terminate = message_make(source, MESSAGE_TERMINATE, NULL);
  message_postinbox(agent, terminate);
}

void bullet_vs_agent(CollisionRecord bullet, CollisionRecord enemy, void* dispatcher_) {
  Dispatcher dispatcher = (Dispatcher)dispatcher_;
  particle_remove(&player_bullets, bullet->data);

  Agent enemyagent = (Agent)enemy->data;
  agent_send_terminate(enemyagent, (Agent)dispatcher);

  bullet->skip = 1;
  enemy->skip = 1;
}

void collision_dispatcher_update(Dispatcher dispatcher) {
  // drain our inbox

  // drain the outboxes of our dispatchees
  Dispatchee entry = (Dispatchee)dispatcher->dispatchees.head;
  while(entry) {
    Dispatchee nentry = (Dispatchee)entry->node.next;

    Message message = (Message)entry->agent->outbox.head;
    while(message) {
      Message next = (Message)message->node.next;

      switch(message->kind) {
      case MESSAGE_TERMINATING:
        dispatcher_remove_agent(dispatcher, message->source);
        break;

      default:
        printf("DISPATCHER: outbox unhandled message kind: %d\n",
               message->kind);
      }

      message_report_read(message);
      message = next;
    }

    entry = nentry;
  }


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
    Enemy enemy = container_of(enemyagent, struct Enemy_, agent);
    Particle p = &enemy->particle;
    if(p->pos.x < -(particle_width(p) / 2.0f)) {
      agent_send_terminate((Agent)enemyagent, (Agent)dispatcher);
    }
  }

  collide_arrays(pbs, num_bullets, es, num_enemies, &bullet_vs_agent, dispatcher);
}

void dispatcher_fill(Dispatcher dispatcher, AgentUpdate update, int state) {
  agent_fill((Agent)dispatcher, update, state);
  dll_zero(&dispatcher->dispatchees);
}

Dispatcher collision_dispatcher_make() {
  Dispatcher dispatcher = fixed_allocator_alloc(agent_allocator);
  dispatcher_fill(dispatcher, (AgentUpdate)&collision_dispatcher_update, COLLISION_IDLE);
  return dispatcher;
}

Dispatchee dispatcher_find_agent(Dispatcher dispatcher, Agent agent) {
  Dispatchee entry = (Dispatchee)dispatcher->dispatchees.head;
  while(entry) {
    if(entry->agent == agent) return entry;
    entry = (Dispatchee)entry->node.next;
  }
  return NULL;
}

void dispatcher_add_agent(Dispatcher dispatcher, Agent agent) {
  Dispatchee entry = fixed_allocator_alloc(dispatchee_allocator);
  entry->agent = agent;
  dll_add_head(&dispatcher->dispatchees, (DLLNode)entry);
  agent->delta_subscribers += 1;
}

void dispatcher_remove_agent(Dispatcher dispatcher, Agent agent) {
  Dispatchee entry = dispatcher_find_agent(dispatcher, agent);
  SAFETY(if(!entry) fail_exit("dispatcher_remove_agent: not found"));
  agent->delta_subscribers -= 1;
  dll_remove(&dispatcher->dispatchees, (DLLNode)entry);
  fixed_allocator_free(dispatchee_allocator, entry);
}

// tie an agent's lifetime to this collective
void collective_add(Collective collective, Agent agent) {
  dll_add_head(&collective->children, (DLLNode)agent);
  // we want messages from things we own too so we can drop them if
  // they tell us they died
  dispatcher_add_agent((Dispatcher)collective, agent);
}

void collective_remove(Collective collective, Agent agent) {
  dll_remove(&collective->children, (DLLNode)agent);
  dispatcher_remove_agent((Dispatcher)collective, agent);
}

void collective_add_enemy(Collective collective, void * data) {
  Enemy enemy = (Enemy)data;

  collective_add(collective, (Agent)&enemy->agent);
  dispatcher_add_agent(collective->collision_dispatcher, (Agent)&enemy->agent);
}

void collective_update(Collective collective) {
  // drain inbox
  Message message = (Message)dll_remove_tail(&collective->dispatcher.agent.inbox);
  while(message) {
    switch(message->kind) {
    case COLLECTIVE_ADD_ENEMY:
      collective_add_enemy(collective, message->data);
      break;

    default:
      printf("COLLECTIVE: Unhandled message kind: %d\n", message->kind);
    }
    message_free(message);
    message = (Message)dll_remove_tail(&collective->dispatcher.agent.inbox);
  }

  // update all of our sub-agents
  Agent child = (Agent)collective->children.head;
  while(child) {
    agent_update(child);
    child = (Agent)child->node.next;
  }

  // drain the outboxes of our dispatchees
  Dispatchee entry = (Dispatchee)collective->dispatcher.dispatchees.head;
  while(entry) {
    Dispatchee nentry = (Dispatchee)entry->node.next;

    Message message = (Message)entry->agent->outbox.head;
    while(message) {
      Message next = (Message)message->node.next;

      switch(message->kind) {
      case MESSAGE_TERMINATING:
        collective_remove(collective, message->source);
        break;

      default:
        printf("COLLECTIVE: outbox unhandled message kind: %d\n",
               message->kind);
      }

      message_report_read(message);
      message = next;
    }
    entry = nentry;
  }
}

Collective collective_make() {
  Collective collective = fixed_allocator_alloc(agent_allocator);
  dispatcher_fill((Dispatcher)collective, (AgentUpdate)collective_update, COLLECTIVE_IDLE);
  dll_zero(&collective->children);
  collective->collision_dispatcher = collision_dispatcher_make();
  collective_add(collective, (Agent)collective->collision_dispatcher);
  return collective;
}

void enemyagent_terminate_report_complete(Message message) {
  // now we can free ourselves
  EnemyAgent enemy = (EnemyAgent)message->source;
  enemyagent_free(enemy);
}

void enemyagent_update(EnemyAgent enemyagent) {
  // drain our inbox
  Message message = (Message)dll_remove_tail(&enemyagent->agent.inbox);
  Message reply;

  while(message) {
    switch(message->kind) {
    case MESSAGE_TERMINATE:
      enemyagent->agent.state = ENEMY_DYING;
      reply = message_make((Agent)enemyagent, MESSAGE_TERMINATING, NULL);
      message_postoutbox((Agent)enemyagent, reply, &enemyagent_terminate_report_complete);
      break;
    default:
      printf("Unhandled message kind: %d\n", message->kind);
    }

    message_free(message);
    message = (Message)dll_remove_tail(&enemyagent->agent.inbox);
  }

  if(enemyagent->agent.state != ENEMY_DYING) {
    // bail if we can't hit the player
    Enemy enemy = container_of(enemyagent, struct Enemy_, agent);

    if((enemy->particle.pos.x < player->pos.x) ||
       (abs(enemy->particle.pos.y - player->pos.y)
        > particle_height(player))) {
      return;
    }

    // take a shot if we can
    long current_time = clock_time(main_clock);
    long dtl = current_time - enemyagent->last_fire;
    float dt = clock_cycles_to_seconds(dtl);

    if(dt > enemy_fire_rate) {
      enemy_fire(&enemy->particle);
      enemyagent->last_fire = current_time;
    }
  }
}

void enemyagent_fill(EnemyAgent enemy) {
  agent_fill((Agent)enemy, (AgentUpdate)enemyagent_update, ENEMY_IDLE);
  enemy->last_fire = 0;
}

void enemyagent_free(EnemyAgent enemyagent) {
  // is it possible to leak messages at this point?
  Enemy enemy = container_of(enemyagent, struct Enemy_, agent);
  dll_remove(&enemies, (DLLNode)&enemy->particle);

  // sentinal so our unit tests will know we did right
  SAFETY(enemy->agent.agent.state = ENEMY_MAX);
  enemy_free(enemy);
}
