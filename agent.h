#ifndef AGENT_C
#define AGENT_C

#include "listlib.h"

void agent_init();

struct Message_;
struct Agent_;
struct Particle_;

typedef void(*ReportCompleted)(struct Message_*);

// messages can either be commands or reports. The direction of flow
// determines which. Also, only reports go in outboxes, only commands
// go in inboxes.
typedef struct Message_ {
  struct DLLNode_ node;
  ReportCompleted report_completed; // called if this is a report
  struct Agent_* source;
  void* data;
  int kind;
  int read_count;
} *Message;

struct Agent_;

typedef void(*AgentUpdate)(struct Agent_*);

typedef struct Agent_ {
  struct DLLNode_ node; // list of ownership siblings
  struct DLL_ inbox;
  struct DLL_ outbox;
  AgentUpdate update;
  int delta_subscribers; // subscriber count update is deferred
  int subscribers;
  int state;
} *Agent;

// because many dispatchers can share the same agent we have to keep
// our list structure outside the agent
typedef struct Dispatchee_ {
  struct DLLNode_ node;
  Agent agent;
} *Dispatchee;

typedef struct Dispatcher_ {
  struct Agent_ agent;
  struct DLL_ dispatchees;
} *Dispatcher;

typedef struct Collective_ {
  struct Dispatcher_ dispatcher;
  struct DLL_ children;
  Dispatcher collision_dispatcher;
} *Collective;

typedef struct Enemy_ {
  struct Agent_ agent;
  struct Particle_* visual;
  int hp;
} *Enemy;

enum MessageKind {
  COLLECTIVE_SPAWN_ENEMY, // collective should create enemy
  MESSAGE_MAX0,
  MESSAGE_TERMINATE,      // command agent to terminate
  MESSAGE_TERMINATING,    // agent is terminating
  MESSAGE_MAX1,
  AGENT_TAKE_DAMAGE,      // command agent to take damage
  MESSAGE_MAX2
};

enum State {
  COLLECTIVE_IDLE,
  COLLISION_IDLE,
  ENEMY_ATACKING,
  ENEMY_IDLE,
  ENEMY_DYING,
  ENEMY_MAX
};

Message message_make(Agent source, int kind, void* data);
void message_free(Message message);
void message_report_read(Message message);
void message_postinbox(Agent dst, Message message);
void message_postoutbox(Agent src, Message message, ReportCompleted report_completed);
void messages_dropall(Agent agent);

// eventually orchestrates the scenario
Collective collective_make();
Dispatcher collision_dispatcher_make();

void dispatcher_add_agent(Dispatcher dispatcher, Agent agent);
void dispatcher_remove_agent(Dispatcher dispatcher, Agent agent);

void agent_update(Agent agent);

Enemy enemy_make(struct Particle_* particle, int hp);
void enemy_free(Enemy enemy);

#endif
