#ifndef AGENT_C
#define AGENT_C

#include "listlib.h"

void agent_init();

struct Message_;
struct Agent_;

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

typedef void(*AgentUpdate)(struct Agent_*, float dt);
typedef void(*AgentFree)(struct Agent_*);

typedef struct AgentClass_ {
  AgentUpdate update;
  AgentFree free;
} *AgentClass;

typedef struct Agent_ {
  struct DLLNode_ node; // list of ownership siblings
  struct DLL_ inbox;
  struct DLL_ outbox;
  AgentClass klass;
  long next_timer;
  int delta_subscribers; // subscriber count update is deferred
  int subscribers;
  int state;
} *Agent;

typedef struct Dispatcher_ {
  struct Agent_ agent;
  LLNode dispatchees;
} *Dispatcher;

#define COLLECTIVE_SUB_DISPATCHERS 2

typedef struct Collective_ {
  struct Dispatcher_ dispatcher;
  struct DLL_ children;
  Dispatcher sub_dispatchers[COLLECTIVE_SUB_DISPATCHERS];
} *Collective;

typedef struct EnemyAgent_ {
  struct Agent_ agent;
  int hp;
} *EnemyAgent;

enum MessageKind {
  COLLECTIVE_ADD_AGENT,   // collective should own agent
  MESSAGE_MAX0,
  MESSAGE_TERMINATE,      // command agent to terminate
  MESSAGE_TERMINATING,    // agent is terminating
  MESSAGE_MAX1,
  AGENT_TAKE_DAMAGE,      // command agent to take damage
  AGENT_START_ATTACK,
  AGENT_FLEE,
  AGENT_IDLE,
  MESSAGE_MAX2
};

enum State {
  COLLECTIVE_IDLE,
  DISPATCHER_IDLE,
  DISPATCHER_ATTACKING,
  ENEMY_FLEEING,
  ENEMY_IDLE,
  ENEMY_ATTACKING,
  ENEMY_DYING,
  ENEMY_MAX
};

Message message_make(Agent source, int kind, void* data);
void message_free(Message message);
void message_report_read(Message message);
void message_postinbox(Agent dst, Message message);
void message_postoutbox(Agent src, Message message, ReportCompleted report_completed);
void messages_dropall(Agent agent);

void basicagent_free(Agent agent);
void agent_fill(Agent agent, AgentClass klass, int state);

// eventually orchestrates the scenario
Collective collective_make(Dispatcher sub_dispatchers[COLLECTIVE_SUB_DISPATCHERS]);
Dispatcher dispatcher_make(AgentClass klass);

void dispatcher_add_agent(Dispatcher dispatcher, Agent agent);
void dispatcher_remove_agent(Dispatcher dispatcher, Agent agent);

void agent_update(Agent agent, float dt);

typedef void(*OutboxMessageCallback)(Dispatcher dispatcher, Message message, void * udata);
typedef void(*InboxMessageCallback)(Agent agent, Message message, void * udata);

void agent_send_terminate(Agent agent, Agent source);

// callback for TERMINATING outbox messages. completes graceful agent
// termination.
void agent_terminate_report_complete(Message message);

void foreach_dispatcheemessage(Dispatcher dispatcher, OutboxMessageCallback callback,
                               void * udata);
void foreach_outboxmessage(Dispatcher dispatcher, Agent agent,
                           OutboxMessageCallback callback, void * udata);
void foreach_inboxmessage(Agent agent, InboxMessageCallback callback, void * udata);


#endif
