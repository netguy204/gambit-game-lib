#ifndef AGENT_C
#define AGENT_C

#include "listlib.h"
#include "heapvector.h"
#include "ooc.h"

class Message;
class Agent;

typedef void(*ReportCompleted)(Message*);

// messages can either be commands or reports. The direction of flow
// determines which. Also, only reports go in outboxes, only commands
// go in inboxes.
class Message {
 public:
  struct DLLNode_ node;
  ReportCompleted report_completed; // called if this is a report
  Agent* source;
  void* data;
  void* data2;
  int kind;
  int read_count;
};

Message* node_to_message(DLLNode node);

class Agent : public Object {
 public:
  OBJECT_PROTO(Agent);

  Agent();
  virtual ~Agent();

  virtual void update(float dt);

  struct DLLNode_ node; // list of ownership siblings
  struct DLL_ inbox;
  struct DLL_ outbox;
  long next_timer;
  int delta_subscribers; // subscriber count update is deferred
  int subscribers;
  int state;
};

DLL agent_inbox(Agent* agent);
DLL agent_outbox(Agent* agent);

class Dispatcher : public Agent {
 public:
  OBJECT_PROTO(Dispatcher);

  Dispatcher();
  virtual ~Dispatcher();

  virtual void update(float dt);

  LLNode dispatchees;
};

class Collective : public Dispatcher {
 public:
  OBJECT_PROTO(Collective);

  Collective();
  virtual ~Collective();

  virtual void update(float dt);

  struct DLL_ children;
  HeapVector sub_dispatchers;
};

enum MessageKind {
  COLLECTIVE_ADD_AGENT,   // collective should own agent
  MESSAGE_MAX0,
  MESSAGE_TERMINATE,      // command agent to terminate
  MESSAGE_TERMINATING,    // agent is terminating
  MESSAGE_COLLIDING,      // agent is colliding with other (cother, cself)
  MESSAGE_TIMER_EXPIRED,  // args (payload)
  MESSAGE_MAX1,
  AGENT_TAKE_DAMAGE,      // command agent to take damage
  AGENT_START_ATTACK,
  AGENT_FLEE,
  MESSAGE_MAX2
};

enum State {
  COLLECTIVE_IDLE,
  DISPATCHER_IDLE,
  AGENT_IDLE,
  DISPATCHER_ATTACKING,
  ENEMY_FLEEING,
  ENEMY_IDLE,
  ENEMY_ATTACKING,
  ENEMY_DYING,
  ENEMY_MAX
};

Message* message_make(Agent* source, int kind, void* data);
void message_free(Message* message);
void message_report_read(Message* message);
void message_postinbox(Agent* dst, Message* message);
void message_postoutbox(Agent* src, Message* message, ReportCompleted report_completed);
void messages_dropall(Agent* agent);

void dispatcher_add_agent(Dispatcher* dispatcher, Agent* agent);
void dispatcher_remove_agent(Dispatcher* dispatcher, Agent* agent);

typedef void(*OutboxMessageCallback)(Dispatcher* dispatcher, Message* message, void * udata);
typedef void(*InboxMessageCallback)(Agent* agent, Message* message, void * udata);

void agent_send_terminate(Agent* agent, Agent* source);

// callback for TERMINATING outbox messages. completes graceful agent
// termination.
void agent_terminate_report_complete(Message* message);

void foreach_dispatcheemessage(Dispatcher* dispatcher, OutboxMessageCallback callback,
                               void * udata);
void foreach_outboxmessage(Dispatcher* dispatcher, Agent* agent,
                           OutboxMessageCallback callback, void * udata);
void foreach_inboxmessage(Agent* agent, InboxMessageCallback callback, void * udata);


#endif
