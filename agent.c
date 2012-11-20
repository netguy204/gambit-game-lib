#include "agent.h"
#include "memory.h"
#include "utils.h"
#include "config.h"
#include "game.h"
#include "particle.h"

#include <stdio.h>
#include <assert.h>
#include <stdarg.h>

const void* AgentClass;
const void* AgentObject;
const void* DispatcherObject;
const void* CollectiveObject;

FixedAllocator message_allocator;
FixedAllocator agent_allocator;

// new selector added in AgentClass metaclass
void agent_update(void* _self, float dt) {
  const struct AgentClass_* class = classOf(_self);
  assert(class->agent_update);
  class->agent_update(_self, dt);
}

void super_agent_update(const void* _class, void* _self, float dt) {
  const struct AgentClass_* superclass = super(_class);
  assert(_self && superclass->agent_update);
  superclass->agent_update(_self, dt);
}

static void* AgentClass_ctor(void* _self, va_list * app) {
  struct AgentClass_* self = super_ctor(AgentClass, _self, app);
  va_list ap;
  va_copy(ap, *app);

  while(1) {
    voidf selector = va_arg(ap, voidf);
    if(!((int)selector)) break;

    voidf method = va_arg(ap, voidf);
    if(selector == (voidf)agent_update) {
      *(voidf*)&self->agent_update = method;
    }
  }

  return self;
}

// agentobject methods
static void* AgentObject_alloci(void* _self) {
  return fixed_allocator_alloc(agent_allocator);
}

static void AgentObject_dealloci(void* _self) {
  fixed_allocator_free(agent_allocator, _self);
}

void* AgentObject_ctor(void* _self, va_list* app) {
  Agent agent = super_ctor(AgentObject, _self, app);

  dll_zero(&agent->inbox);
  dll_zero(&agent->outbox);
  agent->subscribers = 0;
  agent->delta_subscribers = 0;
  agent->state = va_arg(*app, int);
  agent->next_timer = 0;
  return agent;
}

void AgentObject_update(void* _self, float dt) {
  Agent agent = _self;

  // update the subscriber count
  agent->subscribers += agent->delta_subscribers;
  agent->delta_subscribers = 0;
}

void* AgentObject_dtor(void* _self) {
  Agent agent = _self;

  // should we do something with the agent's mailboxes?
  SAFETY(if(agent->inbox.head != NULL) fail_exit("agent inbox not empty"));
  SAFETY(if(agent->outbox.head != NULL) fail_exit("agent outbox not empty"));
  return super_dtor(AgentObject, _self);
}


// dispatcher methods

void* DispatcherObject_ctor(void* _self, va_list* app) {
  Dispatcher dispatcher = super_ctor(DispatcherObject, _self, app);
  dispatcher->dispatchees = NULL;
  return dispatcher;
}

// collective

void collective_add(Collective collective, Agent agent);

void* CollectiveObject_ctor(void* _self, va_list* app) {
  Collective collective = super_ctor(CollectiveObject, _self, app);
  dll_zero(&collective->children);

  int ii = 0;
  for(ii = 0; ii < COLLECTIVE_SUB_DISPATCHERS; ++ii) {
    Dispatcher dispatcher = va_arg(*app, Dispatcher);
    collective->sub_dispatchers[ii] = dispatcher;

    if(dispatcher) {
      collective_add(collective, (Agent)dispatcher);
    }
  }

  return collective;
}

void CollectiveObject_update(void* _self, float dt);

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


  AgentClass = new(Class, "AgentClass",
                   Class, sizeof(struct AgentClass_),
                   ctor, AgentClass_ctor,
                   0);

  AgentObject = new(AgentClass, "Agent",
                    Object, sizeof(struct Agent_),
                    alloci, AgentObject_alloci,
                    dealloci, AgentObject_dealloci,
                    ctor, AgentObject_ctor,
                    dtor, AgentObject_dtor,
                    agent_update, AgentObject_update,
                    0);

  DispatcherObject = new(AgentClass, "Dispatcher",
                         AgentObject, sizeof(struct Dispatcher_),
                         ctor, DispatcherObject_ctor,
                         0);

  CollectiveObject = new(AgentClass, "Collective",
                         DispatcherObject, sizeof(struct Collective_),
                         ctor, CollectiveObject_ctor,
                         agent_update, CollectiveObject_update,
                         0);
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
    dll_remove(&message->source->outbox, (DLLNode)message);

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

void agent_send_terminate(Agent agent, Agent source) {
  Message terminate = message_make(source, MESSAGE_TERMINATE, NULL);
  message_postinbox(agent, terminate);
}

// a null callback will result in proper dispatchee exit handling and
// will mark all messages read. Assumed to be called by a outbox
// reader (a dispatcher)
void foreach_outboxmessage(Dispatcher dispatcher, Agent agent,
                           OutboxMessageCallback callback, void * udata) {

  Message message = (Message)agent->outbox.head;
  int agent_terminating = 0;

  while(message) {
    Message next = (Message)message->node.next;

    if(message->kind == MESSAGE_TERMINATING) {
      if(!callback) {
        dispatcher_remove_agent(dispatcher, message->source);
      } else {
        callback(dispatcher, message, udata);
      }

      agent_terminating = 1;
    } else if(!agent_terminating && callback) {
      callback(dispatcher, message, udata);
    }

    message_report_read(message);
    message = next;
  }
}

void foreach_dispatcheemessage(Dispatcher dispatcher, OutboxMessageCallback callback,
                               void * udata) {
  LLNode node = dispatcher->dispatchees;
  Agent agent;
  while((agent = llentry_nextvalue(&node))) {
    foreach_outboxmessage(dispatcher, agent, callback, udata);
  }
}

void agent_terminate_report_complete(Message message) {
  // now we can free ourselves
  Agent agent = (Agent)message->source;
  SAFETY(agent->state = ENEMY_MAX);
  delete(agent);
}

void foreach_inboxmessage(Agent agent, InboxMessageCallback callback, void * udata) {
  Message message = (Message)dll_remove_tail(&agent->inbox);

  int terminate_requested = 0;

  while(message) {
    if(message->kind == MESSAGE_TERMINATE) {
      terminate_requested = 1;

      if(!callback) {
        Message reply = message_make(agent, MESSAGE_TERMINATING, NULL);
        message_postoutbox(agent, reply, &agent_terminate_report_complete);
      } else {
        callback(agent, message, udata);
      }
    } else if(!terminate_requested && callback) {
      callback(agent, message, udata);
    }

    message_free(message);
    message = (Message)dll_remove_tail(&agent->inbox);
  }
}

void dispatcher_add_agent(Dispatcher dispatcher, Agent agent) {
  llentry_add(&dispatcher->dispatchees, agent);
  agent->delta_subscribers += 1;
}

void dispatcher_remove_agent(Dispatcher dispatcher, Agent agent) {
  llentry_remove(&dispatcher->dispatchees, agent);
  agent->delta_subscribers -= 1;
}

// tie an agent's lifetime to this collective
void collective_add(Collective collective, Agent agent) {
  dll_add_head(&collective->children, &agent->node);
  // we want messages from things we own too so we can drop them if
  // they tell us they died
  dispatcher_add_agent((Dispatcher)collective, agent);
}

void collective_remove(Collective collective, Agent agent) {
  dll_remove(&collective->children, &agent->node);
  dispatcher_remove_agent((Dispatcher)collective, agent);
}

void collective_add_agent(Collective collective, void * data) {
  Agent agent = (Agent)data;

  collective_add(collective, agent);

  int ii = 0;
  for(ii = 0; ii < COLLECTIVE_SUB_DISPATCHERS; ++ii) {
    Dispatcher dispatcher = collective->sub_dispatchers[ii];
    if(dispatcher) {
      dispatcher_add_agent(dispatcher, agent);
    }
  }
}

void collective_handle_outboxes(Dispatcher dispatcher, Message message, void * udata) {
  Collective collective = (Collective)dispatcher;

  switch(message->kind) {
  case MESSAGE_TERMINATING:
    collective_remove(collective, message->source);
    break;
  default:
    printf("COLLECTIVE: outbox unhandled message kind: %d\n",
           message->kind);
  }
}

void collective_handle_inbox(Agent agent, Message message, void * udata) {
  Collective collective = (Collective)agent;

  switch(message->kind) {
  case COLLECTIVE_ADD_AGENT:
    collective_add_agent(collective, message->data);
    break;

  default:
    printf("COLLECTIVE: Unhandled message kind: %d\n", message->kind);
  }
}

void CollectiveObject_update(void* _self, float dt) {
  super_agent_update(CollectiveObject, _self, dt);
  Collective collective = _self;

  // drain inbox
  foreach_inboxmessage((Agent)collective, collective_handle_inbox, NULL);

  Message message = (Message)dll_remove_tail(&collective->dispatcher.agent.inbox);
  while(message) {
    message_free(message);
    message = (Message)dll_remove_tail(&collective->dispatcher.agent.inbox);
  }

  // update all of our sub-agents
  DLLNode child = collective->children.head;
  while(child) {
    agent_update(container_of(child, struct Agent_, node), dt);
    child = child->next;
  }

  // drain the outboxes of our dispatchees
  foreach_dispatcheemessage((Dispatcher)collective, collective_handle_outboxes, NULL);
}
