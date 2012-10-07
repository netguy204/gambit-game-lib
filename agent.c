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
    dll_remove(&message->source->outbox, (DLLNode)message);

    if(message->report_completed) {
      message->report_completed(message);
    }
    message_free(message);
  }
}

void basicagent_free(Agent agent) {
  fixed_allocator_free(agent_allocator, agent);
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

void agent_fill(Agent agent, AgentClass klass, int state) {
  dll_zero(&agent->inbox);
  dll_zero(&agent->outbox);
  agent->klass = klass;
  agent->subscribers = 0;
  agent->delta_subscribers = 0;
  agent->state = state;
  agent->next_timer = 0;
}

void agent_update(Agent agent, float dt) {
  // update the subscriber count
  agent->subscribers += agent->delta_subscribers;
  agent->delta_subscribers = 0;
  agent->klass->update(agent, dt);
}

void agent_free(Agent agent) {
  // should we do something with the agent's mailboxes?
  SAFETY(if(agent->inbox.head != NULL) fail_exit("agent inbox not empty"));
  SAFETY(if(agent->outbox.head != NULL) fail_exit("agent outbox not empty"));
  agent->klass->free(agent);
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

  Dispatchee entry = (Dispatchee)dispatcher->dispatchees.head;
  while(entry) {
    Dispatchee nentry = (Dispatchee)entry->node.next;
    foreach_outboxmessage(dispatcher, entry->agent, callback, udata);
    entry = nentry;
  }
}

void agent_terminate_report_complete(Message message) {
  // now we can free ourselves
  Agent agent = (Agent)message->source;
  SAFETY(agent->state = ENEMY_MAX);
  agent_free(agent);
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

void dispatcher_fill(Dispatcher dispatcher, AgentClass klass, int state) {
  agent_fill((Agent)dispatcher, klass, state);
  dll_zero(&dispatcher->dispatchees);
}

Dispatcher dispatcher_make(AgentClass klass) {
  Dispatcher dispatcher = fixed_allocator_alloc(agent_allocator);
  dispatcher_fill(dispatcher, klass, DISPATCHER_IDLE);
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

void collective_update(Agent agent, float dt) {
  Collective collective = (Collective)agent;

  // drain inbox
  foreach_inboxmessage((Agent)collective, collective_handle_inbox, NULL);

  Message message = (Message)dll_remove_tail(&collective->dispatcher.agent.inbox);
  while(message) {
    message_free(message);
    message = (Message)dll_remove_tail(&collective->dispatcher.agent.inbox);
  }

  // update all of our sub-agents
  Agent child = (Agent)collective->children.head;
  while(child) {
    agent_update(child, dt);
    child = (Agent)child->node.next;
  }

  // drain the outboxes of our dispatchees
  foreach_dispatcheemessage((Dispatcher)collective, collective_handle_outboxes, NULL);
}

struct AgentClass_ collective_klass = {collective_update, basicagent_free};

Collective collective_make(Dispatcher dispatchers[COLLECTIVE_SUB_DISPATCHERS]) {
  Collective collective = fixed_allocator_alloc(agent_allocator);
  dispatcher_fill((Dispatcher)collective, &collective_klass, COLLECTIVE_IDLE);
  dll_zero(&collective->children);

  int ii = 0;
  for(ii = 0; ii < COLLECTIVE_SUB_DISPATCHERS; ++ii) {
    Dispatcher dispatcher = dispatchers[ii];
    collective->sub_dispatchers[ii] = dispatcher;

    if(dispatcher) {
      collective_add(collective, (Agent)dispatcher);
    }
  }

  return collective;
}
