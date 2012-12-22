#include "agent.h"
#include "memory.h"
#include "utils.h"
#include "config.h"
#include "game.h"
#include "particle.h"

#include <stdio.h>
#include <assert.h>
#include <stdarg.h>

FixedAllocator message_allocator;

Message* node_to_message(DLLNode node) {
  return container_of(node, Message, node);
}

OBJECT_IMPL(Agent);

Agent::Agent() {
  dll_zero(&this->inbox);
  dll_zero(&this->outbox);
  this->subscribers = 0;
  this->delta_subscribers = 0;
  this->state = 0;
  this->next_timer = 0;
}

Agent::~Agent() {
  // should we do something with the agent's mailboxes?
  SAFETY(if(this->inbox.head != NULL) fail_exit("agent inbox not empty"));
  SAFETY(if(this->outbox.head != NULL) fail_exit("agent outbox not empty"));
}

void Agent::update(float dt) {
  // update the subscriber count
  this->subscribers += this->delta_subscribers;
  this->delta_subscribers = 0;
}

DLL agent_inbox(Agent* agent) {
  return &agent->inbox;
}

// dispatcher methods

OBJECT_IMPL(Dispatcher);

Dispatcher::Dispatcher() {
  this->dispatchees = NULL;
}

Dispatcher::~Dispatcher() {
  // a dispatcher should free its entries (but not the agents they
  // point to since lifetime of agents is controlled by the
  // collective)
  LLNode node = this->dispatchees;
  while(node) {
    LLNode next = node->next;
    llentry_free((LLEntry)node);
    node = next;
  }
  this->dispatchees = NULL;
}

void Dispatcher::update(float dt) {
}

// collective

void collective_add(Collective* collective, Agent* agent);

OBJECT_IMPL(Collective);

Collective::Collective() {
  dll_zero(&this->children);
  this->sub_dispatchers = heapvector_make();
}

Collective::~Collective() {
  // we own the lifetimes of our agents so we destroy them too
  DLLNode node = this->children.head;
  while(node) {
    DLLNode next = node->next;
    Agent* agent = container_of(node, Agent, node);
    delete agent;
    node = next;
  }

  heapvector_free(this->sub_dispatchers);
}

Message* message_make(Agent* source, int kind, void* data) {
  Message* message = new Message();
  message->report_completed = NULL;
  message->source = source;
  message->data = data;
  message->kind = kind;
  message->read_count = 0;
  return message;
}

void message_free(Message* message) {
  delete message;
}

void message_report_read(Message* message) {
  message->read_count += 1;
  if(message->read_count >= message->source->subscribers) {
    dll_remove(&message->source->outbox, (DLLNode)message);

    if(message->report_completed) {
      message->report_completed(message);
    }
    message_free(message);
  }
}

void message_postinbox(Agent* dst, Message* message) {
  dll_add_head(&dst->inbox, (DLLNode)message);
}

void message_postoutbox(Agent* src, Message* message, ReportCompleted report_completed) {
  message->report_completed = report_completed;
  dll_add_head(&src->outbox, (DLLNode)message);
}

void messages_drop(DLL list) {
  Message* message = (Message*)dll_remove_tail(list);
  while(message) {
    message_free(message);
    message = (Message*)dll_remove_tail(list);
  }
}

void messages_dropall(Agent* agent) {
  messages_drop(&agent->inbox);
  messages_drop(&agent->outbox);
}

void agent_send_terminate(Agent* agent, Agent* source) {
  Message* terminate = message_make(source, MESSAGE_TERMINATE, NULL);
  message_postinbox(agent, terminate);
}

// a null callback will result in proper dispatchee exit handling and
// will mark all messages read. Assumed to be called by a outbox
// reader (a dispatcher)
void foreach_outboxmessage(Dispatcher* dispatcher, Agent* agent,
                           OutboxMessageCallback callback, void * udata) {

  DLLNode node = agent->outbox.head;
  int agent_terminating = 0;

  while(node) {
    DLLNode next = node->next;
    Message* message = node_to_message(node);

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
    node = next;
  }
}

void foreach_dispatcheemessage(Dispatcher* dispatcher, OutboxMessageCallback callback,
                               void * udata) {
  LLNode node = dispatcher->dispatchees;
  Agent* agent;
  while((agent = (Agent*)llentry_nextvalue(&node))) {
    foreach_outboxmessage(dispatcher, agent, callback, udata);
  }
}

void agent_terminate_report_complete(Message* message) {
  // now we can free ourselves
  Agent* agent = message->source;
  SAFETY(agent->state = ENEMY_MAX);
  delete agent;
}

void foreach_inboxmessage(Agent* agent, InboxMessageCallback callback, void * udata) {
  Message* message = (Message*)dll_remove_tail(&agent->inbox);

  int terminate_requested = 0;

  while(message) {
    if(message->kind == MESSAGE_TERMINATE) {
      terminate_requested = 1;

      if(!callback) {
        Message* reply = message_make(agent, MESSAGE_TERMINATING, NULL);
        message_postoutbox(agent, reply, &agent_terminate_report_complete);
      } else {
        callback(agent, message, udata);
      }
    } else if(!terminate_requested && callback) {
      callback(agent, message, udata);
    }

    message_free(message);
    message = (Message*)dll_remove_tail(&agent->inbox);
  }
}

void dispatcher_add_agent(Dispatcher* dispatcher, Agent* agent) {
  llentry_add(&dispatcher->dispatchees, agent);
  agent->delta_subscribers += 1;
}

void dispatcher_remove_agent(Dispatcher* dispatcher, Agent* agent) {
  llentry_remove(&dispatcher->dispatchees, agent);
  agent->delta_subscribers -= 1;
}

// tie an agent's lifetime to this collective
void collective_add(Collective* collective, Agent* agent) {
  dll_add_head(&collective->children, &agent->node);
  // we want messages from things we own too so we can drop them if
  // they tell us they died
  dispatcher_add_agent(collective, agent);
}

void collective_remove(Collective* collective, Agent* agent) {
  dll_remove(&collective->children, &agent->node);
  dispatcher_remove_agent(collective, agent);
}

void collective_add_agent(Collective* collective, void * data) {
  Agent* agent = (Agent*)data;

  collective_add(collective, agent);

  int ii = 0;
  for(ii = 0; ii < HV_SIZE(collective->sub_dispatchers, Dispatcher*); ++ii) {
    Dispatcher* dispatcher = *HV_GET(collective->sub_dispatchers, Dispatcher*, ii);
    dispatcher_add_agent(dispatcher, agent);
  }
}

void collective_handle_outboxes(Dispatcher* disp, Message* message, void * udata) {
  Collective* collective = (Collective*)disp;

  switch(message->kind) {
  case MESSAGE_TERMINATING:
    collective_remove(collective, message->source);
    break;
  default:
    printf("COLLECTIVE: outbox unhandled message kind: %d\n",
           message->kind);
  }
}

void collective_handle_inbox(Agent* agent, Message* message, void * udata) {
  Collective* collective = (Collective*)agent;

  switch(message->kind) {
  case COLLECTIVE_ADD_AGENT:
    collective_add_agent(collective, message->data);
    break;

  default:
    printf("COLLECTIVE: Unhandled message kind: %d\n", message->kind);
  }
}

void Collective::update(float dt) {
  // drain inbox
  foreach_inboxmessage(this, collective_handle_inbox, NULL);

  // FIXME: is this necessary?
  Message* message = (Message*)dll_remove_tail(&this->inbox);
  while(message) {
    message_free(message);
    message = (Message*)dll_remove_tail(&this->inbox);
  }

  // update all of our sub-agents
  DLLNode child = this->children.head;
  while(child) {
    Agent* agent = container_of(child, Agent, node);
    agent->update(dt);
    child = child->next;
  }

  // drain the outboxes of our dispatchees
  foreach_dispatcheemessage(this, collective_handle_outboxes, NULL);
}
