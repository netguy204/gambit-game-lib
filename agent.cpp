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

OBJECT_IMPL(Agent, Object);
OBJECT_PROPERTY(Agent, ttag);

Agent::Agent() {
  this->subscribers = 0;
  this->delta_subscribers = 0;
  this->state = 0;
  this->next_timer = 0;
}

Agent::Agent(void* p) {
  this->subscribers = 0;
  this->delta_subscribers = 0;
  this->state = 0;
  this->next_timer = 0;
}

Agent::~Agent() {
  // should we do something with the agent's mailboxes?
  messages_dropall(this);
}

void Agent::update(float dt) {
  // update the subscriber count
  this->subscribers += this->delta_subscribers;
  this->delta_subscribers = 0;
}

// dispatcher methods

OBJECT_IMPL(Dispatcher, Agent);

Dispatcher::Dispatcher() {
  this->dispatchees = NULL;
}

Dispatcher::Dispatcher(void* p) {
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

OBJECT_IMPL(Collective, Agent);

Collective::Collective() {
  this->sub_dispatchers = heapvector_make();
}

Collective::Collective(void* p) {
  this->sub_dispatchers = heapvector_make();
}

Collective::~Collective() {
  // we own the lifetimes of our agents so we destroy them too
  this->children.foreach([](Agent* agent) -> int {
      delete agent;
      return 0;
    });

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
    message->source->outbox.remove(message);

    if(message->report_completed) {
      message->report_completed(message);
    }
    message_free(message);
  }
}

void message_postinbox(Agent* dst, Message* message) {
  dst->inbox.add_head(message);
}

void message_postoutbox(Agent* src, Message* message, ReportCompleted report_completed) {
  message->report_completed = report_completed;
  src->outbox.add_head(message);
}

void messages_drop(MessageDLL* list) {
  Message* message = list->remove_tail();
  while(message) {
    message_free(message);
    message = list->remove_tail();
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

  int agent_terminating = 0;

  agent->outbox.foreach([=,&agent_terminating](Message* message) -> int {
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
      return 0;
    });
}

void agent_terminate_report_complete(Message* message) {
  // now we can free ourselves
  Agent* agent = message->source;
  SAFETY(agent->state = ENEMY_MAX);
  delete agent;
}

void foreach_inboxmessage(Agent* agent, InboxMessageCallback callback, void * udata) {
  Message* message = agent->inbox.remove_tail();

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
    message = agent->inbox.remove_tail();
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
  collective->children.add_head(agent);
  agent->delta_subscribers += 1;
}

void collective_remove(Collective* collective, Agent* agent) {
  collective->children.remove(agent);
  agent->delta_subscribers -= 1;
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

  // update all of our sub-agents
  this->children.foreach([=](Agent* agent) -> int {
      if(!(agent->ttag & TAG_SKIP)) {
        agent->update(dt);
      }
      return 0;
    });

  // drain the outboxes of our dispatchees
  this->children.foreach([this](Agent* agent) -> int {
      int removed = 0;
      agent->outbox.foreach([&](Message* message) -> int {
          if(message->kind == MESSAGE_TERMINATING) {
            collective_remove(this, agent);
          }
          message_report_read(message);
          return 0;
        });
      return 0;
    });
}
