#include "listlib.h"
#include "memlib.h"
#include "utils.h"
#include "config.h"

#include <assert.h>

FixedAllocator llentry_allocator;

void listlib_init() {
  llentry_allocator = fixed_allocator_make(sizeof(struct LLEntry_), MAX_NUM_LLENTRY,
                                           "llentry_allocator");
}

int ll_count(LLNode node) {
  int ii = 0;
  while(node) {
    ii++;
    node = node->next;
  }
  return ii;
}

LLEntry llentry_make() {
  return (LLEntry)fixed_allocator_alloc(llentry_allocator);
}

void llentry_free(LLEntry entry) {
  fixed_allocator_free(llentry_allocator, entry);
}

void llentry_add(LLNode* head, void* value) {
  LLEntry entry = llentry_make();
  entry->data = value;
  entry->node.next = *head;
  *head = (LLNode)entry;
}

#define ENTRY(v) ((LLEntry)v)
#define ENTRY4P(p) ENTRY(*p)

void* llentry_nextvalue(LLNode* current) {
  if(*current == NULL) return NULL;
  void* data = ENTRY4P(current)->data;
  *current = (*current)->next;
  return data;
}

void llentry_remove(LLNode* head, void* value) {
  assert(head);
  SAFETY(if(*head == NULL) fail_exit("llentry_remove: tried to remove from empty list"));
  assert(*head);

  // is it the head?
  if(ENTRY4P(head)->data == value) {
    LLEntry oldentry = ENTRY4P(head);
    *head = (*head)->next;
    llentry_free(oldentry);
    return;
  }

  // find it
  LLNode last = *head;
  LLNode next = last->next;

  while(next) {
    if(ENTRY(next)->data == value) {
      // unlink
      last->next = next->next;
      llentry_free((LLEntry)next);
      return;
    }

    last = next;
    next = next->next;
  }
}

SimpleDLL::SimpleDLL()
  : head(NULL), tail(NULL), nelems(0) {
}

void SimpleDLL::add_head_node(DLLNode addition) {
  if(this->head == NULL) {
    addition->next = NULL;
    addition->prev = NULL;
    this->head = addition;
    this->tail = addition;
    ++this->nelems;
  } else {
    insert_before(this->head, addition);
    this->head = addition;
  }
}

void SimpleDLL::add_tail_node(DLLNode addition) {
  if(this->tail == NULL) {
    addition->next = NULL;
    addition->prev = NULL;
    this->head = addition;
    this->tail = addition;
    ++this->nelems;
  } else {
    insert_after(this->tail, addition);
    this->tail = addition;
  }
}

void SimpleDLL::remove_node(DLLNode node) {
  if(this->head == node) {
    DLLNode next = node->next;
    if(next) {
      next->prev = NULL;
      this->head = node->next;
    } else {
      this->head = NULL;
      this->tail = NULL;
    }

    --this->nelems;
    return;
  }

  if(this->tail == node) {
    this->remove_tail_node();
    return;
  }

  DLLNode after = node->next;
  DLLNode before = node->prev;

  if(after) {
    assert(after->prev == node);
    after->prev = before;
  }

  if(before) {
    assert(before->next == node);
    before->next = after;
  }
  --this->nelems;
}

DLLNode SimpleDLL::remove_tail_node() {
  if (this->tail == NULL) return NULL;

  DLLNode result = this->tail;
  DLLNode before = result->prev;
  if(before) {
    before->next = NULL;
    this->tail = before;
  } else {
    this->head = NULL;
    this->tail = NULL;
  }
  --this->nelems;
  return result;
}

int SimpleDLL::contains_node(DLLNode test) {
  DLLNode node = head;
  while(node) {
    if(node == test) return 1;
    node = node->next;
  }
  return 0;
}

int SimpleDLL::count() {
  return this->nelems;
}

void SimpleDLL::zero() {
  this->head = NULL;
  this->tail = NULL;
  this->nelems = 0;
}

int SimpleDLL::is_empty() {
  return this->head == NULL;
}

void SimpleDLL::insert_after(DLLNode target, DLLNode addition) {
  DLLNode after = target->next;
  target->next = addition;
  addition->prev = target;
  addition->next = after;
  if(after) {
    after->prev = addition;
  }
  ++this->nelems;
}

void SimpleDLL::insert_before(DLLNode target, DLLNode addition) {
  DLLNode before = target->prev;
  target->prev = addition;
  addition->next = target;
  addition->prev = before;
  if(before) {
    before->next = addition;
  }
  ++this->nelems;
}
