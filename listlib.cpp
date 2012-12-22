#include "listlib.h"
#include "memory.h"
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

void llnode_insert_after(DLLNode target, DLLNode addition) {
  DLLNode after = target->next;
  target->next = addition;
  addition->prev = target;
  addition->next = after;
  if(after) {
    after->prev = addition;
  }
}

void llnode_insert_before(DLLNode target, DLLNode addition) {
  DLLNode before = target->prev;
  target->prev = addition;
  addition->next = target;
  addition->prev = before;
  if(before) {
    before->next = addition;
  }
}

void llnode_remove(DLLNode node) {
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
}

void dll_add_head(DLL list, DLLNode addition) {
  if(list->head == NULL) {
    addition->next = NULL;
    addition->prev = NULL;
    list->head = addition;
    list->tail = addition;
  } else {
    INSERT_BEFORE(list->head, addition);
    list->head = addition;
  }
}

DLLNode dll_remove_tail(DLL list) {
  if (list->tail == NULL) return NULL;

  DLLNode result = list->tail;
  DLLNode before = result->prev;
  if(before) {
    before->next = NULL;
    list->tail = before;
  } else {
    list->head = NULL;
    list->tail = NULL;
  }
  return result;
}

void dll_remove(DLL list, DLLNode node) {
  if(list->head == node) {
    DLLNode next = node->next;
    if(next) {
      next->prev = NULL;
      list->head = node->next;
    } else {
      list->head = NULL;
      list->tail = NULL;
    }
    return;
  }

  if(list->tail == node) {
    dll_remove_tail(list);
    return;
  }

  REMOVE(node);
}

int dll_count(DLL list) {
  int count = 0;
  DLLNode node = list->head;
  while(node) {
    ++count;
    node = node->next;
  }
  return count;
}

void dll_zero(DLL list) {
  list->head = NULL;
  list->tail = NULL;
}
