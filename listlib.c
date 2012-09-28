#include "listlib.h"

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
    after->prev = before;
  }

  if(before) {
    before->next = after;
  }
}

#define NULL 0

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
