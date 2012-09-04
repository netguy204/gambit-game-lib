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

