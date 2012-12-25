#ifndef LISTLIB_H
#define LISTLIB_H

#include <assert.h>
#include <stddef.h>

void listlib_init();

typedef struct LLNode_* LLNode;

struct LLNode_ {
  LLNode next;
};

int ll_count(LLNode node);

typedef struct LLEntry_ {
  struct LLNode_ node;
  void* data;
} *LLEntry;

void llentry_add(LLNode* head, void* value);
void* llentry_nextvalue(LLNode* current);
void llentry_remove(LLNode* head, void* value);
void llentry_free(LLEntry entry);

typedef struct DLLNode_ *DLLNode;

struct DLLNode_ {
  DLLNode next;
  DLLNode prev;
};

class SimpleDLL {
 public:
  SimpleDLL();

  DLLNode head;
  DLLNode tail;
  int nelems;

  void add_head_node(DLLNode node);
  void add_tail_node(DLLNode node);
  void remove_node(DLLNode node);
  DLLNode remove_tail_node();

  int count();
  void zero();
  int is_empty();
  void insert_after(DLLNode target, DLLNode addition);
  void insert_before(DLLNode target, DLLNode addition);

  template<typename Func>
  void foreach_node(Func func) {
    DLLNode node = this->head;
    while(node) {
      DLLNode next = node->next;
      if(func(node)) return;
      node = next;
    }
  }
};

template<typename E, int OFFSET>
class DLL : public SimpleDLL {
 public:

  void add_head(E* element) {
    DLLNode addition = DLL<E,OFFSET>::to_node(element);
    add_head_node(addition);
  }

  void add_tail(E* element) {
    DLLNode addition = DLL<E,OFFSET>::to_node(element);
    add_tail_node(addition);
  }

  void remove(E* element) {
    DLLNode node = DLL<E,OFFSET>::to_node(element);
    remove_node(node);
  }

  E* remove_tail() {
    DLLNode node = remove_tail_node();
    return DLL<E,OFFSET>::to_element(node);
  }

  E* head_element() {
    if(!this->head) return NULL;
    return DLL<E,OFFSET>::to_element(this->head);
  }

  E* tail_element() {
    if(!this->tail) return NULL;
    return DLL<E,OFFSET>::to_element(this->tail);
  }

  DLLNode to_node(E* element) {
    return (DLLNode)((char*)element + OFFSET);
  }

  E* to_element(DLLNode node) {
    return (E*)((char*)node - OFFSET);
  }

  template<typename Func>
  void foreach(Func func) {
    DLLNode node = this->head;
    while(node) {
      DLLNode next = node->next;
      if(func(DLL<E,OFFSET>::to_element(node))) return;
      node = next;
    }
  }

  template<typename Func>
  void insert_before_when(E* element, Func func) {
    E* h = head_element();

    if(!h || func(h)) {
      add_head(element);
      return;
    }

    int found = 0;
    foreach([=, &found](E* e) -> int {
        if(e != h && func(e)) {
          insert_before(DLL<E,OFFSET>::to_node(e), DLL<E,OFFSET>::to_node(element));
          found = 1;
          return 1;
        }
        return 0;
      });

    if(!found) {
      add_tail(element);
    }
  }
};

#define DLL_DECLARE(TYPE, LINKNAME) DLL<TYPE, offsetof(TYPE, LINKNAME)>

#endif
