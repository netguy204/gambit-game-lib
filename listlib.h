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
    DLLNode addition = to_node(element);
    add_head_node(addition);
  }

  void remove(E* element) {
    DLLNode node = to_node(element);
    remove_node(node);
  }

  E* remove_tail() {
    DLLNode node = remove_tail_node();
    return to_element(node);
  }

  int next(E** element) {
    if(*element == NULL) {
      *element = to_element(this->head);
      return *element != NULL;
    }

    DLLNode node = to_node(*element);
    DLLNode next = node->next;
    if(next) {
      *element = to_element(next);
      return 1;
    } else {
      *element = NULL;
      return 0;
    }
  }

  inline DLLNode to_node(E* element) {
    return (DLLNode)((char*)element + OFFSET);
  }

  inline E* to_element(DLLNode node) {
    return (E*)((char*)node - OFFSET);
  }

  template<typename Func>
  void foreach(Func func) {
    DLLNode node = this->head;
    while(node) {
      DLLNode next = node->next;
      if(func(to_element(node))) return;
      node = next;
    }
  }
};

#define DLL_DECLARE(TYPE, LINKNAME) DLL<TYPE, offsetof(TYPE, LINKNAME)>

#endif
