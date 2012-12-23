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

  DLLNode_();
};

class SimpleDLL {
 public:
  SimpleDLL();

  DLLNode head;
  DLLNode tail;

  void add_head_node(DLLNode node);
  void remove_node(DLLNode node);
  DLLNode remove_tail_node();

  int count();
  void zero();
  int is_empty();
  void insert_after(DLLNode target, DLLNode addition);
  void insert_before(DLLNode target, DLLNode addition);
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

  /*
  void insert_after(E* _target, E* _addition) {
    DLLNode target = to_node(_target);
    DLLNode addition = to_node(_addition);
    insert_after(target, addition);
  }

  void insert_before(E* _target, E* _addition) {
    DLLNode target = to_node(_target);
    DLLNode addition = to_node(_addition);
    insert_before(target, addition);
  }
  */

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

  DLLNode to_node(E* element) {
    return (DLLNode)((char*)element + OFFSET);
  }

  E* to_element(DLLNode node) {
    return (E*)((char*)node - OFFSET);
  }

};

#define DLL_DECLARE(TYPE, LINKNAME) DLL<TYPE, offsetof(TYPE, LINKNAME)>

#endif
