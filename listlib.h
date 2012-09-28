#ifndef LISTLIB_H
#define LISTLIB_H

typedef struct LLNode_* LLNode;

struct LLNode_ {
  LLNode next;
};

typedef struct DLLNode_ *DLLNode;

struct DLLNode_ {
  DLLNode next;
  DLLNode prev;
};

typedef struct DLL_ {
  DLLNode head;
  DLLNode tail;
} *DLL;

void llnode_insert_after(DLLNode target, DLLNode addition);
void llnode_insert_before(DLLNode target, DLLNode addition);
void llnode_remove(DLLNode node);

void dll_add_head(DLL list, DLLNode addition);
DLLNode dll_remove_tail(DLL list);
void dll_remove(DLL list, DLLNode node);
int dll_count(DLL list);

#define INSERT_AFTER(target, addition) \
  llnode_insert_after((DLLNode)target, (DLLNode)addition)

#define INSERT_BEFORE(target, addition) \
  llnode_insert_before((DLLNode)target, (DLLNode)addition)

#define REMOVE(node) \
  llnode_remove((DLLNode)node)



#endif
