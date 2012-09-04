#ifndef LISTLIB_H
#define LISTLIB_H

typedef struct LLNode_* LLNode;

#define LL_FOREACH(type, var, head) for(type var = head; var != NULL; var = (type)(((LLNode)var)->next))

struct LLNode_ {
  LLNode next;
};

typedef struct DLLNode_ *DLLNode;

struct DLLNode_ {
  DLLNode next;
  DLLNode prev;
};

void llnode_insert_after(DLLNode target, DLLNode addition);
void llnode_insert_before(DLLNode target, DLLNode addition);
void llnode_remove(DLLNode node);

#define INSERT_AFTER(target, addition) \
  llnode_insert_after((DLLNode)target, (DLLNode)addition)

#define INSERT_BEFORE(target, addition) \
  llnode_insert_before((DLLNode)target, (DLLNode)addition)

#define REMOVE(node) \
  llnode_remove((DLLNode)node)



#endif
