#ifndef ITEMS_H
#define ITEMS_H

#include "agent.h"

#define ITEM_MAX_NAME 12

typedef struct ItemClass_ {
  struct DLLNode_ node;
  struct Agent_ agent;
  char name[ITEM_MAX_NAME];

} *ItemSystem;

#endif
