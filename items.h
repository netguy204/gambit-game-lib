#ifndef ITEMS_H
#define ITEMS_H

#include "agent.h"
#include "config.h"
#include "updateable.h"

#include <stddef.h>

struct SystemInstance_;
struct ComponentInstance_;

typedef enum {
  PORT_NORTH,
  PORT_SOUTH,
  PORT_EAST,
  PORT_WEST,
  PORT_TOP,
  PORT_BOTTOM,
  PORT_MAX
} PortDirection;

typedef enum {
  ACTIVATION_FIRE,
  ACTIVATION_MAX
} Activation;

typedef struct ComponentPort_ {
  int valid;
  int offsetx;
  int offsety;
  int type;
} *ComponentPort;

struct ActivatableClass {
  struct UpdateableClass_ _;
  void(*activate)(void* self, Activation activation);
};

extern void* ActivatableClass;

struct ComponentClass {
  struct ActivatableClass _;
  struct DLLNode_ node; // keeps us in the class registry
  struct ComponentPort_ ports[PORT_MAX];
};

void activate(void* self, Activation activation);

void items_init();

struct ComponentClass* componentclass_find(char *name);

typedef struct ComponentInstance_ {
  struct Object _;
  struct ComponentInstance_* connected[PORT_MAX];
} *ComponentInstance;

extern void* ComponentClass;
extern void* ComponentObject;

// temporary XML based file loading. I'll come up with something
// better once I've decided this system is a good idea at all.
void items_load_xml(char* filename);

#endif
