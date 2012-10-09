#ifndef ITEMS_H
#define ITEMS_H

#include "agent.h"

#include <stddef.h>

#define ITEM_MAX_NAME 12

typedef enum {
  HEAT,
  POWER,
  FUEL,
  SPACE,
  MAX_RESOURCE
} Resource;

char* resource_names[MAX_RESOURCE] = {
  "heat",
  "power",
  "fuel",
  "space"
};

typedef enum {
  FIRE,
  IMPACT,
  MAX_ACTIVATION
} Activation;

typedef float Resources_[MAX_RESOURCE];
typedef float* Resources;

void resources_zero(Resources attr);

/* update cycle:
 * - consumers request from producers and producers reply until depleted
 * - producers release remaining mandatory productions to the system
 */

struct SystemInstance_;
struct ComponentInstance_;

typedef void(*UpdateComponent)(struct ComponentInstance_*);
typedef void(*ActivateComponent)(struct ComponentInstance_*, Activation activation);
typedef int(*ComponentPullResource)(struct ComponentInstance_*, Resources resources);
typedef int(*ComponentPushResource)(struct ComponentInstance_*, Resources resources);
typedef char ComponentName[ITEM_MAX_NAME];

typedef struct Stats_ {
  Resources_ storage;
  Resources_ max_capacity;
  Resources_ production_rates;
} *Stats;

typedef struct ComponentClass_ {
  struct DLLNode_ node; // keeps us in the class registry
  LLNode subcomponents; // non-intrusive list of subcomponents
  Resources_ requirements;
  UpdateComponent update;
  ActivateComponent activate;
  ComponentPushResource push;
  ComponentPullResource pull;
  struct Stats_ stats;
  float quality;
  ComponentName name;
} *ComponentClass;

void items_init();

ComponentClass componentclass_make_(char *name, size_t size);
#define componentclass_make(name, klass) (klass*)componentclass_make_(name, sizeof(klass))

ComponentClass componentclass_find(char *name);

struct ComponentInstance_;

typedef struct ComponentInstance_ {
  ComponentClass klass;
  struct DLLNode_ node; // siblings
  struct ComponentInstance_* parent;
  struct DLL_ children;
  struct Stats_ stats;
  float quality;
} *ComponentInstance;

ComponentInstance componentinstance_make(ComponentClass klass);
void componentinstance_free(ComponentInstance instance);

void componentinstance_addchild(ComponentInstance parent, ComponentInstance child);
void componentinstance_removechild(ComponentInstance child);

int component_push(ComponentInstance comp, Resources resources);
int component_pull(ComponentInstance comp, Resources resources);
void component_update(ComponentInstance comp);
void component_activate(ComponentInstance comp, Activation activation);

#define componentinstance_from_node(n) container_of(n, struct ComponentInstance_, node)

#endif
