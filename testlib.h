#ifndef TEST_H
#define TEST_H

/** conventions:
 *
 * Things that happen on the renderer thread are prefixed with
 * renderer_
 *
 * Things that allocate from the per frame memory allocator are
 * prefixed with frame_
 *
 * Functions that deal with objects are prefixed with objectname_
 */

#define MAX_NUM_CLOCKS 20
#define MAX_NUM_IMAGES 40
#define MAX_NUM_COMMANDS 60
#define DEBUG_MEMORY

#include <pthread.h>
#include "threadlib.h"

/* initialize the internal allocators for the library. Must be called
   before other functions */
void lib_init();

/* shuts down the renderer and frees memory. Must be called before
   termination */
void lib_shutdown();

void begin_frame();
void end_frame();

/* exported by test.scm */
void scm_init();
void step(int);
void terminate();
void resources_released();

/* filled by the gambit repl when gambit is shutting down. no more
   calls to gabmit should happen after this is set */
extern int gambit_running;
void notify_gambit_terminated();

typedef struct InputState_ {
  int quit_requested;
} *InputState;

/* provided by system specific lib */
InputState frame_inputstate();
long time_millis();
void sleep_millis(long millis);

/* exported by testlib.c */
typedef struct FixedAllocator_ {
#ifdef DEBUG_MEMORY
  const char* name;
  long inflight;
  long max_inflight;
#endif
  size_t allocation_size;
  void* first_free;
} *FixedAllocator;

typedef struct StackAllocator_ {
#ifdef DEBUG_MEMORY
  const char* name;
#endif
  void* stack_top;
  void* stack_bottom;
  void* stack_max;
} *StackAllocator;

FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n,
                                    const char* name);
void* fixed_allocator_alloc(FixedAllocator allocator);
void fixed_allocator_free(FixedAllocator allocator, void *obj);

StackAllocator stack_allocator_make(size_t stack_size,
                                    const char* name);
void* stack_allocator_alloc(StackAllocator allocator, size_t size);
void stack_allocator_freeall(StackAllocator allocator);

typedef struct Clock_ {
  long cycles; /* msecs */
  float time_scale;
  int paused;
} *Clock;

Clock clock_make();
void clock_free(Clock clock);
float clock_update(Clock clock, float delta); /* time in seconds */
long clock_time(Clock clock); /* time in cycles */

float clock_cycles_to_seconds(long cycles);
long clock_seconds_to_cycles(float seconds);

typedef struct LLNode_* LLNode;

#define LL_FOREACH(type, var, head) for(type var = head; var != NULL; var = (type)(((LLNode)var)->next))

struct LLNode_ {
  LLNode next;
};

typedef struct ImageResource_ {
  struct LLNode_ node;
  int w, h;
  unsigned int texture;
  int channels;
  unsigned char* data; /* shortlived, internal */
} *ImageResource;

ImageResource image_load(char * file);
int image_width(ImageResource resource);
int image_height(ImageResource resource);
void images_free();

typedef struct Sprite_ {
  ImageResource resource;
  float angle;
  float originX;
  float originY;
  float displayX;
  float displayY;
} *Sprite;

Sprite frame_make_sprite();

typedef struct SpriteList_ {
  struct LLNode_ node;
  Sprite sprite;
} *SpriteList;

SpriteList frame_spritelist_append(SpriteList list, Sprite sprite);

void spritelist_enqueue_for_screen(SpriteList list);

typedef void (*CommandFunction)(void*);

typedef struct Command_ {
  struct DLLNode_ node;
  CommandFunction function;
  void *data;
} *Command;

#define command_make(function, data)                    \
  command_make_((CommandFunction)function, (void*)data)

Command command_make_(CommandFunction function, void* data);
void command_free(Command command);

#define command_enqueue(queue, item) enqueue(queue, (DLLNode)item)
#define command_dequeue(queue) (Command)dequeue(queue)

#endif
