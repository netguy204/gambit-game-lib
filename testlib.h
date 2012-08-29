#ifndef TEST_H
#define TEST_H


#define MAX_NUM_CLOCKS 20
#define MAX_NUM_IMAGES 40
#define MAX_NUM_COMMANDS 40

#define USE_SDL

#include <SDL/SDL.h>
#include <SDL/SDL_opengl.h>
#include <pthread.h>

/* initialize the internal allocators for the library. Must be called
   before other functions */
void lib_init();


void begin_frame();
void end_frame();

/* exported by test.scm */
void scm_init();
void step(int);
void terminate();
void resources_released();

/* exported by testlib.c */
typedef struct FixedAllocator_ {
  size_t allocation_size;
  void* first_free;
} *FixedAllocator;

typedef struct StackAllocator_ {
  void* stack_top;
  void* stack_bottom;
  void* stack_max;
} *StackAllocator;

FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n);
void* fixed_allocator_alloc(FixedAllocator allocator);
void fixed_allocator_free(FixedAllocator allocator, void *obj);

StackAllocator stack_allocator_make(size_t stack_size);
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
  GLuint texture;
  SDL_Surface *surface; /* shortlived, internal */
} *ImageResource;

ImageResource image_load(char * file);
int image_width(ImageResource resource);
int image_height(ImageResource resource);
void images_free();
void image_render_to_screen(ImageResource src, float angle,
                            float cx, float cy,
                            float x, float y);

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

typedef void (*CommandFunction)(void*);

typedef struct Command_ {
  struct DLLNode_ node;
  CommandFunction function;
  void *data;
} *Command;

Command command_make(CommandFunction function, void* data);
void command_free(Command command);

typedef struct Queue_ {
  DLLNode head;
  DLLNode tail;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
} *Queue;

Queue queue_make();
void enqueue(Queue queue, DLLNode item);
DLLNode dequeue(Queue queue);

#define command_enqueue(queue, item) enqueue(queue, (DLLNode)item)
#define command_dequeue(queue) (Command)dequeue(queue)

typedef struct ThreadBarrier_ {
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int nthreads;
  int threads_waiting;
} *ThreadBarrier;

ThreadBarrier threadbarrier_make();
void threadbarrier_wait(ThreadBarrier barrier);

#endif
