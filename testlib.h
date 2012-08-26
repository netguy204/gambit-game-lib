#ifndef TEST_H
#define TEST_H


#define MAX_NUM_CLOCKS 20
#define MAX_NUM_IMAGES 20

#define USE_SDL

#ifdef USE_SDL
#include <SDL/SDL.h>
#endif

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
#ifdef USE_SDL
  SDL_Surface* surface;
#endif
} *ImageResource;

ImageResource image_load(char * file);
int image_width(ImageResource resource);
int image_height(ImageResource resource);
void images_free();
void image_render_to_screen(ImageResource src, float angle, float x, float y);

typedef struct Sprite_ {
  ImageResource resource;
  float angle;
  float displayX;
  float displayY;
} *Sprite;

Sprite frame_make_sprite();

typedef struct SpriteList_ {
  struct LLNode_ node;
  Sprite sprite;
} *SpriteList;

SpriteList frame_spritelist_append(SpriteList list, Sprite sprite);

void spritelist_render_to_screen(SpriteList list);

#endif
