#include <math.h>

#include "testlib.h"

#define OFFSET(idx, obj_size, ptr) ((void*)(((char*)ptr) + (idx * obj_size)))
#define NEXT_ALIGNED_SIZE(x) ((x + 8 - 1) & ~(8 - 1))
#define SAFETY(x) x

static FixedAllocator clock_allocator;
static FixedAllocator image_resource_allocator;
static StackAllocator frame_allocator;

void* fail_exit(char * message) {
  fprintf(stderr, "FAIL_EXIT: %s\n", message);
  fflush(stderr);
  exit(1);
  return NULL;
}

void lib_init() {
  clock_allocator = fixed_allocator_make(sizeof(struct Clock_), MAX_NUM_CLOCKS);
  image_resource_allocator = fixed_allocator_make(sizeof(struct ImageResource_), MAX_NUM_IMAGES);
  frame_allocator = stack_allocator_make(1024 * 1024);

  scm_init();
}

#ifdef USE_SDL
extern SDL_Surface* screen;
#endif

void begin_frame() {
  stack_allocator_freeall(frame_allocator);
#ifdef USE_SDL
  SDL_FillRect(screen, NULL, 0);
#endif
}

void end_frame() {
#ifdef USE_SDL
  SDL_Flip(screen);
#endif
}

/**
 * FixedAllocator's are used to quickly allocate and free objects of
 * fixed size. They opporate in constant time but cannot allocate more
 * objects than they were initially designed to hold. This makes them
 * appropriate for holding things like resource handles (since the
 * number of resources in the system is finite), timelines, and other
 * finite arity and long duration objects.
 */
FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n) {
  int ii;

  /* next 8 byte aligned size */
  obj_size = NEXT_ALIGNED_SIZE(obj_size);

  FixedAllocator allocator = (FixedAllocator)malloc(obj_size * n + 2*sizeof(struct FixedAllocator_));
  allocator->allocation_size = obj_size;

  void* mem = &allocator[1];
  allocator->first_free = NULL;
  for(ii = 0; ii < n; ++ii) {
    *(void**)mem = allocator->first_free;
    allocator->first_free = mem;
    mem += obj_size;
  }

  return allocator;
}

void* fixed_allocator_alloc(FixedAllocator allocator) {
  SAFETY(if(!allocator->first_free) return fail_exit("fixed_allocator failed"));

  void * mem = allocator->first_free;
  allocator->first_free = *(void**)allocator->first_free;
  return mem;
}

void fixed_allocator_free(FixedAllocator allocator, void *obj) {
  *(void**)obj = allocator->first_free;
  allocator->first_free = obj;
}

StackAllocator stack_allocator_make(size_t stack_size) {
  size_t size = sizeof(struct StackAllocator_) * 2 + stack_size;
  size = NEXT_ALIGNED_SIZE(size);

  StackAllocator allocator = (StackAllocator)malloc(size);
  allocator->stack_bottom = &allocator[1];
  allocator->stack_top = allocator->stack_bottom;
  allocator->stack_max = (char*)allocator->stack_top + stack_size;
  return allocator;
}

void* stack_allocator_alloc(StackAllocator allocator, size_t size) {
  size = NEXT_ALIGNED_SIZE(size);
  SAFETY(if((char*)allocator->stack_top + size > (char*)allocator->stack_max) return fail_exit("stack_allocator failed"));
  void* mem = allocator->stack_top;
  allocator->stack_top = (char*)allocator->stack_top + size;
  return mem;
}

void stack_allocator_freeall(StackAllocator allocator) {
  allocator->stack_top = allocator->stack_bottom;
}

Clock clock_make() {
  Clock clock = (Clock)fixed_allocator_alloc(clock_allocator);
  clock->cycles = 0;
  clock->time_scale = 1.0f;
  clock->paused = 0;
  return clock;
}

void clock_free(Clock clock) {
  fixed_allocator_free(clock_allocator, clock);
}

float clock_update(Clock clock, float delta) {
  if(!clock->paused) {
    float scaled = delta * clock->time_scale;
    clock->cycles += clock_seconds_to_cycles(scaled);
    return scaled;
  } else {
    return 0.0f;
  }
}

long clock_time(Clock clock) {
  return clock->cycles;
}

float clock_cycles_to_seconds(long cycles) {
  return cycles / 1000.0f;
}

long clock_seconds_to_cycles(float seconds) {
  return roundf(seconds * 1000.0f);
}

static LLNode last_resource = NULL;

#ifdef USE_SDL
#include <SDL/SDL_image.h>
#include <SDL/SDL_rotozoom.h>

ImageResource image_load(char * file) {
  SDL_Surface *image;
  SDL_Surface *optimized;

  image = IMG_Load(file);
  if(image == NULL) {
    fprintf(stderr, "failed to load %s\n", file);
    return NULL;
  }

  optimized = SDL_DisplayFormatAlpha(image);
  SDL_FreeSurface(image);

  ImageResource resource = (ImageResource)fixed_allocator_alloc(image_resource_allocator);
  resource->surface = optimized;
  resource->node.next = last_resource;
  last_resource = (LLNode)resource;

  return resource;
}

int image_width(ImageResource resource) {
  return resource->surface->w;
}

int image_height(ImageResource resource) {
  return resource->surface->h;
}

void images_free() {
  LLNode head = last_resource;
  LLNode next;
  while(head) {
    ImageResource resource = (ImageResource)head;
    SDL_FreeSurface(resource->surface);
    next = head->next;
    fixed_allocator_free(image_resource_allocator, resource);
    head = next;
  }
  last_resource = NULL;
  resources_released();
}

void image_render_to_screen(ImageResource img, float angle, float x, float y) {
  SDL_Surface* src = img->surface;
  SDL_Rect dest;
  dest.x = (int)roundf(x);
  dest.y = (int)roundf(y);
  dest.w = src->w;
  dest.h = src->h;

  if(angle != 0) {
    // have to make a new surface in SW
    SDL_Surface* new_src = rotozoomSurface(src, angle, 1.0, 1);
    SDL_BlitSurface(new_src, NULL, screen, &dest);
    SDL_FreeSurface(new_src);
  } else {
    SDL_BlitSurface(src, NULL, screen, &dest);
  }
}
#endif

Sprite frame_make_sprite() {
  Sprite sprite = stack_allocator_alloc(frame_allocator, sizeof(struct Sprite_));
  sprite->angle = 0.0f;
  return sprite;
}

SpriteList frame_spritelist_append(SpriteList rest, Sprite sprite) {
  SpriteList list = stack_allocator_alloc(frame_allocator, sizeof(struct SpriteList_));
  list->node.next = (LLNode)rest;
  list->sprite = sprite;
  return list;
}

void spritelist_render_to_screen(SpriteList list) {
  LL_FOREACH(SpriteList, element, list) {
    Sprite sprite = element->sprite;
    image_render_to_screen(sprite->resource, sprite->angle,
                           sprite->displayX, sprite->displayY);
  }
}
