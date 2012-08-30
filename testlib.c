#include <math.h>
#include <stdarg.h>

#include "testlib.h"
#include "testlib_internal.h"
#include "stb_image.h"

#define OFFSET(idx, obj_size, ptr) ((void*)(((char*)ptr) + (idx * obj_size)))
#define NEXT_ALIGNED_SIZE(x) ((x + 8 - 1) & ~(8 - 1))
#define SAFETY(x) x

ThreadBarrier render_barrier;

static FixedAllocator clock_allocator;
static FixedAllocator image_resource_allocator;
static StackAllocator frame_allocator;
static FixedAllocator command_allocator;
static Queue render_queue;
static pthread_t renderer_thread;

void* fail_exit(const char * message, ...) {
  fprintf(stderr, "FAIL_EXIT: ");

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  fflush(stderr);
  exit(1);
  return NULL;
}

void process_render_command() {
  Command command = command_dequeue(render_queue);
  command->function(command->data);
  command_free(command);
}

void* renderer_exec(void* empty) {
  while(1) {
    process_render_command();
  }
}

void lib_init() {
  clock_allocator = fixed_allocator_make(sizeof(struct Clock_), MAX_NUM_CLOCKS, "clock_allocator");
  image_resource_allocator = fixed_allocator_make(sizeof(struct ImageResource_), MAX_NUM_IMAGES, "image_resource_allocator");
  frame_allocator = stack_allocator_make(1024 * 1024, "frame_allocator");
  command_allocator = fixed_allocator_make(sizeof(struct Command_), MAX_NUM_COMMANDS, "command_allocator");
  render_queue = queue_make();
  render_barrier = threadbarrier_make(2);
  pthread_create(&renderer_thread, NULL, renderer_exec, NULL);

  Command init = command_make(renderer_init, NULL);
  command_enqueue(render_queue, init);

  scm_init();
}

void lib_shutdown() {
  images_free();
  Command command = command_make(renderer_shutdown, NULL);
  command_enqueue(render_queue, command);
  threadbarrier_wait(render_barrier);
}

void enqueue_begin_frame() {
  Command command = command_make(renderer_begin_frame, NULL);
  command_enqueue(render_queue, command);
}

void begin_frame() {
  stack_allocator_freeall(frame_allocator);
  enqueue_begin_frame();
}

void renderer_await_end_of_frame() {
  Command command = command_make(signal_render_complete, NULL);
  command_enqueue(render_queue, command);
  threadbarrier_wait(render_barrier);
}

void end_frame() {
  renderer_await_end_of_frame();
}

static LLNode last_resource = NULL;

int image_width(ImageResource resource) {
  return resource->w;
}

int image_height(ImageResource resource) {
  return resource->h;
}

ImageResource image_load(char * file) {
  int w, h, channels;
  unsigned char *data = stbi_load(file, &w, &h, &channels, 0);

  if(data == NULL) {
    fprintf(stderr, "failed to load %s\n", file);
    return NULL;
  }

  ImageResource resource = (ImageResource)fixed_allocator_alloc(image_resource_allocator);
  resource->w = w;
  resource->h = h;
  resource->channels = channels;
  resource->node.next = last_resource;
  resource->data = data;
  last_resource = (LLNode)resource;

  Command command = command_make((CommandFunction)renderer_finish_image_load,
                                 resource);
  command_enqueue(render_queue, command);

  return resource;
}

void images_free() {
  LLNode head = last_resource;
  LLNode next;
  while(head) {
    ImageResource resource = (ImageResource)head;
    Command command = command_make(renderer_finish_image_free,
                                   (void*)resource->texture);
    command_enqueue(render_queue, command);

    next = head->next;
    fixed_allocator_free(image_resource_allocator, resource);
    head = next;
  }
  last_resource = NULL;
  resources_released();
}

/* portable implementation */

/**
 * FixedAllocator's are used to quickly allocate and free objects of
 * fixed size. They operrate in constant time but cannot allocate more
 * objects than they were initially designed to hold. This makes them
 * appropriate for holding things like resource handles (since the
 * number of resources in the system is finite), timelines, and other
 * finite arity and long duration objects.
 */
FixedAllocator fixed_allocator_make(size_t obj_size, unsigned int n,
                                    const char* name) {
  int ii;

  /* next 8 byte aligned size */
  obj_size = NEXT_ALIGNED_SIZE(obj_size);

  FixedAllocator allocator = (FixedAllocator)malloc(obj_size * n + 2*sizeof(struct FixedAllocator_));
  allocator->name = name;
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
  SAFETY(if(!allocator->first_free) return fail_exit("fixed_allocator %s failed", allocator->name));

  void * mem = allocator->first_free;
  allocator->first_free = *(void**)allocator->first_free;
  return mem;
}

void fixed_allocator_free(FixedAllocator allocator, void *obj) {
  *(void**)obj = allocator->first_free;
  allocator->first_free = obj;
}

StackAllocator stack_allocator_make(size_t stack_size, const char* name) {
  size_t size = sizeof(struct StackAllocator_) * 2 + stack_size;
  size = NEXT_ALIGNED_SIZE(size);

  StackAllocator allocator = (StackAllocator)malloc(size);
  allocator->name = name;
  allocator->stack_bottom = &allocator[1];
  allocator->stack_top = allocator->stack_bottom;
  allocator->stack_max = (char*)allocator->stack_top + stack_size;
  return allocator;
}

void* stack_allocator_alloc(StackAllocator allocator, size_t size) {
  size = NEXT_ALIGNED_SIZE(size);
  SAFETY(if((char*)allocator->stack_top + size > (char*)allocator->stack_max) return fail_exit("stack_allocator %s failed", allocator->name));
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

Sprite frame_make_sprite() {
  Sprite sprite = stack_allocator_alloc(frame_allocator, sizeof(struct Sprite_));
  sprite->angle = 0.0f;
  sprite->originX = 0.0f;
  sprite->originY = 0.0f;
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
                           sprite->originX, sprite->originY,
                           sprite->displayX, sprite->displayY);
  }
}

void spritelist_enqueue_for_screen(SpriteList list) {
  Command command = command_make((CommandFunction)spritelist_render_to_screen, list);
  command_enqueue(render_queue, command);
}

Command command_make(CommandFunction function, void* data) {
  Command command = (Command)fixed_allocator_alloc(command_allocator);
  command->node.next = NULL;
  command->node.prev = NULL;
  command->function = function;
  command->data = data;
  return command;
}

void command_free(Command command) {
  fixed_allocator_free(command_allocator, command);
}

