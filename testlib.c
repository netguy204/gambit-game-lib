#include <math.h>
#include <stdarg.h>

#include "testlib.h"
#include "testlib_internal.h"
#include "stb_image.h"


ThreadBarrier render_barrier;
FixedAllocator clock_allocator;
FixedAllocator image_resource_allocator;
StackAllocator frame_allocator;
FixedAllocator command_allocator;
Queue render_queue;

int gambit_running;

static pthread_t renderer_thread;

void process_render_command() {
  Command command = command_dequeue(render_queue);
  command->function(command->data);
  command_free(command);
}

static int renderer_running = 0;
void* renderer_exec(void* empty) {
  while(renderer_running) {
    process_render_command();
  }
  return NULL;
}

void lib_init() {
  clock_allocator = fixed_allocator_make(sizeof(struct Clock_), MAX_NUM_CLOCKS, "clock_allocator");
  image_resource_allocator = fixed_allocator_make(sizeof(struct ImageResource_), MAX_NUM_IMAGES, "image_resource_allocator");
  frame_allocator = stack_allocator_make(1024 * 1024, "frame_allocator");
  command_allocator = fixed_allocator_make(sizeof(struct Command_), MAX_NUM_COMMANDS, "command_allocator");
  render_queue = queue_make();
  render_barrier = threadbarrier_make(2);

  renderer_running = 1;
  pthread_create(&renderer_thread, NULL, renderer_exec, NULL);

  Command init = command_make(renderer_init, NULL);
  command_enqueue(render_queue, init);

  if(gambit_running) scm_init();
}

extern void gambit_cleanup();
void notify_gambit_terminated() {
  gambit_running = 0;
}

void render_loop_exit(void* empty) {
  renderer_running = 0;
}

void lib_shutdown() {
  images_free();
  Command command = command_make(renderer_shutdown, NULL);
  command_enqueue(render_queue, command);
  
  command = command_make(render_loop_exit, NULL);
  command_enqueue(render_queue, command);

  threadbarrier_wait(render_barrier);
  gambit_cleanup();
  at_exit();
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

  Command command = command_make(renderer_finish_image_load, resource);
  command_enqueue(render_queue, command);

  return resource;
}

void images_free() {
  LLNode head = last_resource;
  LLNode next;
  while(head) {
    ImageResource resource = (ImageResource)head;
    Command command = command_make(renderer_finish_image_free,
                                   resource->texture);
    command_enqueue(render_queue, command);

    next = head->next;
    fixed_allocator_free(image_resource_allocator, resource);
    head = next;
  }
  last_resource = NULL;
  if(gambit_running) resources_released();
}

/* portable implementation */
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
  Command command = command_make(spritelist_render_to_screen, list);
  command_enqueue(render_queue, command);
}

Command command_make_(CommandFunction function, void* data) {
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

