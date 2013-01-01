#include "testlib.h"
#include "testlib_internal.h"
#include "stb_image.h"

#include <math.h>
#include <stdarg.h>
#include <string.h>

ThreadBarrier render_barrier;
FixedAllocator clock_allocator;
FixedAllocator image_resource_allocator;
StackAllocator frame_allocator;
CommandQueue* render_queue;
AllocatorQueue* render_reply_queue;

uint32_t screen_width;
uint32_t screen_height;

float screen_x_br = 0.0f;
float screen_y_br = 0.0f;

void screen_rect(Rect rect) {
  rect->minx = screen_x_br;
  rect->miny = screen_y_br;
  rect->maxx = screen_x_br + screen_width;
  rect->maxy = screen_y_br + screen_height;
}

static pthread_t renderer_thread;

void process_render_command() {
  Command command = render_queue->dequeue();
  command->function(command->data);
}

static int renderer_running = 0;
void* renderer_exec(void* empty) {
  while(renderer_running) {
    process_render_command();
  }
  return NULL;
}

void renderer_await_startup(void* empty) {
  threadbarrier_wait(render_barrier);
}

void testlib_init() {
  listlib_init();
  clock_allocator = fixed_allocator_make(sizeof(struct Clock_), MAX_NUM_CLOCKS, "clock_allocator");
  image_resource_allocator = fixed_allocator_make(sizeof(struct ImageResource_), MAX_NUM_IMAGES, "image_resource_allocator");
  frame_allocator = stack_allocator_make(1024 * 1024, "frame_allocator1");

  render_queue = new CommandQueue();
  render_reply_queue = new AllocatorQueue();

  // get our offscreen allocator primed
  render_reply_queue->enqueue(stack_allocator_make(1024 * 1024, "frame_allocator2"));

  render_barrier = threadbarrier_make(2);
}

void lib_init() {
  testlib_init();

  native_init();

  renderer_running = 1;
  pthread_create(&renderer_thread, NULL, renderer_exec, NULL);

  // let the renderer finish init
  renderer_enqueue_sync(renderer_init, NULL);

  // kick off the audio system
  audio_init();
}

void render_loop_exit(void* empty) {
  renderer_running = 0;
  threadbarrier_wait(render_barrier);
}

void lib_shutdown() {
  images_free();
  renderer_enqueue(renderer_shutdown, NULL);
  renderer_enqueue_sync(render_loop_exit, NULL);
  at_exit();
}

void begin_frame() {
  stack_allocator_freeall(frame_allocator);
  renderer_enqueue(renderer_begin_frame, NULL);
}

void* frame_alloc(size_t bytes) {
  return stack_allocator_alloc(frame_allocator, bytes);
}

void end_frame() {
  renderer_enqueue(signal_render_complete, frame_allocator);
  frame_allocator = render_reply_queue->dequeue();
}

static DLL_DECLARE(ImageResource_, node) image_resources;

int image_width(ImageResource resource) {
  return resource->w;
}

int image_height(ImageResource resource) {
  return resource->h;
}

ImageResource image_load(const char * file) {
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
  resource->data = data;
  image_resources.add_head(resource);
  renderer_enqueue(renderer_finish_image_load, resource);

  return resource;
}

void image_free(ImageResource resource) {
  renderer_enqueue(renderer_finish_image_free,
                   resource->texture);
  image_resources.remove(resource);
  fixed_allocator_free(image_resource_allocator, resource);
}

void images_free() {
  image_resources.foreach([](ImageResource resource) -> int {
      image_free(resource);
      return 0;
    });
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
  Sprite sprite = (Sprite)frame_alloc(sizeof(struct Sprite_));
  sprite->angle = 0.0f;
  sprite->originX = 0.0f;
  sprite->originY = 0.0f;

  sprite->u0 = 0.0f;
  sprite->v0 = 1.0f;
  sprite->u1 = 1.0f;
  sprite->v1 = 0.0f;

  sprite->w = 100;
  sprite->h = 100;
  return sprite;
}

SpriteList frame_spritelist_append(SpriteList rest, Sprite sprite) {
  SpriteList list = (SpriteList)frame_alloc(sizeof(struct SpriteList_));
  list->node.next = (LLNode)rest;
  list->sprite = sprite;

  if(rest) {
    list->count = rest->count + 1;
  } else {
    list->count = 1;
  }

  return list;
}

extern void spritelist_render_to_screen(SpriteList list);

void spritelist_enqueue_for_screen(SpriteList list) {
  renderer_enqueue(spritelist_render_to_screen, list);
}

extern void spritelist_render_to_screen_colored(SpriteList list);

void spritelist_enqueue_for_screen_colored(SpriteList list) {
  renderer_enqueue(spritelist_render_to_screen_colored, list);
}

extern void rect_render_to_screen(ColoredRect rect);

void rect_enqueue_for_screen(ColoredRect rect) {
  renderer_enqueue(rect_render_to_screen, rect);
}

extern void filledrect_render_to_screen(ColoredRect rect);

void filledrect_enqueue_for_screen(ColoredRect rect) {
  renderer_enqueue(filledrect_render_to_screen, rect);
}

Command command_make(CommandFunction function, void* data) {
  Command command = (Command)frame_alloc(sizeof(Command_));
  command->node.next = NULL;
  command->node.prev = NULL;
  command->function = function;
  command->data = data;
  return command;
}

void command_async(CommandQueue* queue, CommandFunction function, void* data) {
  Command command = command_make(function, data);
  queue->enqueue(command);
}

static void command_sync_function(ThreadBarrier b) {
  threadbarrier_wait(b);
}

void command_sync(CommandQueue* queue, ThreadBarrier b,
                  CommandFunction function, void* data) {
  command_async(queue, function, data);
  command_async(queue, (CommandFunction)command_sync_function, b);
  threadbarrier_wait(b);
}
