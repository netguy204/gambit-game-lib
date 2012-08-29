#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_opengl.h>

#include <math.h>

#include "testlib.h"

#define OFFSET(idx, obj_size, ptr) ((void*)(((char*)ptr) + (idx * obj_size)))
#define NEXT_ALIGNED_SIZE(x) ((x + 8 - 1) & ~(8 - 1))
#define SAFETY(x) x

static FixedAllocator clock_allocator;
static FixedAllocator image_resource_allocator;
static StackAllocator frame_allocator;
static FixedAllocator command_allocator;
static Queue render_queue;
static ThreadBarrier render_barrier;
static pthread_t renderer_thread;
static SDL_Surface* screen = NULL;

void* fail_exit(char * message) {
  fprintf(stderr, "FAIL_EXIT: %s\n", message);
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
  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
    exit(1);
  }
  IMG_Init(IMG_INIT_PNG);

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  screen = SDL_SetVideoMode(640, 480, 16, SDL_OPENGL);
  if(screen == NULL) {
    fprintf(stderr, "Unable to set 640x480 video: %s\n", SDL_GetError());
    exit(1);
  }


  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClearColor(0.8f, 0.8f, 0.8f, 0.0f);
  glViewport(0, 0, 640, 480);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0.0f, 640, 0.0f, 480.0f, -1.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  while(1) {
    process_render_command();
  }
}

void lib_init() {
  clock_allocator = fixed_allocator_make(sizeof(struct Clock_), MAX_NUM_CLOCKS);
  image_resource_allocator = fixed_allocator_make(sizeof(struct ImageResource_), MAX_NUM_IMAGES);
  frame_allocator = stack_allocator_make(1024 * 1024);
  command_allocator = fixed_allocator_make(sizeof(struct Command_), MAX_NUM_COMMANDS);
  render_queue = queue_make();
  render_barrier = threadbarrier_make(2);
  pthread_create(&renderer_thread, NULL, renderer_exec, NULL);

  scm_init();
}

void renderer_begin_frame(void* empty) {
  glClear(GL_COLOR_BUFFER_BIT);
}

void enqueue_begin_frame() {
  Command command = command_make(renderer_begin_frame, NULL);
  command_enqueue(render_queue, command);
}

void begin_frame() {
  stack_allocator_freeall(frame_allocator);
  enqueue_begin_frame();
}

static void signal_render_complete(void* empty) {
  SDL_GL_SwapBuffers();
  threadbarrier_wait(render_barrier);
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

void renderer_finish_image_load(ImageResource resource) {
  GLuint texture;
  GLenum texture_format;
  GLint num_colors;
  SDL_Surface *surface = resource->surface;
  resource->surface = NULL;

  num_colors = surface->format->BytesPerPixel;
  if(num_colors == 4) {
    if(surface->format->Rmask == 0x000000ff) {
      texture_format = GL_RGBA;
    } else {
      texture_format = GL_BGRA;
    }
  } else {
    if (surface->format->Rmask == 0x000000ff) {
      texture_format = GL_RGB;
    } else {
      texture_format = GL_BGR;
    }
  }

  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, num_colors, surface->w, surface->h, 0,
               texture_format, GL_UNSIGNED_BYTE, surface->pixels);

  resource->texture = texture;

  SDL_FreeSurface(surface);
}

ImageResource image_load(char * file) {
  SDL_Surface *surface;

  surface = IMG_Load(file);
  if(surface == NULL) {
    fprintf(stderr, "failed to load %s\n", file);
    return NULL;
  }

  ImageResource resource = (ImageResource)fixed_allocator_alloc(image_resource_allocator);
  resource->w = surface->w;
  resource->h = surface->h;
  resource->node.next = last_resource;
  resource->surface = surface;
  last_resource = (LLNode)resource;

  Command command = command_make((CommandFunction)renderer_finish_image_load,
                                 resource);
  command_enqueue(render_queue, command);

  return resource;
}

int image_width(ImageResource resource) {
  return resource->w;
}

int image_height(ImageResource resource) {
  return resource->h;
}

static void renderer_finish_image_free(void* texturep) {
  GLuint texture = *(GLuint*)texturep;
  glDeleteTextures(1, &texture);
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

void image_render_to_screen(ImageResource img, float angle,
                            float cx, float cy,
                            float x, float y) {
  glBindTexture(GL_TEXTURE_2D, img->texture);
  glPushMatrix();
  glScalef(0.5, 0.5, 0.5);
  
  glTranslatef(x, y, 0.0f);

  glRotatef(angle, 0.0f, 0.0f, 1.0f);
  //glTranslatef(-2.0f * cx, -2.0f * cy, 0.0f);
  glTranslatef(-cx, -cy, 0.0f);

  glBegin(GL_QUADS);
  
  glTexCoord2i(0, 1);
  glVertex3f(0, 0, 0.0f);

  glTexCoord2i(1, 1);
  glVertex3f(img->w, 0, 0.0f);

  glTexCoord2i(1, 0);
  glVertex3f(img->w, img->h, 0.0f);

  glTexCoord2i(0, 0);
  glVertex3f(0, img->h, 0.0f);

  glEnd();

  glPopMatrix();
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

