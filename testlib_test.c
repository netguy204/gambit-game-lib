#include "memory.h"
#include "testcase.h"
#include "agent.h"
#include "game.h"

int main(int argc, char ** argv) {
  int ii;

  FixedAllocator fa = fixed_allocator_make(sizeof(long), 100, "fa");
  void* last;
  for(ii=0; ii < 100; ++ii) {
    ASSERT((last = fixed_allocator_alloc(fa)) != NULL);
  }

  //ASSERT(fixed_allocator_alloc(fa) == NULL);
  fixed_allocator_free(fa, last);
  ASSERT(fixed_allocator_alloc(fa) != NULL);

  StackAllocator sa = stack_allocator_make(sizeof(long) * 100, "sa");
  for(ii=0; ii < 100; ++ii) {
    ASSERT((last = stack_allocator_alloc(sa, sizeof(long))) != NULL);
  }

  //ASSERT(stack_allocator_alloc(sa, sizeof(long)) == NULL);
  stack_allocator_freeall(sa);
  ASSERT(stack_allocator_alloc(sa, sizeof(long) * 5) != NULL);

  CircularBuffer buffer = circularbuffer_make(100);
  ASSERT(circularbuffer_bytes_writable(buffer) == 100);
  ASSERT(circularbuffer_bytes_readable(buffer) == 0);

  char bytes[100];
  for(ii = 0; ii < 100; ++ii) {
    bytes[ii] = ii;
  }

  circularbuffer_insert(buffer, bytes, 100);
  ASSERT(circularbuffer_bytes_writable(buffer) == 0);
  ASSERT(circularbuffer_bytes_readable(buffer) == 100);

  char morebytes[100];
  circularbuffer_read(buffer, morebytes, 50);
  ASSERT(circularbuffer_bytes_readable(buffer) == 50);
  ASSERT(circularbuffer_bytes_writable(buffer) == 50);
  for(int ii = 0; ii < 50; ++ii) {
    ASSERT(morebytes[ii] == ii);
  }

  circularbuffer_insert(buffer, bytes, 50);
  ASSERT(circularbuffer_bytes_writable(buffer) == 0);
  ASSERT(circularbuffer_bytes_readable(buffer) == 100);

  circularbuffer_read(buffer, morebytes, 100);
  ASSERT(circularbuffer_bytes_writable(buffer) == 100);
  ASSERT(circularbuffer_bytes_readable(buffer) == 0);
  for(int ii = 0; ii < 100; ++ii) {
    if(ii < 50) {
      ASSERT(morebytes[ii] == ii + 50);
    } else {
      ASSERT(morebytes[ii] == ii - 50);
    }
  }

  testlib_init();
  screen_width = 640;
  screen_height = 480;

  game_init();
  agent_init();

  Particle particle = particle_make();
  Enemy enemy = enemy_make((Particle)particle, 100);

  ASSERT(enemy->agent.inbox.head == NULL);
  ASSERT(enemy->agent.outbox.head == NULL);

  agent_update((Agent)enemy);
  ASSERT(enemy->agent.state == ENEMY_IDLE);

  ASSERT(enemy->agent.inbox.head == NULL);
  ASSERT(enemy->agent.outbox.head == NULL);

  Message terminate = message_make(NULL, MESSAGE_TERMINATE, NULL);
  message_postinbox((Agent)enemy, terminate);

  agent_update((Agent)enemy);

  ASSERT(enemy->agent.inbox.head == NULL);
  ASSERT(enemy->agent.outbox.head != NULL);

  Message reply = (Message)dll_remove_tail(&enemy->agent.outbox);
  ASSERT(reply->kind == MESSAGE_TERMINATING);
  ASSERT(enemy->agent.state != ENEMY_MAX);
  message_report_read(reply);
  ASSERT(enemy->agent.state == ENEMY_MAX);

  // now use a collective
  Collective collective = collective_make();
  Agent ca = (Agent)collective;

  ASSERT(ca->inbox.head == NULL);
  ASSERT(ca->outbox.head == NULL);
  ASSERT(dll_count(&collective->children) == 1);

  Dispatcher dispatcher = (Dispatcher)collective->children.head;
  ASSERT(dll_count(&dispatcher->dispatchees) == 0);

  agent_update((Agent)collective);

  ASSERT(ca->inbox.head == NULL);
  ASSERT(ca->outbox.head == NULL);
  ASSERT(dll_count(&collective->children) == 1);

  Message spawn = message_make(NULL, COLLECTIVE_SPAWN_ENEMY, NULL);
  message_postinbox((Agent)collective, spawn);

  agent_update((Agent)collective);

  ASSERT(ca->inbox.head == NULL);
  ASSERT(ca->outbox.head == NULL);
  ASSERT(dll_count(&collective->children) == 2);
  ASSERT(dll_count(&dispatcher->dispatchees) == 1);

  Dispatchee entry = (Dispatchee)dispatcher->dispatchees.head;
  enemy = (Enemy)entry->agent;
  Particle ep = enemy->visual;

  // park a bullet on top of the enemy and see if the notifications
  // flow
  struct ImageResource_ image;
  image.w = 128;
  image.h = 128;

  Particle bullet =
    (Particle)spawn_bullet(&ep->pos, &ep->vel, &image);
  dll_add_head(&player_bullets, (DLLNode)bullet);

  // current update strategy takes two cycles to push through the
  // deletion
  agent_update((Agent)collective);
  agent_update((Agent)collective);

  ASSERT(dll_count(&collective->children) == 1);
  ASSERT(dll_count(&dispatcher->dispatchees) == 0);

  END_MAIN();
}
