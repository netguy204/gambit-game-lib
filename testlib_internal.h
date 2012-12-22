#ifndef TESTLIB_INTERNAL_H
#define TESTLIB_INTERNAL_H

#include "threadlib.h"

/* happens on the renderer thread. */
void renderer_init(void* empty);
void renderer_shutdown(void* empty); // barrier
void renderer_begin_frame(void* empty);
void signal_render_complete(void* empty); // barrier
void renderer_finish_image_load(ImageResource resource);
void renderer_finish_image_free(void* texturep);
void sprite_render_to_screen(Sprite sprite);

void at_exit();

/* internal data structures */
extern ThreadBarrier render_barrier;

#endif
