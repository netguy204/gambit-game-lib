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
void image_render_to_screen(ImageResource img, float angle,
                            float cx, float cy,
                            float x, float y);

/* internal data structures */
ThreadBarrier render_barrier;

#endif
