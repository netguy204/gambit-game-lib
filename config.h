#ifndef CONFIG_H
#define CONFIG_H

#define SAFETY(x) x
#define DEBUG_MEMORY

#define MAX_NUM_CLOCKS 20
#define MAX_NUM_IMAGES 40
#define MAX_NUM_COMMANDS 60
#define MAX_NUM_LLENTRY 100

#define MAX_NUM_PARTICLES 100
#define MAX_NUM_PRETTYPARTICLES 100

#define MAX_NUM_AGENTS MAX_NUM_PARTICLES
#define MAX_NUM_MESSAGES (MAX_NUM_AGENTS * 2)
#define MAX_NUM_DISPATCHEES (MAX_NUM_AGENTS * 4)

#define SAMPLE_FREQ 22050
#define MAX_NUM_SAMPLERS 128


#define MAX(a,b)                                                \
  ({ __typeof__ (a) _a = (a);                                   \
    __typeof__ (b) _b = (b);                                    \
    _a > _b ? _a : _b; })

#define MIN(a,b)                                                \
  ({ __typeof__ (a) _a = (a);                                   \
    __typeof__ (b) _b = (b);                                    \
    _a < _b ? _a : _b; })

#define SIGN(a) (a > 0 ? 1 : -1)

#include <stddef.h>

// from http://gitorious.org/cmus/cmus/commit/b10c26c8e65686b8e63cafd4231d4ef02079e86b/diffs
/**
 * container_of - cast a member of a structure out to the containing structure
 *
 * @ptr:	the pointer to the member.
 * @type:	the type of the container struct this is embedded in.
 * @member:	the name of the member within the struct.
 *
 */
#define container_of_portable(ptr, type, member)        \
  ((type *)( (char *)(ptr) - offsetof(type,member) ))
#undef container_of
#if defined(__GNUC__)
#define container_of(ptr, type, member) ({                              \
      const __typeof__( ((type *)0)->member ) *__mptr = (ptr);          \
      container_of_portable(__mptr, type, member);})
#else
#define container_of(ptr, type, member) container_of_portable(ptr, type, member)
#endif

#define array_size(x) (sizeof(x) / sizeof(x[0]))

#define NEXT_ALIGNED_SIZE(x) ((x + 8 - 1) & ~(8 - 1))

#define IN_RADIANS(d) (d * M_PI / 180.0f)

#endif
