C_SRC=testlib_sdl.c audio_sdl.c
BIN=sdlmain

SDL_LIBS:=`sdl-config --libs`

PLATFORM:=$(shell uname)
ifeq ($(PLATFORM), Darwin)
	LDFLAGS+= -framework OpenGL
else
	LDFLAGS+= -lGL -lm -ldl -lutil
endif

CFLAGS+=-std=c99 `sdl-config --cflags`
LDFLAGS+=$(SDL_LIBS) $(OPENGL) 

include Common.mk

# force include of SDL header so that it can do it's main redirection
# magic
gambitmain.o: gambitmain.c
	$(CC) $(CFLAGS) -c $< -include "SDL/SDL.h"

audio_test: audio_test.c sampler.c
	gcc -g -o audio_test sampler.c audio_test.c `sdl-config --libs` `sdl-config --cflags`
