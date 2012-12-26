CPP_SRC=testlib_sdl.cpp audio_sdl.cpp
BIN=sdlmain

SDL_LIBS:=`sdl-config --libs`

PLATFORM:=$(shell uname)
ifeq ($(PLATFORM), Darwin)
	LDFLAGS+= -framework OpenGL
else
	LDFLAGS+= -lGL -lGLEW -lm -ldl -lutil
endif

CFLAGS+=`sdl-config --cflags`
LDFLAGS+=$(SDL_LIBS) $(OPENGL)

include Common.mk

# force include of SDL header so that it can do it's main redirection
# magic
gambitmain.o: gambitmain.cpp
	$(CXX) $(CFLAGS) -c $< -include "SDL/SDL.h"

audio_test: audio_test.cpp sampler.cpp
	gcc -g -o audio_test sampler.cpp audio_test.cpp `sdl-config --libs` `sdl-config --cflags`
