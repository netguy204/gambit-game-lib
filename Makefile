C_SRC=testlib.c
SCM_SRC=scmlib.scm
GSC=gsc

GAMBIT_ROOT=/usr/local/Gambit-C
CFLAGS=-I$(GAMBIT_ROOT)/include
SDL_LIBS=`pkg-config --libs sdl`
LDFLAGS=$(SDL_LIBS) -L$(GAMBIT_ROOT)/lib

SCM_VERSION ?= 1

all: sdlmain

SCMLIB=scmlib.o$(SCM_VERSION)
SCM_C=$(patsubst %.scm,%.c,$(SCM_SRC)) \
	$(patsubst %.scm,%_.c,$(SCM_SRC))

SCM_OBJ=$(patsubst %.c,%.o,$(SCM_C))
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

$(SCM_C): $(SCM_SRC)
	$(GSC) -f -link $(SCM_SRC)

$(SCM_OBJ): $(SCM_C)
	$(GSC) -cc-options "-D___DYNAMIC" -obj $(SCM_C)

sdlmain: sdlmain.c $(SCM_OBJ) $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(C_OBJS) $(SCM_OBJ) $(LDFLAGS) -lgambc

clean:
	rm -f *.o* $(SCM_C) sdlmain

.phony: all
