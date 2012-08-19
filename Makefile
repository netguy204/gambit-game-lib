C_SRC=testlib.c
SCM_SRC=test.scm
GSC=gsc

GAMBIT_ROOT=/usr/local/Gambit-C
CFLAGS=-I$(GAMBIT_ROOT)/include
SDL_LIBS=`pkg-config --libs sdl`
LDFLAGS=$(SDL_LIBS) -L$(GAMBIT_ROOT)/lib

SCM_VERSION ?= 1

all: scmlib.o$(SCM_VERSION) sdlmain

SCMLIB=scmlib.o$(SCM_VERSION)
SCM_C=$(patsubst %.scm,%.c,$(SCM_SRC))
SCM_OBJ=$(patsubst %.scm,%.o,$(SCM_SRC))
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

$(SCMLIB).c: $(SCM_SRC)
	$(GSC) -link -flat -o $@ $(SCM_SRC)

$(SCMLIB).o: scmlib.o$(SCM_VERSION).c
	$(GSC) -cc-options "-D___DYNAMIC" -obj $(SCM_C) scmlib.o$(SCM_VERSION).c

$(SCMLIB): scmlib.o$(SCM_VERSION).o $(C_OBJS)
	gcc -bundle $(SCM_OBJ) $(SCMLIB).o $(C_OBJS) -o $@

sdlmain: sdlmain.c
	$(CC) $(CFLAGS) -o $@ $< $(C_OBJS) $(SCMLIB).o $(SCM_OBJ) $(LDFLAGS) -lgambc

clean:
	rm -f *.o* $(SCM_C) sdlmain

.phony: all
