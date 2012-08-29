C_SRC=threadlib.c testlib.c sdlmain.c realmain.c
SCM_LIB_SRC=link.scm

GAMBIT_ROOT?=/usr/local/Gambit-C
GSC=$(GAMBIT_ROOT)/bin/gsc
XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=-std=c99 -I$(GAMBIT_ROOT)/include `sdl-config --cflags` $(XML_INCLUDE)
SDL_LIBS:=`sdl-config --libs` -lSDL_image

PLATFORM:=$(shell uname)

LDFLAGS=$(SDL_LIBS) -L$(GAMBIT_ROOT)/lib $(OPENGL) -lpthread

ifeq ($(PLATFORM), Darwin)
	LDFLAGS+= -framework OpenGL
else
	LDFLAGS+= -lGL -lm -ldl -lutil
endif

MKMOD=make -f Module.mk
MAKE_XML2=$(MKMOD) SCM_SRC=xml2.scm OUTPUT=xml2 CFLAGS="$(CFLAGS)" LDFLAGS="-lxml2"


SCM_LIB_C=$(patsubst %.scm,%.c,$(SCM_LIB_SRC)) \
	link_.c

SCM_OBJ=$(patsubst %.c,%.o,$(SCM_LIB_C))
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))
SCM_GAMBIT_OBJ=$(patsubst %.scm,%.o1,$(SCM_GAMBIT_SRC))
SCM_R5_OBJ=$(patsubst %.scm,%.o1,$(SCM_R5_SRC))

all: sdlmain xml2.o1.o $(SCM_GAMBIT_OBJ) $(SCM_R5_OBJ)


$(SCM_LIB_C): $(SCM_LIB_SRC)
	$(GSC) -f -link -track-scheme $(SCM_LIB_SRC)

$(SCM_OBJ): $(SCM_LIB_C)
	$(CC) -D___DYNAMIC $(CFLAGS) -c $(SCM_LIB_C)

$(SCM_R5_OBJ): $(SCM_R5_SRC)
	$(GSC) -:s -o $@ $<

$(SCM_GAMBIT_OBJ): $(SCM_GAMBIT_SRC)
	$(GSC) -o $@ $<

sdlmain: $(SCM_OBJ) $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) $(SCM_OBJ) $(LDFLAGS) -lgambc

clean:
	rm -f *.o* $(SCM_LIB_C) sdlmain
	$(MAKE_XML2) clean
	rm -rf sdlmain.app

test_bin: testlib.o testlib_test.o
	$(CC) $(CFLAGS) -o $@ testlib.o testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

xml2.o1.o: xml2.scm
	$(MAKE_XML2)

sdlmain.app: all
	mkdir sdlmain.app

.phony: all

