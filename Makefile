C_SRC=testlib.c sdlmain.c realmain.c
SCM_LIB_SRC=link.scm
SCM_MOD_SRC=scmlib.scm
GAMBIT_ROOT?=/usr/local/Gambit-C
GSC=$(GAMBIT_ROOT)/bin/gsc
XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=-I$(GAMBIT_ROOT)/include `sdl-config --cflags` $(XML_INCLUDE)
SDL_LIBS:=`sdl-config --libs` -lSDL_image
LDFLAGS=$(SDL_LIBS) -L$(GAMBIT_ROOT)/lib -lxml2

MKMOD=make -f Mkmod
MAKE_XML2=$(MKMOD) SCM_SRC=xml2.scm OUTPUT=xml2 CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)"

all: sdlmain xml2.o1.o

SCM_LIB_C=$(patsubst %.scm,%.c,$(SCM_LIB_SRC)) \
	$(patsubst %.scm,%_.c,$(SCM_LIB_SRC))

SCM_OBJ=$(patsubst %.c,%.o,$(SCM_LIB_C))
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

$(SCM_LIB_C): $(SCM_LIB_SRC)
	$(GSC) -f -link $(SCM_LIB_SRC)

$(SCM_OBJ): $(SCM_LIB_C)
	$(GSC) -cc-options "-D___DYNAMIC $(CFLAGS)" -obj $(SCM_LIB_C)

sdlmain: $(SCM_OBJ) $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) $(SCM_OBJ) $(LDFLAGS) -lgambc

clean:
	rm -f *.o* $(SCM_LIB_C) sdlmain
	$(MAKE_XML2) clean

test_bin: testlib.o testlib_test.o
	$(CC) $(CFLAGS) -o $@ testlib.o testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

xml2.o1.o: xml2.scm
	$(MAKE_XML2)

.phony: all

