C_SRC+= \
	threadlib.c memory.c listlib.c testlib.c \
	sampler.c audio.c \
	gambitmain.c realmain.c stb_image.c

SCM_LIB_SRC=link.scm

GAMBIT_ROOT?=/usr/local/Gambit-C
GSC=$(GAMBIT_ROOT)/bin/gsc
XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=-I$(GAMBIT_ROOT)/include $(XML_INCLUDE)
LDFLAGS+=-L$(GAMBIT_ROOT)/lib -lpthread

MKMOD=make -f Module.mk
MAKE_XML2=$(MKMOD) SCM_SRC=xml2.scm OUTPUT=xml2 CFLAGS="$(CFLAGS)" LDFLAGS="-lxml2"


SCM_LIB_C=$(patsubst %.scm,%.c,$(SCM_LIB_SRC)) \
	link_.c

SCM_OBJ=$(patsubst %.c,%.o,$(SCM_LIB_C))
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))
SCM_GAMBIT_OBJ=$(patsubst %.scm,%.o1,$(SCM_GAMBIT_SRC))
SCM_R5_OBJ=$(patsubst %.scm,%.o1,$(SCM_R5_SRC))

all: $(BIN) $(SCM_GAMBIT_OBJ) $(SCM_R5_OBJ) xml2.o1.o

scmlib:
	$(GSC) math.scm common.scm scmlib.scm rect.scm spatial.scm

$(SCM_LIB_C): $(SCM_LIB_SRC)
	$(GSC) -f -link -track-scheme $(SCM_LIB_SRC)

$(SCM_OBJ): $(SCM_LIB_C)
	$(CC) -D___DYNAMIC $(CFLAGS) -c $(SCM_LIB_C)

$(SCM_R5_OBJ): $(SCM_R5_SRC)
	$(GSC) -:s -o $@ $<

$(SCM_GAMBIT_OBJ): $(SCM_GAMBIT_SRC)
	$(GSC) -o $@ $<

$(BIN): $(SCM_OBJ) $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) $(SCM_OBJ) $(LDFLAGS) -lgambc

clean:
	rm -rf *.o* $(SCM_LIB_C) $(BIN)
	$(MAKE_XML2) clean

test_bin: memory.o testlib_test.o
	$(CC) $(CFLAGS) -o $@ memory.o testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

xml2.o1.o: xml2.scm
	$(MAKE_XML2)

xml2: xml2.o1.o

.phony: all

