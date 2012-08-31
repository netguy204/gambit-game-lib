C_SRC=threadlib.c testlib.c testlib_rpi.c gambitmain.c realmain.c stb_image.c
SCM_LIB_SRC=link.scm

GAMBIT_ROOT?=/usr/local/Gambit-C
GSC=$(GAMBIT_ROOT)/bin/gsc
CFLAGS+=-I$(GAMBIT_ROOT)/include -I/usr/include/libxml2
LDFLAGS+=-L$(GAMBIT_ROOT)/lib $(OPENGL) -lpthread

# from https://github.com/raspberrypi/firmware/blob/master/opt/vc/src/hello_pi/Makefile.include
CFLAGS+=-DSTANDALONE -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS -DTARGET_POSIX -D_LINUX -fPIC -DPIC -D_REENTRANT -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -U_FORTIFY_SOURCE -DHAVE_LIBOPENMAX=2 -DOMX -DOMX_SKIP64BIT -ftree-vectorize -pipe -DUSE_EXTERNAL_OMX -DHAVE_LIBBCM_HOST -DUSE_EXTERNAL_LIBBCM_HOST -DUSE_VCHIQ_ARM -Wno-psabi -I$(SDKSTAGE)/opt/vc/include/ -I$(SDKSTAGE)/opt/vc/include/interface/vcos/pthreads -I./ -I../libs/ilclient -I../libs/vgfont

LDFLAGS+=-L$(SDKSTAGE)/opt/vc/lib/ -lGLESv2 -lEGL -lopenmaxil -lbcm_host -lvcos -lvchiq_arm -L../libs/ilclient -L../libs/vgfont

PLATFORM:=$(shell uname)

ifeq ($(PLATFORM), Darwin)
	LDFLAGS+= -framework OpenGL
else
	LDFLAGS+= -lm -ldl -lutil
endif

MKMOD=make -f Module.mk
MAKE_XML2=$(MKMOD) SCM_SRC=xml2.scm OUTPUT=xml2 CFLAGS="$(CFLAGS)" LDFLAGS="-lxml2"


SCM_LIB_C=$(patsubst %.scm,%.c,$(SCM_LIB_SRC)) \
	link_.c

SCM_OBJ=$(patsubst %.c,%.o,$(SCM_LIB_C))
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))
SCM_GAMBIT_OBJ=$(patsubst %.scm,%.o1,$(SCM_GAMBIT_SRC))
SCM_R5_OBJ=$(patsubst %.scm,%.o1,$(SCM_R5_SRC))

all: pimain xml2.o1.o $(SCM_GAMBIT_OBJ) $(SCM_R5_OBJ)


$(SCM_LIB_C): $(SCM_LIB_SRC)
	$(GSC) -f -link -track-scheme $(SCM_LIB_SRC)

$(SCM_OBJ): $(SCM_LIB_C)
	$(CC) -D___DYNAMIC $(CFLAGS) -c $(SCM_LIB_C)

$(SCM_R5_OBJ): $(SCM_R5_SRC)
	$(GSC) -:s -o $@ $<

$(SCM_GAMBIT_OBJ): $(SCM_GAMBIT_SRC)
	$(GSC) -o $@ $<

%.o: %.c
	@rm -f $@ 
	$(CC) $(CFLAGS) $(INCLUDES) -g -c $< -o $@ -Wno-deprecated-declarations

pimain: $(SCM_OBJ) $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ -Wl,--whole-archive $(SCM_OBJ) $(C_OBJS) $(LDFLAGS) -Wl,--no-whole-archive -rdynamic -lgambc

clean:
	rm -f *.o* $(SCM_LIB_C) pimain
	$(MAKE_XML2) clean

test_bin: testlib.o testlib_test.o
	$(CC) $(CFLAGS) -o $@ testlib.o testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

xml2.o1.o: xml2.scm
	$(MAKE_XML2)

.phony: all

