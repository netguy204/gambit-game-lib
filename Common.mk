C_SRC+= \
	threadlib.c memory.c listlib.c testlib.c \
	sampler.c audio.c game.c vector.c particle.c \
	gambitmain.c realmain.c stb_image.c

XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=$(XML_INCLUDE)
LDFLAGS+=-lpthread
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

all: $(BIN)

$(BIN): $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) $(SCM_OBJ) $(LDFLAGS)

clean:
	rm -rf *.o $(BIN)

test_bin: memory.o testlib_test.o
	$(CC) $(CFLAGS) -o $@ memory.o testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

.phony: all
