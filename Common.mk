C_SRC+= \
	threadlib.c memory.c listlib.c testlib.c \
	sampler.c audio.c game.c vector.c particle.c \
	rect.c controls.c agent.c steering.c \
	gambitmain.c realmain.c stb_image.c

XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=$(XML_INCLUDE)
LDFLAGS+=-lpthread
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

all: $(BIN)

$(BIN): $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) $(LDFLAGS)

clean:
	rm -rf *.o $(BIN)

TEST_DEPS=\
	memory.o testlib_test.o listlib.o agent.o \
	game.o testlib.o threadlib.o particle.o \
	rect.o controls.o testlib_sdl.o audio.o \
	sampler.o vector.o stb_image.o audio_sdl.o

test_bin: $(TEST_DEPS)
	$(CC) $(CFLAGS) -o $@ $(TEST_DEPS) $(LDFLAGS)

test: test_bin
	./test_bin

.phony: all
