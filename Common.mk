C_SRC+= \
	threadlib.c memory.c listlib.c testlib.c \
	sampler.c audio.c game.c vector.c particle.c \
	rect.c controls.c agent.c steering.c spriteatlas.c \
	gambitmain.c realmain.c stb_image.c

XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=$(XML_INCLUDE)
LDFLAGS+=-lpthread
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

all: $(BIN) resources

$(BIN): $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) $(LDFLAGS)

clean:
	rm -rf *.o $(BIN) buildatlas test

TEST_DEPS=\
	memory.o listlib.o agent.o spriteatlas.o \
	game.o testlib.o threadlib.o particle.o \
	rect.o controls.o testlib_sdl.o audio.o \
	sampler.o vector.o stb_image.o audio_sdl.o \
	steering.o

test_bin: $(TEST_DEPS) testlib_test.o
	$(CC) $(CFLAGS) -o $@ $(TEST_DEPS) testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

buildatlas: buildatlas.c $(TEST_DEPS)
	$(CC) $(CFLAGS) -o $@ $< $(TEST_DEPS) $(XML_INCLUDE) -lxml2 $(LDFLAGS)

IMAGES_INPUT=spacer/images_default.xml spacer/images_default.png
IMAGE_RESOURCE=spacer/images_default.dat

resources: $(IMAGE_RESOURCE)

$(IMAGE_RESOURCE): buildatlas $(IMAGES_INPUT)
	./buildatlas $(IMAGES_INPUT) $(IMAGE_RESOURCE)

.phony: all resources
