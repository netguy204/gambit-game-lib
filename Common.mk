C_SRC+= \
	threadlib.c memory.c listlib.c testlib.c \
	sampler.c audio.c game.c vector.c particle.c \
	rect.c controls.c agent.c steering.c spriteatlas.c \
	realmain.c stb_image.c tiles.c sfmt/SFMT.c random.c \
	perlin.c items.c heapvector.c worldgen.c xmltools.c \
	pathfinder.c utils.c matrix.c

XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=$(XML_INCLUDE) -Isfmt/

LDFLAGS+=-lpthread -ldl -lxml2
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

EXE_OBJS=$(C_OBJS) gambitmain.o

#testlib.o: testlib_gl.c
all: $(BIN) resources

$(BIN): $(EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $(EXE_OBJS) $(LDFLAGS)

clean:
	rm -rf *.o $(BIN) buildatlas test

test_bin: $(C_OBJS) testlib_test.o
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

C_TOOL_OBJS=$(C_OBJS)
BUILD_WITH_XML=$(CC) $(CFLAGS) -o $@ $< $(C_TOOL_OBJS) $(LDFLAGS)

buildatlas: buildatlas.o $(C_TOOL_OBJS)
	$(BUILD_WITH_XML)

items_bin: items_bin.o $(C_TOOL_OBJS)
	$(BUILD_WITH_XML)

sfmt/SFMT.o: sfmt/SFMT.c
	$(CC) $(CFLAGS) -c $< -o $@ -DSFMT_MEXP=607

IMAGES_INPUT=resources/images_default.xml resources/images_default.png
IMAGE_RESOURCE=resources/images_default.dat

resources: $(IMAGE_RESOURCE)

$(IMAGE_RESOURCE): buildatlas $(IMAGES_INPUT)
	./buildatlas $(IMAGES_INPUT) $(IMAGE_RESOURCE)

.phony: all resources
