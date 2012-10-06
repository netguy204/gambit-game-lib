C_SRC+= \
	threadlib.c memory.c listlib.c testlib.c \
	sampler.c audio.c game.c vector.c particle.c \
	rect.c controls.c agent.c steering.c spriteatlas.c \
	realmain.c stb_image.c tiles.c sfmt/SFMT.c random.c

XML_INCLUDE:=-I/usr/include/libxml2
CFLAGS+=$(XML_INCLUDE) -DSFMT_MEXP=607 -Isfmt/

LDFLAGS+=-lpthread
C_OBJS=$(patsubst %.c,%.o,$(C_SRC))

EXE_OBJS=$(C_OBJS) gambitmain.o

all: $(BIN) resources

$(BIN): $(EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $(EXE_OBJS) $(LDFLAGS)

clean:
	rm -rf $(C_OBJS) $(BIN) buildatlas test

test_bin: $(C_OBJS) testlib_test.o
	$(CC) $(CFLAGS) -o $@ $(C_OBJS) testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

buildatlas: buildatlas.o $(C_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(C_OBJS) $(XML_INCLUDE) -lxml2 $(LDFLAGS)

IMAGES_INPUT=spacer/images_default.xml spacer/images_default.png
IMAGE_RESOURCE=spacer/images_default.dat

resources: $(IMAGE_RESOURCE)

$(IMAGE_RESOURCE): buildatlas $(IMAGES_INPUT)
	./buildatlas $(IMAGES_INPUT) $(IMAGE_RESOURCE)

.phony: all resources
