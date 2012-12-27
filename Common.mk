CPP_SRC+= \
	threadlib.cpp memory.cpp listlib.cpp testlib.cpp \
	sampler.cpp audio.cpp game.cpp vector.cpp particle.cpp \
	rect.cpp controls.cpp agent.cpp steering.cpp spriteatlas.cpp \
	realmain.cpp stb_image.cpp tiles.cpp random.cpp \
	perlin.cpp heapvector.cpp xmltools.cpp \
	pathfinder.cpp utils.cpp matrix.cpp ooc.cpp \
	game_ui.cpp platform.cpp gameobject.cpp color.cpp

TREMOR_SRC=$(wildcard tremor/*.c)
OGG_SRC=$(wildcard libogg-1.3.0/src/*.c)

C_SRC+= \
	sfmt/SFMT.c spectra.c $(OGG_SRC) $(TREMOR_SRC)

XML_INCLUDE:=-I/usr/include/libxml2
CXXFLAGS+=$(XML_INCLUDE) -Isfmt/ -std=c++0x -Wno-invalid-offsetof -Itremor/

LDFLAGS+=-lpthread -ldl -lxml2
C_OBJS=\
	$(patsubst %.cpp,%.o,$(CPP_SRC)) \
	$(patsubst %.c,%.o,$(C_SRC))

EXE_OBJS=$(C_OBJS) gambitmain.o

#testlib.o: testlib_gl.cpp
all: $(BIN) resources

$(BIN): $(EXE_OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(EXE_OBJS) $(LDFLAGS)

test_bin: $(C_OBJS) testlib_test.o
	$(CXX) $(CXXFLAGS) -o $@ $(C_OBJS) testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

C_TOOL_OBJS=$(C_OBJS)
BUILD_WITH_XML=$(CXX) $(CXXFLAGS) -o $@ $< $(C_TOOL_OBJS) $(LDFLAGS)

SPRITE_PSDS=$(wildcard sprites/*.psd)
SPRITE_PNGS=$(patsubst %.psd, %.png, $(SPRITE_PSDS))

%.png: %.psd
	osascript tools/psdconvert.scpt $(PWD)/$< $(PWD)/$@

pngs: $(SPRITE_PNGS)

RESOURCE_FILES=resources/images_default.png resources/images_default.dat

$(RESOURCE_FILES):
	python tools/spritepak.py sprites/notrim.txt resources/images_default $(SPRITE_PNGS)

resources: $(RESOURCE_FILES)

items_bin: items_bin.o $(C_TOOL_OBJS)
	$(BUILD_WITH_XML)

sfmt/SFMT.o: sfmt/SFMT.c
	$(CXX) $(CXXFLAGS) -c $< -o $@ -DSFMT_MEXP=607

clean:
	rm -rf $(C_OBJS) $(BIN) buildatlas test items_bin $(RESOURCE_FILES)



.phony: all resources pngs
