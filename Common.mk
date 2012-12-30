B2D_BASE=vender/Box2D_v2.2.1/
B2D_SRC=\
	$(wildcard $(B2D_BASE)/Box2D/Collision/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Collision/Shapes/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Common/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Dynamics/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Dynamics/Joints/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Dynamics/Contacts/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Rope/*.cpp)

CPP_SRC+= \
	threadlib.cpp memory.cpp listlib.cpp testlib.cpp \
	sampler.cpp audio.cpp game.cpp vector.cpp \
	rect.cpp controls.cpp steering.cpp spriteatlas.cpp \
	realmain.cpp stb_image.cpp tiles.cpp random.cpp \
	perlin.cpp heapvector.cpp xmltools.cpp \
	pathfinder.cpp utils.cpp matrix.cpp ooc.cpp \
	game_ui.cpp platform.cpp gameobject.cpp color.cpp \
	$(B2D_SRC)

TREMOR_SRC=$(wildcard vender/tremor/*.c)
OGG_SRC=$(wildcard vender/libogg-1.3.0/src/*.c)

OGG_HEADER=vender/libogg-1.3.0/include/ogg/config_types.h
LUA_LIB=vender/lua-5.2.1/src/liblua.a

C_SRC+= \
	sfmt/SFMT.c spectra.c $(OGG_SRC) $(TREMOR_SRC)

XML_INCLUDE:=-I/usr/include/libxml2

CFLAGS+=$(XML_INCLUDE) -Isfmt/ -Ivender/tremor/ -Ivender/libogg-1.3.0/include/ -Ivender/lua-5.2.1/src
CXXFLAGS+=$(CFLAGS) -std=c++0x -Wno-invalid-offsetof -I$(B2D_BASE)

LDFLAGS+=-lpthread -ldl -lxml2 $(LUA_LIB)
C_OBJS=\
	$(patsubst %.cpp,%.o,$(CPP_SRC)) \
	$(patsubst %.c,%.o,$(C_SRC))

EXE_OBJS=$(C_OBJS) gambitmain.o

#testlib.o: testlib_gl.cpp
all: $(OGG_HEADER) $(BIN) resources

$(BIN): $(EXE_OBJS) $(LUA_LIB)
	$(CXX) $(CXXFLAGS) -o $@ $(EXE_OBJS) $(LDFLAGS)

test_bin: $(C_OBJS) testlib_test.o
	$(CXX) $(CXXFLAGS) -o $@ $(C_OBJS) testlib_test.o $(LDFLAGS)

test: test_bin
	./test_bin

$(OGG_HEADER): vender/libogg-1.3.0/configure
	cd vender/libogg-1.3.0 ; ./configure

$(LUA_LIB):
	cd vender/lua-5.2.1 ; make $(PLATFORM)

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
