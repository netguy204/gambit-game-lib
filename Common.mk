include Src.mk

B2D_BASE=vender/Box2D_v2.2.1/
B2D_SRC=\
	$(wildcard $(B2D_BASE)/Box2D/Collision/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Collision/Shapes/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Common/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Dynamics/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Dynamics/Joints/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Dynamics/Contacts/*.cpp) \
	$(wildcard $(B2D_BASE)/Box2D/Rope/*.cpp)

CPP_SRC+=$(GAME_SRC) \
	$(B2D_SRC)

TREMOR_SRC=$(wildcard vender/tremor/*.c)
OGG_SRC=$(wildcard vender/libogg-1.3.0/src/*.c)

OGG_HEADER=vender/libogg-1.3.0/include/ogg/config_types.h
LUA_LIB=vender/lua-5.2.1/src/liblua.a

C_SRC+=sfmt/SFMT.c spectra.c $(OGG_SRC) $(TREMOR_SRC) stb_image.c

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

spriter_test: $(C_OBJS) spriter_test.o
	$(CXX) $(CXXFLAGS) -o $@ $(C_OBJS) spriter_test.o $(LDFLAGS)

test: test_bin
	./test_bin

$(OGG_HEADER): vender/libogg-1.3.0/configure
	cd vender/libogg-1.3.0 ; ./configure

$(LUA_LIB):
	cd vender/lua-5.2.1 ; make $(PLATFORM)

C_TOOL_OBJS=$(C_OBJS)
BUILD_WITH_XML=$(CXX) $(CXXFLAGS) -o $@ $< $(C_TOOL_OBJS) $(LDFLAGS)

# plain (non-tile) sprites whose ordering and dimensions don't matter
SPRITE_PSDS=$(wildcard sprites/*.psd)
SPRITE_PNGS_FROM_PSD=$(patsubst %.psd, %.png, $(SPRITE_PSDS))
SPRITE_PNGS_FROM_PNG=$(wildcard sprites/*.png)
SPRITE_PNGS=$(sort $(SPRITE_PNGS_FROM_PNG) $(SPRITE_PNGS_FROM_PSD))

%.png: %.psd
	osascript tools/psdconvert.scpt $(PWD)/$< $(PWD)/$@

RESOURCE_FILES=resources/images_default.png resources/images_default.dat

$(RESOURCE_FILES):
	python tools/spritepak.py sprites/trim.txt resources/images_default $(SPRITE_PNGS)

# compiled spriter
SPRITER_SCML=$(wildcard sprites/*.scml)
SPRITER_CS=$(patsubst sprites/%.scml, resources/%.cs, $(SPRITER_SCML))

resources/%.cs: sprites/%.scml
	python tools/spriterpak.py $< $@


# tiles (must all be the same dimension and must maintain order or
# tiled will get unhappy
TILE_NAMEFILE=tiles/order.txt
TILE_NAMES=$(shell cat $(TILE_NAMEFILE))
TILE_PSDS=$(patsubst %, tiles/%.psd, $(TILE_NAMES))
TILE_PNGS=$(patsubst %.psd, %.png, $(TILE_PSDS))

testest:
	@echo $(TILE_NAMES)

pngs: $(SPRITE_PNGS) $(TILE_PNGS)

TILE_RESOURCES=resources/tiles.png resources/tiles.dat
$(TILE_RESOURCES): $(TILE_NAMEFILE)
	python tools/spritepak.py sprites/trim.txt resources/tiles $(TILE_PNGS)

resources: $(RESOURCE_FILES) $(SPRITER_CS) $(TILE_RESOURCES)

items_bin: items_bin.o $(C_TOOL_OBJS)
	$(BUILD_WITH_XML)

sfmt/SFMT.o: sfmt/SFMT.c
	$(CXX) $(CXXFLAGS) -c $< -o $@ -DSFMT_MEXP=607

GAME_OBJS=$(patsubst %.cpp,%.o,$(GAME_SRC))

clean:
	rm -rf $(GAME_OBJS) $(BIN) buildatlas test items_bin $(RESOURCE_FILES) $(TILE_RESOURCES) resources/*.cs

distclean: clean
	rm -rf $(C_OBJS)
	(cd vender/libogg-1.3.0 ; make distclean)
	(cd vender/lua-5.2.1 ; make clean)

.phony: all resources pngs
