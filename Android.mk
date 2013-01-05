LOCAL_PATH:= $(call my-dir)
include jni/Src.mk

TREMOR_SRC=\
	vender/tremor/block.c vender/tremor/mapping0.c vender/tremor/synthesis.c \
	vender/tremor/codebook.c vender/tremor/mdct.c vender/tremor/vorbisfile.c \
	vender/tremor/floor0.c vender/tremor/registry.c	vender/tremor/window.c \
	vender/tremor/floor1.c vender/tremor/res012.c vender/tremor/info.c \
	vender/tremor/sharedbook.c

OGG_SRC=\
	vender/libogg-1.3.0/src/bitwise.c vender/libogg-1.3.0/src/framing.c

LUA_SRC=\
	vender/lua-5.2.1/src/lapi.c vender/lua-5.2.1/src/ldo.c \
	vender/lua-5.2.1/src/loadlib.c vender/lua-5.2.1/src/ltablib.c \
	vender/lua-5.2.1/src/lauxlib.c vender/lua-5.2.1/src/ldump.c \
	vender/lua-5.2.1/src/lobject.c vender/lua-5.2.1/src/ltm.c \
	vender/lua-5.2.1/src/lbaselib.c vender/lua-5.2.1/src/lfunc.c \
	vender/lua-5.2.1/src/lopcodes.c	vender/lua-5.2.1/src/lua.c \
	vender/lua-5.2.1/src/lbitlib.c vender/lua-5.2.1/src/lgc.c \
	vender/lua-5.2.1/src/loslib.c vender/lua-5.2.1/src/luac.c \
	vender/lua-5.2.1/src/lcode.c vender/lua-5.2.1/src/linit.c \
	vender/lua-5.2.1/src/lparser.c vender/lua-5.2.1/src/lundump.c \
	vender/lua-5.2.1/src/lcorolib.c	vender/lua-5.2.1/src/liolib.c \
	vender/lua-5.2.1/src/lstate.c vender/lua-5.2.1/src/lvm.c \
	vender/lua-5.2.1/src/lctype.c vender/lua-5.2.1/src/llex.c \
	vender/lua-5.2.1/src/lstring.c vender/lua-5.2.1/src/lzio.c \
	vender/lua-5.2.1/src/ldblib.c vender/lua-5.2.1/src/lmathlib.c \
	vender/lua-5.2.1/src/lstrlib.c vender/lua-5.2.1/src/ldebug.c \
	vender/lua-5.2.1/src/lmem.c vender/lua-5.2.1/src/ltable.c

B2D_SRC=\
	vender/Box2D_v2.2.1/Box2D/Collision/b2BroadPhase.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2CollideCircle.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2CollideEdge.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2CollidePolygon.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2Collision.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2Distance.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2DynamicTree.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/b2TimeOfImpact.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/Shapes/b2ChainShape.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/Shapes/b2CircleShape.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/Shapes/b2EdgeShape.cpp \
	vender/Box2D_v2.2.1/Box2D/Collision/Shapes/b2PolygonShape.cpp \
	vender/Box2D_v2.2.1/Box2D/Common/b2BlockAllocator.cpp \
	vender/Box2D_v2.2.1/Box2D/Common/b2Draw.cpp \
	vender/Box2D_v2.2.1/Box2D/Common/b2Math.cpp \
	vender/Box2D_v2.2.1/Box2D/Common/b2Settings.cpp \
	vender/Box2D_v2.2.1/Box2D/Common/b2StackAllocator.cpp \
	vender/Box2D_v2.2.1/Box2D/Common/b2Timer.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/b2Body.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/b2ContactManager.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/b2Fixture.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/b2Island.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/b2World.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/b2WorldCallbacks.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2ChainAndCircleContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2ChainAndPolygonContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2CircleContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2Contact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2ContactSolver.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2EdgeAndCircleContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2EdgeAndPolygonContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2PolygonAndCircleContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Contacts/b2PolygonContact.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2DistanceJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2FrictionJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2GearJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2Joint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2MouseJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2PrismaticJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2PulleyJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2RevoluteJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2RopeJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2WeldJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Dynamics/Joints/b2WheelJoint.cpp \
	vender/Box2D_v2.2.1/Box2D/Rope/b2Rope.cpp

# needed for tremor on ARM
FORCE_ARM=-DLITTLE_ENDIAN=1 -DBYTE_ORDER=LITTLE_ENDIAN

include $(CLEAR_VARS)
LOCAL_MODULE    := gamesupport
LOCAL_CFLAGS	:= -Werror -Ijni/vender/libogg-1.3.0/include -Ijni/vender/tremor -Ijni/vender/lua-5.2.1/src -Ijni/$(B2D_BASE) -DSFMT_MEXP=607 $(FORCE_ARM)
LOCAL_CPPFLAGS	:= -Ijni/vender/Box2D_v2.2.1/
LOCAL_SRC_FILES := $(B2D_SRC) $(TREMOR_SRC) $(OGG_SRC) $(LUA_SRC) \
	sfmt/SFMT.c spectra.c stb_image.c
include $(BUILD_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := main
LOCAL_CFLAGS	:= -Werror -Ijni/vender/libogg-1.3.0/include -Ijni/vender/tremor -Ijni/vender/lua-5.2.1/src -Ijni/sfmt -DBUILD_ANDROID
LOCAL_CPPFLAGS  := -std=c++0x -Wno-invalid-offsetof -Ijni/vender/Box2D_v2.2.1/
LOCAL_SRC_FILES := $(GAME_SRC) testlib_ouya.cpp audio_ouya.cpp androidmain.cpp
LOCAL_LDLIBS    := -llog -lGLESv2 -lEGL -landroid
LOCAL_STATIC_LIBRARIES := gamesupport android_native_app_glue

include $(BUILD_SHARED_LIBRARY)

$(call import-module,android/native_app_glue)
