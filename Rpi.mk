CPP_SRC=testlib_rpi.cpp audio_rpi.cpp joystick.cpp
BIN=pimain

PLATFORM=linux

# from https://github.com/raspberrypi/firmware/blob/master/opt/vc/src/hello_pi/Makefile.include
CFLAGS+=-D_ARM_ASSEM_ -DBUILD_RPI

CXXFLAGS+=-I$(SDKSTAGE)/opt/vc/include/ -I$(SDKSTAGE)/opt/vc/include/interface/vcos/pthreads -I/opt/vc/src/hello_pi/libs/ilclient/

LDFLAGS+=-L$(SDKSTAGE)/opt/vc/lib/ -lGLESv2 -lEGL -lbcm_host -lvcos -lutil -L/opt/vc/src/hello_pi/libs/ilclient/ -lilclient -lopenmaxil

include Common.mk

%.o: %.cpp
	@rm -f $@
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@ -Wno-deprecated-declarations
