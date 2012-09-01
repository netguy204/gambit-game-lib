C_SRC=testlib_rpi.c
BIN=pimain

# from https://github.com/raspberrypi/firmware/blob/master/opt/vc/src/hello_pi/Makefile.include
CFLAGS+=-I$(SDKSTAGE)/opt/vc/include/ -I$(SDKSTAGE)/opt/vc/include/interface/vcos/pthreads

LDFLAGS+=-L$(SDKSTAGE)/opt/vc/lib/ -lGLESv2 -lEGL -lbcm_host -lvcos -lutil

include Common.mk

%.o: %.c
	@rm -f $@ 
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@ -Wno-deprecated-declarations

# testlib needs c99 enabled for the FOREACH macro to be implementable
testlib.o: testlib.c
	@rm -f $@ 
	$(CC) -std=c99 $(CFLAGS) $(INCLUDES) -c $< -o $@ -Wno-deprecated-declarations

