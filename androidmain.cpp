#include "utils.h"
#include "testlib.h"

//BEGIN_INCLUDE(all)
#include <jni.h>
#include <errno.h>
#include <android_native_app_glue.h>

extern int real_main(int argc, char ** argv);
extern android_app* android_state;

FILE* nativeOpen(const char* fname) {
  // wild hack to prevent resource compression
  char buffer[256];
  snprintf(buffer, sizeof(buffer), "%s.mp3", fname);

  AAsset* asset = AAssetManager_open(android_state->activity->assetManager, buffer, 0);
  if(asset == NULL) {
    LOGW("Asset %s could not be found", fname);
  }

  off_t start, length;
  int fd = AAsset_openFileDescriptor(asset, &start, &length);
  if(fd < 0) {
    LOGW("Asset %s is compressed", fname);
    return NULL;
  }
  return fdopen(fd, "r");
}

/**
 * Process the next main command.
 */
static void engine_handle_cmd(struct android_app* app, int32_t cmd) {
    switch (cmd) {
        case APP_CMD_SAVE_STATE:
          LOGI("APP_CMD_SAVE_STATE");
            break;
        case APP_CMD_INIT_WINDOW:
          real_main(0, NULL);
          LOGI("APP_CMD_INIT_WINDOW");
            break;
        case APP_CMD_TERM_WINDOW:
          LOGI("APP_CMD_TERM_WINDOW");
            break;
        case APP_CMD_GAINED_FOCUS:
          LOGI("APP_CMD_GAINED_FOCUS");
            break;
        case APP_CMD_LOST_FOCUS:
          LOGI("APP_CMD_LOST_FOCUS");
            break;
    }
}

static int32_t engine_handle_input(struct android_app* app, AInputEvent* event) {
  LOGI("Got input");
  return 0;
}


/**
 * This is the main entry point of a native application that is using
 * android_native_app_glue.  It runs in its own thread, with its own
 * event loop for receiving input events and doing other things.
 */

// defined by testlib_ouya.cpp
#include <unistd.h>
AAssetManager* android_asset_manager;

void android_main(struct android_app* state) {
  LOGI("Hello, native world!");
  sleep(3);
  android_state = state;
  android_fopen_set_asset_manager(state->activity->assetManager);

  state->onAppCmd = engine_handle_cmd;
  state->onInputEvent = engine_handle_input;

  // Make sure glue isn't stripped.
  app_dummy();

  // need need to pump events until we find a window
  int ident;
  int events;
  struct android_poll_source* source;

  // If not animating, we will block forever waiting for events.
  // If animating, we loop until all events are read, then continue
  // to draw the next frame of animation.
  while(1) {
  while ((ident=ALooper_pollAll(0, NULL, &events, (void**)&source)) >= 0) {

    // Process this event.
    if (source != NULL) {
      source->process(state, source);
    }
  }
  }
}
//END_INCLUDE(all)
