#include "utils.h"
#include "testlib.h"
#include "game.h"
#include "android_fopen.h"
#include "testlib_internal.h"

#include <jni.h>
#include <errno.h>
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>

extern int real_main(int argc, char ** argv);

extern "C" {
  JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_MainActivity_nativeSetAssetManager(JNIEnv* env, jobject obj, jobject assetManager);
  JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameSurfaceView_nativePause(JNIEnv* env, jobject obj);
  JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameSurfaceView_nativeResume(JNIEnv* env, jobject obj);
  JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameRenderer_nativeInit(JNIEnv* env, jobject obj);
  JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameRenderer_nativeResize(JNIEnv* env, jobject obj, int w, int h);
  JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameRenderer_nativeRender(JNIEnv* env, jobject obj);
  JNIEXPORT jint JNICALL JNI_OnLoad( JavaVM *vm, void *pvt );
}

JNIEXPORT jint JNICALL JNI_OnLoad( JavaVM *vm, void *pvt ) {
  testlib_init();
  native_init();
  audio_init();
  return JNI_VERSION_1_6;
}

JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_MainActivity_nativeSetAssetManager(JNIEnv* env, jobject obj, jobject assetManager) {
  AAssetManager* mgr = AAssetManager_fromJava(env, assetManager);
  android_fopen_set_asset_manager(mgr);
}

static pthread_t game_thread;
static int game_running = 0;

void* game_exec(void* empty) {
  // wait for the render thread
  renderer_enqueue_sync(renderer_init, NULL);
  game_init();
  int local_running = 1;
  while(game_running && local_running) {
    local_running = loop_once();
  }
  game_shutdown();
}

JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameSurfaceView_nativePause(JNIEnv* env, jobject obj) {
  LOGI("nativePause");
  void* result;
  game_running = 0;
}

JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameSurfaceView_nativeResume(JNIEnv* env, jobject obj) {
  LOGI("nativeResume");
  game_running = 1;
  pthread_create(&game_thread, NULL, game_exec, NULL);
}

JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameRenderer_nativeInit(JNIEnv* env, jobject obj) {
  LOGI("nativeInit");
  renderer_gl_init(1360, 768);
}

JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameRenderer_nativeResize(JNIEnv* env, jobject obj, int w, int h) {
  LOGI("nativeResize");
  renderer_resize(w, h);
}

int end_of_frame = 0;

void signal_render_complete(void* _allocator) {
  StackAllocator allocator = (StackAllocator)_allocator;
  render_reply_queue->enqueue(allocator);
  end_of_frame = 1;
}

JNIEXPORT void JNICALL Java_com_fiftyply_gglppt_GameRenderer_nativeRender(JNIEnv* env, jobject obj) {
  // drain the render queue
  end_of_frame = 0;
  while(!end_of_frame) {
    process_render_command();
  }
}


/*
void android_main(struct android_app* state) {
  LOGI("Hello, native world!");
  sleep(3);
  android_state = state;
  android_fopen_set_asset_manager(state->activity->assetManager);

  state->onAppCmd = engine_handle_cmd;
  state->onInputEvent = engine_handle_input;

  // Make sure glue isn't stripped.
  app_dummy();

  JavaVM* vm = state->activity->vm;

  // get the ouya constants
  JNIEnv* env;
  vm->AttachCurrentThread(&env, NULL);

  jclass ouyaClazz = env->FindClass("tv/ouya/console/api/OuyaController");
  int BUTTON_A = getStaticField(env, "BUTTON_A", ouyaClazz);
  LOGI("ButtonA is %d", BUTTON_A);

  // need need to pump events until we find a window
  int ident;
  int events;
  struct android_poll_source* source;

  int should_run = 1;
  // block until we get notification that our window exists
  while(should_run) {
  while ((ident=ALooper_pollAll(-1, NULL, &events, (void**)&source)) >= 0) {

    // Process this event.
    if (source != NULL) {
      source->process(state, source);
    }

    // we've got the event that says we can get going!
    if(can_start == 1) {
      real_main(0, NULL);
      should_run = 0;
    }
  }
  }

  vm->DetachCurrentThread();
}
*/
