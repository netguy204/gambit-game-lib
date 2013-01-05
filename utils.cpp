#include "utils.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

void* fail_exit(const char * message, ...) {
  fprintf(stderr, "FAIL_EXIT: ");

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  fflush(stderr);
  exit(1);
  return NULL;
}

void timer_start(Timer timer) {
  gettimeofday((struct timeval*)timer, NULL);
}

long timer_elapsed_msecs(Timer timer) {
  struct timeval now;
  gettimeofday(&now, NULL);

  long dsecs = now.tv_sec - timer->val.tv_sec;
  long dusecs = now.tv_usec - timer->val.tv_usec;

  return dsecs * 1000 + (dusecs / 1000);
}

#ifndef __ANDROID__
long filename_size(const char* filename) {
  FILE* f = nativeOpen(filename);
  if(!f) fail_exit("file %s does not exist", filename);

  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fclose(f);
  return size;
}

char* filename_slurp(const char* filename) {
  long size = filename_size(filename);
  char* data = (char*)malloc(size + 1);
  data[size] = '\0';

  FILE* f = nativeOpen(filename);
  fread(data, 1, size, f);
  fclose(f);

  return data;
}
#else

#include <android_native_app_glue.h>
extern android_app* android_state;

char* filename_slurp(const char* filename) {
  AAsset* asset = AAssetManager_open(android_state->activity->assetManager, filename, 0);
  if(asset == NULL) {
    fail_exit("Asset %s could not be found", filename);
  }

  off_t nBytes = AAsset_getLength(asset);
  char* buffer = (char*)malloc(nBytes + 1);
  off_t read = 0;
  while(read < nBytes) {
    int nRead = AAsset_read(asset, &buffer[read], nBytes - read);
    if(nRead < 0) {
      fail_exit("Error reading asset %s", filename);
    } else if(nRead == 0) {
      break;
    }
    read += nRead;
  }
  AAsset_close(asset);

  buffer[read] = '\0';
  return buffer;
}

#endif
