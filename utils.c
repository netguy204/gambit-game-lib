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

long filename_size(char* filename) {
  FILE* f = fopen(filename, "r");
  if(!f) fail_exit("file %s does not exist", filename);

  fseek(f, 0, SEEK_END);
  return ftell(f);
}

char* filename_slurp(char* filename) {
  long size = filename_size(filename);
  char* data = malloc(size + 1);
  data[size] = '\0';

  FILE* f = fopen(filename, "r");
  fread(data, 1, size, f);
  fclose(f);

  return data;
}
