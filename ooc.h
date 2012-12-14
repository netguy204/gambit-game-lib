#ifndef OOC_H
#define OOC_H

#include <stdio.h>

struct Class;

struct Object {
  const struct Class * class;
};

struct Class;

struct Class {
  const struct Object _;
  const char * name;
  const struct Class * super;
  size_t size;
  void*(*alloci)(const void* class);
  void(*dealloci)(void* self);
  void*(*ctor)(void* self, va_list * args);
  void*(*dtor)(void* self);
  int(*differ)(const void* self, const void* b);
  int(*tofile)(const void* self, FILE* fp);
};

extern const void* Object;
extern const void* Class;

void* new(const void* class, ...);
void* vinit(const void* class, void* self, va_list *app);
void* init(const void * class, void* self, ...);
void delete(void* self);

int differ(const void* self, const void * b);
int tofile(const void* self, FILE * fp);

const void* classOf(const void* self);
const char* className(const void* class);
size_t sizeOf(const void* self);
const void* super(const void* self);

void* alloci(const void* class);
void* ctor(void* _self, va_list* app);
void* dtor(void* _self);
void dealloci(void* _self);

void* super_ctor(const void* class, void* self, va_list* app);
void* super_dtor(const void* class, void* self);

int isInstanceOf(const void* _class, void* _object);

typedef void (*voidf)();

#endif
