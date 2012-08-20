#ifndef TEST_H
#define TEST_H

#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

/* exported by test.scm */
void set_screen(SDL_Surface*);
void step(void);
void terminate();

/* exported by testlib.c */
SDL_Surface* load_image(char * file);
void blit_image(SDL_Surface* target, SDL_Surface* src, int x, int y);

#endif
