/* the location of the GL headers is platform dependant. clean that up
   a bit */

#ifndef GLHEADERS_H
#define GLHEADERS_H

#ifdef BUILD_SDL
#ifdef __APPLE__
/* SDL on Mac */
#include <OpenGL/gl.h>
#else
/* SDL on linux */
#include <GL/glew.h>
#include <GL/gl.h>
#endif
#define glOrthof glOrtho

#else
#if defined(BUILD_RPI) || defined(BUILD_ANDROID)
/* Rpi */
#define GL_GLEXT_PROTOTYPES

#include "GLES2/gl2.h"
#include "GLES2/gl2ext.h"
#include "EGL/egl.h"
#include "EGL/eglext.h"

#else
#error "Either BUILD_SDL or BUILD_RPI or BUILD_ANDROID must be defined"
#endif
#endif

#endif
