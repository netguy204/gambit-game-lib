#ifndef TESTLIB_GL_H
#define TESTLIB_GL_H

#define GL_CHECK_ERRORS

void gl_check_(const char * msg);


#ifdef GL_CHECK_ERRORS
#define gl_check(command) command; gl_check_(#command)
#else
#define gl_check(command) command
#endif


#endif
