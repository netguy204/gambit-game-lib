#ifndef TESTLIB_GL_H
#define TESTLIB_GL_H

#define GL_CHECK_ERRORS

void gl_check_(const char * msg);

#define STRINGIZE(x) STRINGIZE2(x)
#define STRINGIZE2(x) #x

#ifdef GL_CHECK_ERRORS
#define gl_check(command) command; gl_check_(__FILE__ ": " STRINGIZE(__LINE__) " " #command)
#else
#define gl_check(command) command
#endif


#endif
