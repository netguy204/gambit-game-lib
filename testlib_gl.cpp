#include "gl_headers.h"
#include "testlib_gl.h"

#include "memlib.h"
#include "utils.h"
#include "matrix.h"
#include "testlib.h"
#include "utils.h"

#include <math.h>
#include <stdio.h>
#include <stdarg.h>

StackAllocator gldata_allocator = NULL;

static const GLfloat quadCoords[4 * 3] = {
  0.0f, 0.0f, 0.0f,
  1.0f, 0.0f, 0.0f,
  1.0f, 1.0f, 0.0f,
  0.0f, 1.0f, 0.0f,
};

void gl_check_(const char * msg) {
  GLenum error = glGetError();
  if(error == GL_NO_ERROR) return;

  const char* e_msg;
  switch(error) {
  case GL_INVALID_ENUM:
    e_msg = "GL_INVALID_ENUM";
    break;
  case GL_INVALID_VALUE:
    e_msg = "GL_INVALID_VALUE";
    break;
  case GL_INVALID_OPERATION:
    e_msg = "GL_INVALID_OPERATION";
    break;
  case GL_OUT_OF_MEMORY:
    e_msg = "GL_OUT_OF_MEMORY";
    break;
  default:
    e_msg = "unknown";
  }

  LOGW("GL_ERROR: %s => %s\n", msg, e_msg);
}

int renderer_load_shader(const char* src, GLenum kind) {
  int shader = glCreateShader(kind);
  gl_check_("glCreateShader");

  glShaderSource(shader, 1, &src, NULL);
  gl_check_("glShaderSource");

  glCompileShader(shader);
  gl_check_("glCompileShader");

  int status;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);

  if(status == GL_FALSE) {
    char buffer[1024];
    int length;
    glGetShaderInfoLog(shader, sizeof(buffer), &length, buffer);
    fail_exit("glCompileShader: %s, %s\n", buffer, src);
  }

  return shader;
}

typedef enum {
  GLPARAM_VERTEX,
  GLPARAM_OTHER0,
  GLPARAM_NORMAL0,
  GLPARAM_COLOR0,
  GLPARAM_COLOR1,
  GLPARAM_FOGCOORD0,
  GLPARAM_OTHER1,
  GLPARAM_TEXCOORD0,
  GLPARAM_TEXCOORD1,
  GLPARAM_TEXCOORD2,
  GLPARAM_DONE
} ProgramParameters;

GLuint standard_program;
GLuint vertex_buffer;
GLuint texcoord_buffer;
GLuint color_buffer;
struct Matrix44_ orthographic_projection;
GLuint tex0_uniform_location;
GLuint mvp_uniform_location;

GLuint solid_program;
GLuint solid_mvp_uniform_location;
GLuint solid_color_location;

GLuint standard_color_program;
GLuint color_tex0_uniform_location;
GLuint color_mvp_uniform_location;

int renderer_link_shader(const char* vertexname, const char* fragmentname, ...) {
  char* vertex_source = filename_slurp(vertexname);
  char* fragment_source = filename_slurp(fragmentname);

  int vertex = renderer_load_shader(vertex_source, GL_VERTEX_SHADER);
  int fragment = renderer_load_shader(fragment_source, GL_FRAGMENT_SHADER);
  free(vertex_source);
  free(fragment_source);

  int program = glCreateProgram();

  gl_check(glAttachShader(program, vertex));
  gl_check(glAttachShader(program, fragment));

  va_list ap;
  va_start(ap, fragmentname);
  while(1) {
    ProgramParameters param = (ProgramParameters)va_arg(ap, int);
    if(param == GLPARAM_DONE) break;

    const char* name = va_arg(ap, char*);
    gl_check(glBindAttribLocation(program, param, name));
  }

  gl_check(glLinkProgram(program));

  gl_check(glDeleteShader(vertex));
  gl_check(glDeleteShader(fragment));
  int link_status;
  glGetProgramiv(program, GL_LINK_STATUS, &link_status);
  if(link_status == GL_FALSE) {
    char buffer[1024];
    int length;
    glGetProgramInfoLog(program, sizeof(buffer), &length, buffer);
    fail_exit("glLinkProgram: %s\n", buffer);
  }

  return program;
}

void renderer_init_standard_shader() {
  int program = renderer_link_shader("resources/standard.vert", "resources/standard.frag",
                                     GLPARAM_VERTEX, "vertex",
                                     GLPARAM_TEXCOORD0, "tex_coord0",
                                     GLPARAM_DONE);
  standard_program = program;
  tex0_uniform_location = glGetUniformLocation(program, "textureUnit0");
  mvp_uniform_location = glGetUniformLocation(program, "mvpMatrix");

  gl_check(glGenBuffers(1, &vertex_buffer));
  gl_check(glGenBuffers(1, &texcoord_buffer));
  gl_check(glGenBuffers(1, &color_buffer));

  program = renderer_link_shader("resources/standard.vert", "resources/solid.frag",
                                 GLPARAM_VERTEX, "vertex",
                                 GLPARAM_DONE);
  solid_program = program;
  solid_mvp_uniform_location = glGetUniformLocation(program, "mvpMatrix");
  solid_color_location = glGetUniformLocation(program, "color");

  program = renderer_link_shader("resources/standard_color.vert", "resources/standard_color.frag",
                                 GLPARAM_VERTEX, "vertex",
                                 GLPARAM_TEXCOORD0, "tex_coord0",
                                 GLPARAM_OTHER0, "color_coord0",
                                 GLPARAM_DONE);

  standard_color_program = program;
  color_tex0_uniform_location = glGetUniformLocation(program, "textureUnit0");
  color_mvp_uniform_location = glGetUniformLocation(program, "mvpMatrix");
}

void renderer_resize(int w, int h) {
  screen_width = w;
  screen_height = h;
  glViewport(0, 0, screen_width, screen_height);
  matrix_orthographic_proj(&orthographic_projection, 0.0f, screen_width, 0.0f, screen_height,
                           -1.0f, 1.0f);
}

void renderer_gl_init(int w, int h) {
  if(!gldata_allocator) {
    gldata_allocator = stack_allocator_make(1024 * 1024, "gldata_allocator");
  }

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  //glBlendEquationSeparate(GL_FUNC_ADD, GL_ONE);
  //glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE);

  glClearColor(0.8f, 0.8f, 0.8f, 1.0f);
  renderer_resize(w, h);

  // build a vao that we'll use for everything
  GLuint vao;
#ifdef __APPLE__
  glGenVertexArraysAPPLE(1, &vao);
  glBindVertexArrayAPPLE(vao);
#else
#ifdef __ANDROID__
  //glGenVertexArraysOES(1, &vao);
  //glBindVertexArrayOES(vao);
#else
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);
#endif
#endif

  renderer_init_standard_shader();

  gl_check_("setup");
}

void renderer_gl_shutdown() {
  glClear(GL_COLOR_BUFFER_BIT);
}

void renderer_begin_frame(void* empty) {
  stack_allocator_freeall(gldata_allocator);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void renderer_finish_image_load(ImageResource resource) {
  LOGI("finished image load");
  GLuint texture;
  GLenum texture_format;
  GLint num_colors;

  num_colors = resource->channels;
  if(num_colors == 4) {
    texture_format = GL_RGBA;
  } else {
    texture_format = GL_RGB;
  }

  gl_check(glGenTextures(1, &texture));
  gl_check(glBindTexture(GL_TEXTURE_2D, texture));
  gl_check(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST));
  gl_check(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST));
  gl_check(glTexImage2D(GL_TEXTURE_2D, 0, texture_format,
                        resource->w, resource->h, 0,
                        texture_format, GL_UNSIGNED_BYTE, resource->data));

  resource->texture = texture;

  free(resource->data);
}

void renderer_finish_image_free(void* texturep) {
  intptr_t big = (intptr_t)texturep;
  GLuint texture = (GLuint)big;
  glDeleteTextures(1, &texture);
}

GLuint last_texture = -1;

void spritelist_set_texs_and_verts_gl(int nverts, GLfloat* verts, GLfloat* texs) {
  gl_check(glEnableVertexAttribArray(GLPARAM_VERTEX));
  gl_check(glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer));
  gl_check(glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 3 * nverts, verts, GL_DYNAMIC_DRAW));
  gl_check(glVertexAttribPointer(GLPARAM_VERTEX, 3, GL_FLOAT, GL_FALSE, 0, 0));

  gl_check(glEnableVertexAttribArray(GLPARAM_TEXCOORD0));
  gl_check(glBindBuffer(GL_ARRAY_BUFFER, texcoord_buffer));
  gl_check(glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 2 * nverts, texs, GL_DYNAMIC_DRAW));
  gl_check(glVertexAttribPointer(GLPARAM_TEXCOORD0, 2, GL_FLOAT, GL_FALSE, 0, 0));
}

void basespritelist_render_to_screen(BaseSprite list) {
  if(!list) return;

  glUseProgram(standard_program);

  int nquads = list->count;
  int ntris = nquads * 2;
  int nverts = 3 * ntris;

  GLfloat* verts = (GLfloat*)stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 3);
  GLfloat* texs = (GLfloat*)stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 2);

  int vert_idx = 0;
  int tex_idx = 0;

  BaseSprite sprite;
  for(sprite = list; sprite != NULL;
      sprite = sprite->next) {

    // bottom-left
    verts[vert_idx++] = sprite->displayX;
    verts[vert_idx++] = sprite->displayY;
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v0;

    // bottom-right
    verts[vert_idx++] = sprite->displayX + sprite->w;
    verts[vert_idx++] = sprite->displayY;
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v0;

    // top-right
    verts[vert_idx++] = sprite->displayX + sprite->w;
    verts[vert_idx++] = sprite->displayY + sprite->h;
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v1;

    // top-right
    verts[vert_idx++] = sprite->displayX + sprite->w;
    verts[vert_idx++] = sprite->displayY + sprite->h;
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v1;

    // top-left
    verts[vert_idx++] = sprite->displayX;
    verts[vert_idx++] = sprite->displayY + sprite->h;
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v1;

    // bottom-left
    verts[vert_idx++] = sprite->displayX;
    verts[vert_idx++] = sprite->displayY;
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v0;
  }

  spritelist_set_texs_and_verts_gl(nverts, verts, texs);

  if(list->texture != last_texture) {
    gl_check(glBindTexture(GL_TEXTURE_2D, list->texture));
    last_texture = list->texture;
  }

  gl_check(glUniform1i(tex0_uniform_location, 0));
  gl_check(glUniformMatrix4fv(mvp_uniform_location, 1, GL_FALSE, orthographic_projection.data));
  gl_check(glDrawArrays(GL_TRIANGLES, 0, nverts));
}

int spritelist_set_texs_and_verts(BaseSprite list) {
  int nquads = list->count;
  int ntris = nquads * 2;
  int nverts = 3 * ntris;

  GLfloat* verts = (GLfloat*)stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 3);
  GLfloat* texs = (GLfloat*)stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 2);

  int vert_idx = 0;
  int tex_idx = 0;

  BaseSprite _sprite;
  for(_sprite = list; _sprite != NULL;
      _sprite = _sprite->next) {
    Sprite sprite = (Sprite)_sprite;

    float sa = 0.0f;
    float ca = 1.0f;
    if(sprite->angle != 0.0f) {
      sa = sinf(sprite->angle);
      ca = cosf(sprite->angle);
    }

#define VROT_X(qx, qy) (ca * (qx) - sa * (qy))
#define VROT_Y(qx, qy) (sa * (qx) + ca * (qy))
#define SCALE_X(qx) ((qx - sprite->originX) * sprite->w)
#define SCALE_Y(qy) ((qy - sprite->originY) * sprite->h)
#define VPROCESS_X(qx, qy) (VROT_X(SCALE_X(qx), SCALE_Y(qy)) + sprite->displayX)
#define VPROCESS_Y(qx, qy) (VROT_Y(SCALE_X(qx), SCALE_Y(qy)) + sprite->displayY)

    // bottom-left
    verts[vert_idx++] = VPROCESS_X(0.0f, 0.0f);
    verts[vert_idx++] = VPROCESS_Y(0.0f, 0.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v0;

    // bottom-right
    verts[vert_idx++] = VPROCESS_X(1.0f, 0.0f);
    verts[vert_idx++] = VPROCESS_Y(1.0f, 0.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v0;

    // top-right
    verts[vert_idx++] = VPROCESS_X(1.0f, 1.0f);
    verts[vert_idx++] = VPROCESS_Y(1.0f, 1.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v1;

    // top-right
    verts[vert_idx++] = VPROCESS_X(1.0f, 1.0f);
    verts[vert_idx++] = VPROCESS_Y(1.0f, 1.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v1;

    // top-left
    verts[vert_idx++] = VPROCESS_X(0.0f, 1.0f);
    verts[vert_idx++] = VPROCESS_Y(0.0f, 1.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v1;

    // bottom-left
    verts[vert_idx++] = VPROCESS_X(0.0f, 0.0f);
    verts[vert_idx++] = VPROCESS_Y(0.0f, 0.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v0;
  }

  spritelist_set_texs_and_verts_gl(nverts, verts, texs);
  return nverts;
}

void spritelist_render_to_screen(BaseSprite list) {
  if(!list) return;

  gl_check(glUseProgram(standard_program));

  int nverts = spritelist_set_texs_and_verts(list);

  if(list->texture != last_texture) {
    gl_check(glBindTexture(GL_TEXTURE_2D, list->texture));
    last_texture = list->texture;
  }

  gl_check(glUniform1i(tex0_uniform_location, 0));
  gl_check(glUniformMatrix4fv(mvp_uniform_location, 1, GL_FALSE, orthographic_projection.data));
  gl_check(glDrawArrays(GL_TRIANGLES, 0, nverts));
}

void spritelist_render_to_screen_colored(BaseSprite list) {
  if(!list) return;

  gl_check(glUseProgram(standard_color_program));

  int nverts = spritelist_set_texs_and_verts(list);

  int ncolors = nverts * 4;
  GLfloat* colors = (GLfloat*)stack_allocator_alloc(gldata_allocator, sizeof(float) * ncolors);

  BaseSprite _sprite;
  int color_idx = 0;
  for(_sprite = list; _sprite != NULL;
      _sprite = _sprite->next) {
    Sprite sprite = (Sprite)_sprite;

    // bottom-left
    colors[color_idx++] = sprite->color[0];
    colors[color_idx++] = sprite->color[1];
    colors[color_idx++] = sprite->color[2];
    colors[color_idx++] = sprite->color[3];

    // bottom-right
    colors[color_idx++] = sprite->color[0];
    colors[color_idx++] = sprite->color[1];
    colors[color_idx++] = sprite->color[2];
    colors[color_idx++] = sprite->color[3];

    // top-right
    colors[color_idx++] = sprite->color[0];
    colors[color_idx++] = sprite->color[1];
    colors[color_idx++] = sprite->color[2];
    colors[color_idx++] = sprite->color[3];

    // top-right
    colors[color_idx++] = sprite->color[0];
    colors[color_idx++] = sprite->color[1];
    colors[color_idx++] = sprite->color[2];
    colors[color_idx++] = sprite->color[3];

    // top-left
    colors[color_idx++] = sprite->color[0];
    colors[color_idx++] = sprite->color[1];
    colors[color_idx++] = sprite->color[2];
    colors[color_idx++] = sprite->color[3];

    // bottom-left
    colors[color_idx++] = sprite->color[0];
    colors[color_idx++] = sprite->color[1];
    colors[color_idx++] = sprite->color[2];
    colors[color_idx++] = sprite->color[3];
  }

  gl_check(glEnableVertexAttribArray(GLPARAM_OTHER0));
  gl_check(glBindBuffer(GL_ARRAY_BUFFER, color_buffer));
  gl_check(glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 4 * nverts, colors, GL_DYNAMIC_DRAW));
  gl_check(glVertexAttribPointer(GLPARAM_OTHER0, 4, GL_FLOAT, GL_FALSE, 0, 0));

  if(list->texture != last_texture) {
    gl_check(glBindTexture(GL_TEXTURE_2D, list->texture));
    last_texture = list->texture;
  }

  gl_check(glUniform1i(color_tex0_uniform_location, 0));
  gl_check(glUniformMatrix4fv(color_mvp_uniform_location, 1, GL_FALSE, orthographic_projection.data));
  gl_check(glDrawArrays(GL_TRIANGLES, 0, nverts));
}


void rect_render_to_screen(ColoredRect crect) {
  Rect rect = (Rect)crect;
  GLfloat rect_lines[] = {
    rect->minx, rect->miny, 0,
    rect->minx, rect->maxy, 0,

    rect->minx, rect->maxy, 0,
    rect->maxx, rect->maxy, 0,

    rect->maxx, rect->maxy, 0,
    rect->maxx, rect->miny, 0,

    rect->maxx, rect->miny, 0,
    rect->minx, rect->miny, 0
  };

  gl_check(glUseProgram(solid_program));

  gl_check(glUniformMatrix4fv(mvp_uniform_location, 1, GL_FALSE, orthographic_projection.data));

  gl_check(glEnableVertexAttribArray(GLPARAM_VERTEX));
  gl_check(glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer));
  gl_check(glBufferData(GL_ARRAY_BUFFER, sizeof(rect_lines), rect_lines, GL_DYNAMIC_DRAW));
  gl_check(glVertexAttribPointer(GLPARAM_VERTEX, 3, GL_FLOAT, GL_FALSE, 0, 0));

  gl_check(glUniform4fv(solid_color_location, 1, crect->color));
  gl_check(glDrawArrays(GL_LINES, 0, 8));
}

void filledrect_render_to_screen(ColoredRect crect) {
  Rect rect = (Rect)crect;
  GLfloat rect_tris[] = {
    rect->minx, rect->miny, 0,
    rect->minx, rect->maxy, 0,
    rect->maxx, rect->maxy, 0,

    rect->maxx, rect->maxy, 0,
    rect->maxx, rect->miny, 0,
    rect->minx, rect->miny, 0
  };

  gl_check(glUseProgram(solid_program));

  gl_check(glUniformMatrix4fv(solid_mvp_uniform_location, 1, GL_FALSE, orthographic_projection.data));

  gl_check(glEnableVertexAttribArray(GLPARAM_VERTEX));
  gl_check(glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer));
  gl_check(glBufferData(GL_ARRAY_BUFFER, sizeof(rect_tris), rect_tris, GL_DYNAMIC_DRAW));
  gl_check(glVertexAttribPointer(GLPARAM_VERTEX, 3, GL_FLOAT, GL_FALSE, 0, 0));

  gl_check(glUniform4fv(solid_color_location, 1, crect->color));
  gl_check(glDrawArrays(GL_TRIANGLES, 0, 6));
}
