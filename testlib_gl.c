#include "memory.h"
#include "utils.h"
#include "matrix.h"

#include <math.h>

StackAllocator gldata_allocator;

static const GLfloat quadCoords[4 * 3] = {
  0.0f, 0.0f, 0.0f,
  1.0f, 0.0f, 0.0f,
  1.0f, 1.0f, 0.0f,
  0.0f, 1.0f, 0.0f,
};

void gl_check_(const char * msg) {
  GLenum error = glGetError();
  if(error == GL_NO_ERROR) return;

  char* e_msg;
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
  case GL_STACK_OVERFLOW:
    e_msg = "GL_STACK_OVERFLOW";
    break;
  case GL_STACK_UNDERFLOW:
    e_msg = "GL_STACK_UNDERFLOW";
    break;
  case GL_OUT_OF_MEMORY:
    e_msg = "GL_OUT_OF_MEMORY";
    break;
  default:
    e_msg = "unknown";
  }

  fprintf(stderr, "GL_ERROR: %s => %s\n", msg, e_msg);
}

#ifdef GL_CHECK_ERRORS
#define gl_check(command); gl_check_(#command)
#else
#define gl_check(command) command
#endif

int renderer_load_shader(char* src, int kind) {
  int shader = glCreateShader(kind);
  int length = strlen(src);
  const char* lines[1] = {src};
  glShaderSource(shader, 1, lines, &length);

  gl_check_("glShaderSource");

  glCompileShader(shader);
  gl_check_("glCompileShader");

  int status;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);

  if(status == GL_FALSE) {
    char buffer[1024];
    int length;
    glGetShaderInfoLog(shader, sizeof(buffer), &length, buffer);
    fail_exit("glCompileShader: %s\n", buffer);
  }

  return shader;
}

enum ProgramParameters {
  GLPARAM_VERTEX,
  GLPARAM_TEXCOORD0
};

int renderer_link_program(int vertex, int fragment) {
  int program = glCreateProgram();
  glAttachShader(program, vertex);
  glAttachShader(program, fragment);
  glBindAttribLocation(program, GLPARAM_VERTEX, "vertex");
  glBindAttribLocation(program, GLPARAM_TEXCOORD0, "tex_coord0");
  glLinkProgram(program);

  return program;
}

GLuint standard_program;
GLuint vertex_buffer;
GLuint texcoord_buffer;
struct Matrix44_ orthographic_projection;
GLuint tex0_uniform_location;
GLuint mvp_uniform_location;

void renderer_init_standard_shader() {
  char* vertex_source = filename_slurp("resources/standard.vert");
  char* fragment_source = filename_slurp("resources/standard.frag");

  int vertex = renderer_load_shader(vertex_source, GL_VERTEX_SHADER);
  int fragment = renderer_load_shader(fragment_source, GL_FRAGMENT_SHADER);
  int program = renderer_link_program(vertex, fragment);
  glDeleteShader(vertex);
  glDeleteShader(fragment);
  int link_status;
  glGetProgramiv(program, GL_LINK_STATUS, &link_status);
  if(link_status == GL_FALSE) {
    char buffer[1024];
    int length;
    glGetProgramInfoLog(program, sizeof(buffer), &length, buffer);
    fail_exit("glLinkProgram: %s\n", buffer);
  }

  standard_program = program;
  tex0_uniform_location = glGetUniformLocation(program, "tex0");
  mvp_uniform_location = glGetUniformLocation(program, "mvpMatrix");

  glGenBuffers(1, &vertex_buffer);
  glGenBuffers(1, &texcoord_buffer);
}

void renderer_gl_init() {
  gldata_allocator = stack_allocator_make(1024 * 1024, "gldata_allocator");

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClearColor(0.8f, 0.8f, 0.8f, 1.0f);
  glViewport(0, 0, screen_width, screen_height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrthof(0.0f, screen_width, 0.0f, screen_height, -1.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  matrix_orthographic_proj(&orthographic_projection, 0.0f, screen_width, 0.0f, screen_height,
                           -1.0f, 1.0f);

  renderer_init_standard_shader();

  gl_check_("setup");
}

void renderer_gl_shutdown() {
  glClear(GL_COLOR_BUFFER_BIT);
}

void renderer_begin_frame(void* empty) {
  stack_allocator_freeall(gldata_allocator);
  glClear(GL_COLOR_BUFFER_BIT);
}

void renderer_finish_image_load(ImageResource resource) {
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
  GLuint texture = (GLuint)texturep;
  glDeleteTextures(1, &texture);
}

GLuint last_texture = -1;

void spritelist_render_to_screen(SpriteList list) {
  if(!list) return;

  int nquads = list->count;
  int ntris = nquads * 2;
  int nverts = 3 * ntris;

  GLfloat* verts = stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 3);
  GLfloat* texs = stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 2);

  int vert_idx = 0;
  int tex_idx = 0;

  SpriteList element;
  for(element = list; element != NULL;
      element = (SpriteList)element->node.next) {
    Sprite sprite = element->sprite;

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

  glUseProgram(standard_program);

  if(list->sprite->resource->texture != last_texture) {
    glBindTexture(GL_TEXTURE_2D, list->sprite->resource->texture);
    glUniform1i(tex0_uniform_location, 0);
    last_texture = list->sprite->resource->texture;
  }

  glUniformMatrix4fv(mvp_uniform_location, 1, GL_FALSE, orthographic_projection.data);

  glEnableVertexAttribArray(GLPARAM_VERTEX);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 3 * nverts, verts, GL_STREAM_DRAW);
  glVertexAttribPointer(GLPARAM_VERTEX, 3, GL_FLOAT, GL_FALSE, 0, 0);

  glEnableVertexAttribArray(GLPARAM_TEXCOORD0);
  glBindBuffer(GL_ARRAY_BUFFER, texcoord_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 2 * nverts, texs, GL_STREAM_DRAW);
  glVertexAttribPointer(GLPARAM_TEXCOORD0, 2, GL_FLOAT, GL_FALSE, 0, 0);

  glDrawArrays(GL_TRIANGLES, 0, nverts);
}
