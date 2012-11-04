#include "memory.h"

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

  // set up for drawing just quads
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
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

void sprite_render_to_screen(Sprite sprite) {
  if(sprite->resource->texture != last_texture) {
    glBindTexture(GL_TEXTURE_2D, sprite->resource->texture);
    last_texture = sprite->resource->texture;
  }

  glPushMatrix();

  const GLfloat texCoords[4 * 2] = {
    sprite->u0, sprite->v0,
    sprite->u1, sprite->v0,
    sprite->u1, sprite->v1,
    sprite->u0, sprite->v1,
  };
  glTexCoordPointer(2, GL_FLOAT, 0, texCoords);
  glVertexPointer(3, GL_FLOAT, 0, quadCoords);

  glTranslatef(sprite->displayX, sprite->displayY, 0.0f);
  glRotatef(sprite->angle, 0.0f, 0.0f, 1.0f);
  glScalef(sprite->w, sprite->h, 1.0f);
  glTranslatef(-sprite->originX, -sprite->originY, 0.0f);

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glPopMatrix();
}

void spritelist_render_to_screen(SpriteList list) {
  if(!list) return;

  if(list->sprite->resource->texture != last_texture) {
    glBindTexture(GL_TEXTURE_2D, list->sprite->resource->texture);
    last_texture = list->sprite->resource->texture;
  }

  int nquads = list->count;
  int ntris = nquads * 2;
  int nverts = 3 * ntris;

  float* verts = stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 3);
  float* texs = stack_allocator_alloc(gldata_allocator, sizeof(float) * nverts * 2);

  int vert_idx = 0;
  int tex_idx = 0;

  for(SpriteList element = list; element != NULL;
      element = (SpriteList)element->node.next) {
    Sprite sprite = element->sprite;

#define VPROCESS_X(qx) (((qx - sprite->originX) * sprite->w) + sprite->displayX)
#define VPROCESS_Y(qy) (((qy - sprite->originY) * sprite->h) + sprite->displayY)

    // bottom-left
    verts[vert_idx++] = VPROCESS_X(0.0f);
    verts[vert_idx++] = VPROCESS_Y(0.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v0;

    // bottom-right
    verts[vert_idx++] = VPROCESS_X(1.0f);
    verts[vert_idx++] = VPROCESS_Y(0.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v0;

    // top-right
    verts[vert_idx++] = VPROCESS_X(1.0f);
    verts[vert_idx++] = VPROCESS_Y(1.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v1;

    // top-right
    verts[vert_idx++] = VPROCESS_X(1.0f);
    verts[vert_idx++] = VPROCESS_Y(1.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u1;
    texs[tex_idx++] = sprite->v1;

    // top-left
    verts[vert_idx++] = VPROCESS_X(0.0f);
    verts[vert_idx++] = VPROCESS_Y(1.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v1;

    // bottom-left
    verts[vert_idx++] = VPROCESS_X(0.0f);
    verts[vert_idx++] = VPROCESS_Y(0.0f);
    verts[vert_idx++] = 0.0f;
    texs[tex_idx++] = sprite->u0;
    texs[tex_idx++] = sprite->v0;
  }

  glTexCoordPointer(2, GL_FLOAT, 0, texs);
  glVertexPointer(3, GL_FLOAT, 0, verts);
  glDrawArrays(GL_TRIANGLES, 0, nverts);
}
