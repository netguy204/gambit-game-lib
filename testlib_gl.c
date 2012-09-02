
static const GLfloat quadCoords[4 * 3] = {
  0.0f, 0.0f, 0.0f,
  1.0f, 0.0f, 0.0f,
  1.0f, 1.0f, 0.0f,
  0.0f, 1.0f, 0.0f,
};

static const GLfloat texCoords[4 * 2] = {
  0.0f, 1.0f,
  1.0f, 1.0f,
  1.0f, 0.0f,
  0.0f, 0.0f,
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
  glVertexPointer(3, GL_FLOAT, 0, quadCoords);

  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, 0, texCoords);
  gl_check_("setup");
}

void renderer_gl_shutdown() {
  glClear(GL_COLOR_BUFFER_BIT);
}

void renderer_begin_frame(void* empty) {
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
  gl_check(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
  gl_check(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));
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

void sprite_render_to_screen(Sprite sprite) {
  glBindTexture(GL_TEXTURE_2D, sprite->resource->texture);
  glPushMatrix();
  
  glTranslatef(sprite->displayX, sprite->displayY, 0.0f);
  glScalef(sprite->scale, sprite->scale, 1.0f);
  glRotatef(sprite->angle, 0.0f, 0.0f, 1.0f);
  glTranslatef(-sprite->originX, -sprite->originY, 0.0f);
  glScalef(sprite->resource->w, sprite->resource->h, 1.0f);

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glPopMatrix();
}
