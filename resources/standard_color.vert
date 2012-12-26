#ifdef GLES2
precision mediump float;
#endif

attribute vec4 vertex;
attribute vec2 tex_coord0;
attribute vec4 color_coord0;

uniform mat4 mvpMatrix;

varying vec2 tex0;
varying vec4 color0;

void main(void) {
  tex0 = tex_coord0;
  color0 = color_coord0;
  gl_Position = mvpMatrix * vertex;
}
