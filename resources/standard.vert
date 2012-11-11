attribute vec4 vertex;
attribute vec2 tex_coord0;
uniform mat4 mvpMatrix;

varying vec2 tex0;

void main(void) {
  gl_Position = mvpMatrix * vertex;
  tex0 = tex_coord0;
}
