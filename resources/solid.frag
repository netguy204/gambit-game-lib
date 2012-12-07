#ifdef GLES2
precision mediump float;
#endif

uniform vec4 color;

void main(void) {
  gl_FragColor = color;
}
