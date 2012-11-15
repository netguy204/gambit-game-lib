precision mediump float;

uniform sampler2D textureUnit0;
varying vec2 tex0;

void main(void) {
  gl_FragColor = texture2D(textureUnit0, tex0);
}
