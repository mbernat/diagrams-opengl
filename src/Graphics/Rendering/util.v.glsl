attribute vec2 coord2d;
attribute vec4 v_color;
uniform mat4 mvp;

varying vec4 f_color;

void main(void) {
     gl_Position = mvp * vec4(coord2d, 0, 1.0);
     f_color = v_color;
}
