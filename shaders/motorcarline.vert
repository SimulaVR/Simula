//precision highp float;
uniform mat4 uMVPMatrix;
attribute vec3 aPosition;

void main(void)
{
    gl_Position =   uMVPMatrix * vec4(aPosition, 1.);
}
