#version 300 es
precision highp float;

//precision highp float;
uniform mat4 uMVPMatrix;
in highp vec3 aPosition;

void main(void)
{
    gl_Position =   uMVPMatrix * vec4(aPosition, 1.);
}
