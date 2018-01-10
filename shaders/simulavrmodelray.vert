#version 300 es
precision highp float;

//precision highp float;
uniform mat4 uMVPMatrix;
in highp vec4 aPosition;

void main(void)
{
    gl_Position =   uMVPMatrix * aPosition;
    gl_Position.y = -gl_Position.y;

}
