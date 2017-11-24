#version 300 es
precision highp float;

in highp vec3 aPosition;
in highp vec3 aNormal;
in highp vec2 aTextureCoord;

uniform highp mat4 uMatrix;


out highp vec2 vTexCoord;


void main(void)
{
    vTexCoord = aTexCoord;

    gl_Position =  uMatrix * vec4(aPosition, 1.);
    gl_Position.y = -gl_Position.y;


}
