#version 300 es
precision highp float;

//precision highp float;

uniform mat4 uMVPMatrix;

in highp vec3 aPosition;
in highp vec2 aTexCoord;

out highp vec2 vTexCoord;

void main(void)
{
    vTexCoord = aTexCoord;

    gl_Position =   uMVPMatrix * vec4(aPosition, 1.);


}
