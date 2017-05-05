#version 300 es
precision highp float;

in highp vec3 aPosition;
in highp vec2 aColorTexCoord;
in highp vec2 aDepthTexCoord;

out highp vec2 vColorTexCoord;
out highp vec2 vDepthTexCoord;

void main(void)
{
    vColorTexCoord = aColorTexCoord;
    vDepthTexCoord = aDepthTexCoord;

    gl_Position =  vec4(aPosition, 1.);


}
