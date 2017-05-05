#version 300 es
precision highp float;

//precision highp float;

uniform sampler2D uTexSampler;
in highp vec2 vTexCoord;

out highp vec4 fragmentColor;

void main(void)
{
    fragmentColor = texture2D(uTexSampler, vTexCoord);
}
