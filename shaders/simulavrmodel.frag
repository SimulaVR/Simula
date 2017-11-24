#version 300 es
precision highp float;

uniform sampler2D uColorSampler;
in highp vec2 vTexCoord;

out highp vec4 fragmentColor;

void main(void)
{
    fragmentColor = texture2D(uColorSampler, vTexCoord);
}
