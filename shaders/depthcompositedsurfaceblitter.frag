#version 300 es
precision highp float;

uniform sampler2D uColorSampler;
uniform sampler2D uDepthSampler;
in highp vec2 vTexCoord;
out highp vec4 fragmentColor;

void main(void)
{

    highp vec4 color = texture2D(uColorSampler, vTexCoord);
    float depth = texture2D(uDepthSampler, vTexCoord).r;

    gl_FragDepth = depth;

    //(depth > 0.999) depth = 0.;

    fragmentColor = color; //vec4(0., depth == 1.0f ? 0.0f : depth, min(color.g, 1.),  1.);

}
