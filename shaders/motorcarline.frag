#version 300 es
precision highp float;

//precision highp float;
uniform highp vec3 uColor;
out highp vec4 fragmentColor;

void main(void)
{
    fragmentColor = vec4(uColor, 1.);
}
