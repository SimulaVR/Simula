#version 300 es
precision highp float;

uniform sampler2D uTexSampler;
in highp vec2 textureCoord;
out highp vec4 fragmentColor;

void main() {
   fragmentColor = texture2D(uTexSampler, textureCoord);
}
