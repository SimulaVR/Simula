#version 300 es
precision highp float;

uniform sampler2D texture;
in highp vec2 textureCoord;
out highp vec4 fragmentColor;

void main() {
   fragmentColor = texture2D(texture, textureCoord);
}
