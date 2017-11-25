#version 300 es
precision highp float;

uniform highp mat4 matrix;
in highp vec3 vertexCoordEntry;
in highp vec2 textureCoordEntry;
out highp vec2 textureCoord;
void main() {
   textureCoord = textureCoordEntry;
   gl_Position = matrix * vec4(vertexCoordEntry, 1.);
   gl_Position.y = -gl_Position.y;

}
