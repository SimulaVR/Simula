uniform highp mat4 matrix;
attribute highp vec3 vertexCoordEntry;
attribute highp vec2 textureCoordEntry;
varying highp vec2 textureCoord;
void main() {
   textureCoord = textureCoordEntry;
   gl_Position = matrix * vec4(vertexCoordEntry, 1.);
}
