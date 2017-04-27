uniform sampler2D texture;
varying highp vec2 textureCoord;
void main() {
   gl_FragColor = texture2D(texture, textureCoord);
}
