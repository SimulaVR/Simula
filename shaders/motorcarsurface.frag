//precision highp float;

uniform sampler2D uTexSampler;
varying vec2 vTexCoord;

void main(void)
{
    gl_FragColor = texture2D(uTexSampler, vTexCoord);
}
