uniform sampler2D uColorSampler;
uniform sampler2D uDepthSampler;
varying vec2 vTexCoord;


void main(void)
{

    vec4 color = texture2D(uColorSampler, vTexCoord);
    float depth = texture2D(uDepthSampler, vTexCoord).r;

    gl_FragDepth = depth;

    //(depth > 0.999) depth = 0.;

    gl_FragColor = color; //vec4(0., depth == 1.0f ? 0.0f : depth, min(color.g, 1.),  1.);

}
