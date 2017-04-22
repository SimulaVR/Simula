uniform sampler2D uTexSampler;
varying vec2 vTexCoord;
varying float vIsValid;
void main(void)
{
    if(vIsValid < 1.0){
        discard;
    }
    //gl_FragColor = vec4(vTexCoord.x, vTexCoord.y, 0., 1.);
    gl_FragColor = texture2D(uTexSampler, vTexCoord).bgra;
}
