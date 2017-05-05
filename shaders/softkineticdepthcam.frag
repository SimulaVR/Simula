#version 300 es
precision highp float;

uniform sampler2D uTexSampler;
in highp vec2 vTexCoord;
in float vIsValid;
out highp vec4 fragmentColor;
void main(void)
{
    if(vIsValid < 1.0){
        discard;
    }
    //gl_FragColor = vec4(vTexCoord.x, vTexCoord.y, 0., 1.);
    fragmentColor = texture2D(uTexSampler, vTexCoord).bgra;
}
