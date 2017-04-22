//precision highp float;
attribute vec3 aPosition;
//attribute vec2 aTexCoord;
//varying vec2 vTexCoord;
varying vec2 vUDCPos;
void main(void)
{
    //vTexCoord = aTexCoord;
    vUDCPos = aPosition.xy;
    gl_Position = vec4(aPosition, 1.);
}
