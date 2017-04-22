attribute vec3 aPosition;
attribute vec2 aColorTexCoord;
attribute vec2 aDepthTexCoord;

varying vec2 vColorTexCoord;
varying vec2 vDepthTexCoord;

void main(void)
{
    vColorTexCoord = aColorTexCoord;
    vDepthTexCoord = aDepthTexCoord;

    gl_Position =  vec4(aPosition, 1.);


}
