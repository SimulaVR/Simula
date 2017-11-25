#version 300 es
precision highp float;

//precision highp float;
in highp vec3 aPosition;
//in highp vec2 aTexCoord;
//out highp vec2 vTexCoord;
out highp vec2 vUDCPos;
void main(void)
{
    //vTexCoord = aTexCoord;
    vUDCPos = aPosition.xy;
    gl_Position = vec4(aPosition, 1.);
    gl_Position.y = -gl_Position.y;

}
