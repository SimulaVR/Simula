#version 300 es
precision highp float;

uniform mat4 uMVPMatrix;
in highp vec3 aPosition;
in float aConfidence;
in highp vec2 aTexCoord;

out highp vec2 vTexCoord;
out float vIsValid;

void main(void)
{
    if(aConfidence < 500.0 || aPosition.z < 0.01){

        vIsValid = 0.0;
    }else{
        vIsValid = 1.0;
    }
    gl_Position =   uMVPMatrix * vec4(aPosition, 1.);
    vTexCoord = aTexCoord;

}
