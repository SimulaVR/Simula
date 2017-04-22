uniform mat4 uMVPMatrix;
attribute vec3 aPosition;
attribute float aConfidence;
attribute vec2 aTexCoord;

varying vec2 vTexCoord;
varying float vIsValid;

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
