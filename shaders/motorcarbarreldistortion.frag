#version 300 es
precision highp float;

//precision highp float;
uniform sampler2D uTexSampler;
//barrel distortion coefficients
uniform highp vec4 uDistortionK;
//offset and size of view in uv space
uniform highp vec4 uViewportParams;
//lense center in NDC Space
uniform highp vec2 uLenseCenter;
uniform float uScaleFactor;

//in highp vec2 vTexCoord;

//Fragment position in Uniform Device Coordinates [-1, 1]^2
in highp vec2 vUDCPos;

out highp vec4 fragmentColor;

void main(void)
{
    highp vec4 k = uDistortionK;

    //Radius vector in lense space
    highp vec2 rVecIn = vUDCPos - uLenseCenter;

    //Length Squared of lense space radius vector
    float rSq = rVecIn.x * rVecIn.x + rVecIn.y * rVecIn.y;

    //Lense Space Radius Vector with barrel distortion applied
    highp vec2 rVecDistorted = rVecIn * (k[0] + k[1] * rSq + k[2] * rSq * rSq + k[3] * rSq * rSq * rSq);

    //Fragment position in Uniform Device Coordinates with barrel distortion applied
    highp vec2 newUDCPos = rVecDistorted / uScaleFactor + uLenseCenter;



    //gl_FragColor = vec4(0.,0.,0.,1.);
    //gl_FragColor = vec4(abs(newUDCPos),0.,1.);
    if(clamp(newUDCPos, vec2(-1.), vec2(1.)) != newUDCPos){

        fragmentColor = vec4(0.,0.,0.,1.);

    }else{
        highp vec2 uv = ((newUDCPos + vec2(1.))/vec2(2.)) * uViewportParams.zw + uViewportParams.xy;
        fragmentColor = texture2D(uTexSampler, uv);
    }

}
