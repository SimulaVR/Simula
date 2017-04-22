//precision highp float;
uniform sampler2D uTexSampler;
//barrel distortion coefficients
uniform vec4 uDistortionK;
//offset and size of view in uv space
uniform vec4 uViewportParams;
//lense center in NDC Space
uniform vec2 uLenseCenter;
uniform float uScaleFactor;

//varying vec2 vTexCoord;

//Fragment position in Uniform Device Coordinates [-1, 1]^2
varying vec2 vUDCPos;

void main(void)
{
    vec4 k = uDistortionK;

    //Radius vector in lense space
    vec2 rVecIn = vUDCPos - uLenseCenter;

    //Length Squared of lense space radius vector
    float rSq = rVecIn.x * rVecIn.x + rVecIn.y * rVecIn.y;

    //Lense Space Radius Vector with barrel distortion applied
    vec2 rVecDistorted = rVecIn * (k[0] + k[1] * rSq + k[2] * rSq * rSq + k[3] * rSq * rSq * rSq);

    //Fragment position in Uniform Device Coordinates with barrel distortion applied
    vec2 newUDCPos = rVecDistorted / uScaleFactor + uLenseCenter;



    //gl_FragColor = vec4(0.,0.,0.,1.);
    //gl_FragColor = vec4(abs(newUDCPos),0.,1.);
    if(clamp(newUDCPos, vec2(-1.), vec2(1.)) != newUDCPos){

        gl_FragColor = vec4(0.,0.,0.,1.);

    }else{
        vec2 uv = ((newUDCPos + vec2(1.))/ vec2(2.)) * uViewportParams.zw + uViewportParams.xy;
        gl_FragColor = texture2D(uTexSampler, uv);
    }

}
