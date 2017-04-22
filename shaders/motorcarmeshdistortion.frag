//precision highp float;
uniform sampler2D uTexSampler;

varying vec4 vPosition; 
varying vec2 vTexCoordR; 
varying vec2 vTexCoordG; 
varying vec2 vTexCoordB; 

void main(void)
{
	vec3 fragColor;
   fragColor.r = texture2D(uTexSampler, vTexCoordR).r;
   fragColor.g = texture2D(uTexSampler, vTexCoordG).g;
   fragColor.b = texture2D(uTexSampler, vTexCoordB).b;
	gl_FragColor = vec4(fragColor * vPosition.w, 1.);
}
