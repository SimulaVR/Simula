#version 300 es
precision highp float;

//precision highp float;
uniform sampler2D uTexSampler;

in highp vec4 vPosition; 
in highp vec2 vTexCoordR; 
in highp vec2 vTexCoordG; 
in highp vec2 vTexCoordB; 
out highp vec4 fragmentColor;

void main(void)
{
	highp vec3 fragColor;
   fragColor.r = texture2D(uTexSampler, vTexCoordR).r;
   fragColor.g = texture2D(uTexSampler, vTexCoordG).g;
   fragColor.b = texture2D(uTexSampler, vTexCoordB).b;
	fragmentColor = vec4(fragColor * vPosition.w, 1.);
}
