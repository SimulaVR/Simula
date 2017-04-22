//precision highp float;

uniform vec2 uEyeToSourceUVScale;
uniform vec2 uEyeToSourceUVOffset;

attribute vec4 aPosition;    	  ///< [-1,+1],[-1,+1] over the entire framebuffer. Lerp factor in Pos.z. Vignette fade factorin Pos.w.
attribute vec2 aTanEyeAnglesR;   ///< The tangents of the horizontal and vertical eye angles for the red channel.
attribute vec2 aTanEyeAnglesG;   ///< The tangents of the horizontal and vertical eye angles for the green channel.
attribute vec2 aTanEyeAnglesB;   ///< The tangents of the horizontal and vertical eye angles for the blue channel.

varying vec4 vPosition; 
varying vec2 vTexCoordR; 
varying vec2 vTexCoordG; 
varying vec2 vTexCoordB; 

void main(void)
{
	vPosition = aPosition;
	vTexCoordR = aTanEyeAnglesR * uEyeToSourceUVScale + uEyeToSourceUVOffset;
	vTexCoordG = aTanEyeAnglesG * uEyeToSourceUVScale + uEyeToSourceUVOffset;
	vTexCoordB = aTanEyeAnglesB * uEyeToSourceUVScale + uEyeToSourceUVOffset;
   vTexCoordR.y = 1.0 - vTexCoordR.y;
   vTexCoordG.y = 1.0 - vTexCoordG.y;
   vTexCoordB.y = 1.0 - vTexCoordB.y;
   gl_Position = vec4(aPosition.xy, 0, 1.);
}
