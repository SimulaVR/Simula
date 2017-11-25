#version 300 es
precision highp float;

//precision highp float;

uniform highp vec2 uEyeToSourceUVScale;
uniform highp vec2 uEyeToSourceUVOffset;

in highp vec4 aPosition;    	  ///< [-1,+1],[-1,+1] over the entire framebuffer. Lerp factor in Pos.z. Vignette fade factorin Pos.w.
in highp vec2 aTanEyeAnglesR;   ///< The tangents of the horizontal and vertical eye angles for the red channel.
in highp vec2 aTanEyeAnglesG;   ///< The tangents of the horizontal and vertical eye angles for the green channel.
in highp vec2 aTanEyeAnglesB;   ///< The tangents of the horizontal and vertical eye angles for the blue channel.

out highp vec4 vPosition; 
out highp vec2 vTexCoordR; 
out highp vec2 vTexCoordG; 
out highp vec2 vTexCoordB; 

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
   gl_Position.y = -gl_Position.y;

}
