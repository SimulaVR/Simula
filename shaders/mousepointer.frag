#version 300 es
precision highp float;

uniform sampler2D uColorSampler;
in highp vec2 vTexCoord;
uniform highp vec2 uPointer;

out highp vec4 fragmentColor;

void main(void)
{

    highp vec4 color = texture2D(uColorSampler, vTexCoord);

    highp vec2 upper = uPointer + vec2(2,2);
    highp vec2 lower = uPointer - vec2(2,2);
    highp vec2 pos = gl_FragCoord.xy;

    if (all(greaterThanEqual(pos, lower)) && all(lessThanEqual(pos, upper)))
	fragmentColor = vec4(0,0,0,1);
    else
    	fragmentColor = color;

}
