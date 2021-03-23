shader_type spatial;
render_mode unshaded, cull_disabled, skip_vertex_transform;

uniform float scale_t = 0.2;
uniform float length = 20.0;
uniform float ws = 1.0;
uniform vec4 mix_color : hint_color;
uniform sampler2D arrow_texture : hint_albedo;

void vertex() {
	vec3 down = vec3(0.0, -1.0 / ws, 0.0);
	
	// offset our Z so we're projecting from our origin point
	VERTEX.z -= 0.5;
	VERTEX.z *= length;
	
	// now use that to create our arch
	float t = VERTEX.z * scale_t;
	float t2 = t * t;

	// translate to our world vector
	VERTEX = (WORLD_MATRIX * vec4(VERTEX, 1.0)).xyz; 
	
	// and now create our arch
	VERTEX += down * t2;
	
	// and apply our camera matrix
	VERTEX = (INV_CAMERA_MATRIX * vec4(VERTEX, 1.0)).xyz;
}

void fragment() {
	// and do our color
	float offset =  (TIME * 2.0);
	vec4 col = texture(arrow_texture, vec2(UV.x, (UV.y * length * 4.0) + offset )).rgba;
	ALBEDO = col.rgb * mix_color.rgb;
	
	// need to fix up our image and add an alpha channel
	ALPHA = col.a;
}
