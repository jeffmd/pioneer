// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

// #extension GL_ARB_gpu_shader5 : enable

#include "attributes.glsl"
#include "lib.glsl"

out vec2 texCoord0;

void main(void)
{
	// emission.r is the the thruster power setting
	// the power setting controls the length of the flame
	// flame length can range from 25% to 100%
	float flame_length = 0.25 + (material.emission.r * 0.75);
	// emission.a is the flicker/modulation value which is limited to 5%
	float size = 0.95 + (material.emission.a * 0.05);
	vec4 scale = vec4(size, size, size * flame_length, 1.0);
	
	gl_Position = uViewProjectionMatrix * (a_vertex * scale);
	texCoord0 = a_uv0.xy;
}
