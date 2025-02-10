// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

#include "attributes.glsl"
#include "lib.glsl"

uniform sampler2D texture0; //diffuse
in vec2 texCoord0;

out vec4 frag_color;

void main(void)
{
	// emission.r is the thruster power setting
	// power setting effects the brightness of the flame
	float brightness = 0.5 + material.emission.r * 0.5;
	// emission.a is the flicker value which is limited to 5%
	float flicker = 0.95 + (material.emission.a * 0.05);
	vec4 color = material.diffuse * flicker * brightness;

	color *= texture(texture0, texCoord0);
	frag_color = color;
}
