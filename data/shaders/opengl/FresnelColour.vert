// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

#include "attributes.glsl"
#include "lib.glsl"

out vec3 varyingEyepos;
out vec3 varyingNormal;

void main(void)
{
	gl_Position = matrixTransform();

	varyingEyepos = vec3(uViewMatrix * a_vertex);
	varyingNormal = normalize(normalMatrix() * a_normal);
}
