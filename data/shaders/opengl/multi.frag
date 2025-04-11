// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

#include "attributes.glsl"
#include "lib.glsl"
#include "sgaussian.glsl"

#ifdef TEXTURE0
uniform sampler2D texture0; //diffuse
uniform sampler2D texture1; //specular | roughness/metal/ao
uniform sampler2D texture2; //glow
uniform sampler2D texture3; //ambient
uniform sampler2D texture4; //pattern
uniform sampler2D texture5; //color
uniform sampler2D texture6; //normal
uniform samplerCube texture7; //irradiance IBL

in vec2 texCoord0;
#endif

#ifdef VERTEXCOLOR
in vec4 vertexColor;
#endif
#if (NUM_LIGHTS > 0)
in vec3 eyePos;
in vec3 normal;
#ifdef MAP_NORMAL
	in vec3 tangent;
	//in vec3 bitangent;
#endif
#ifdef HEAT_COLOURING
	uniform sampler2D heatGradient;
#endif // HEAT_COLOURING
#endif // (NUM_LIGHTS > 0)

// Use raw OpenGL uniforms here similar to push constants,
// as a payload for small per-draw data that would otherwise
// have to go in a separate UBO
uniform vec4 lightIntensity;
uniform vec4 heatingNormal;

out vec4 frag_color;

void getSurface(inout Surface surf)
{
#ifdef VERTEXCOLOR
	surf.color = vertexColor;
#else
	surf.color = material.diffuse;
#endif
#ifdef TEXTURE0
	surf.color *= texture(texture0, texCoord0);
#endif

	//patterns - simple lookup
#ifdef MAP_COLOR
	vec4 pat = texture(texture4, texCoord0);
	vec4 mapColor = texture(texture5, vec2(pat.r, 0.0));
	vec4 tint = mix(vec4(1.0),mapColor,pat.a);
	surf.color *= tint;
#endif

	surf.shininess = material.shininess;
	surf.specular = material.specular.xyz;

#ifdef MAP_SPECULAR
	vec3 fxmap = texture(texture1, texCoord0).rgb;
#ifdef USE_PBR
	// convert specular into metallic and roughness
	// this is 
	surf.metallic = max(fxmap.x - 0.65, 0.0);
	surf.roughness = (1.0 - fxmap.r) * (200.0 - material.shininess) / 180.0;
#else
	surf.specular *= fxmap;
#endif
#endif

#ifdef USE_PBR
#ifdef MAP_PBR
	vec3 pbrMap = texture(texture1, texCoord0).xyz;
	// first channel (red) of pbr map is roughness of the material
	surf.roughness = pbrMap.r;
	// second channel (green) of pbr map is how metallic a material is
	surf.metallic = pbrMap.g;
#endif
    surf.specular = mix(vec3(0.04), surf.color.xyz, surf.metallic);
#endif

#if (NUM_LIGHTS > 0)
//directional lighting
#ifdef MAP_NORMAL
	vec3 bump = (texture(texture6, texCoord0).xyz * 2.0) - vec3(1.0);
	vec3 N = normalize(normal);
	vec3 T = normalize(tangent);
	vec3 bitangent = cross(N, T);
	mat3 tangentFrame = mat3(T, bitangent, N);
	surf.normal = tangentFrame * bump;
#else
	surf.normal = normal;
#endif
#else
	// pointing directly at the viewer
	surf.normal = vec3(0, 0, 1);
#endif
	surf.normal = normalize(surf.normal);

	// this is crude "baked ambient occulsion" - basically multiply everything by the ambient texture
	// scaling whatever we've decided the lighting contribution is by 0.0 to 1.0 to account for sheltered/hidden surfaces
#ifdef MAP_AMBIENT
	surf.ambientOcclusion = texture(texture3, texCoord0).r;
#else
	surf.ambientOcclusion = 1.0;
#endif

	//emissive only make sense with lighting
#ifdef MAP_EMISSIVE
	// HACK emissive maps are authored weirdly, mostly under the assumption that they will be multiplied by diffuse
	// We mix a litle towards white to make the emissives pop more
	surf.emissive = mix(surf.color.xyz, vec3(1.0), 0.4) * texture(texture2, texCoord0).xyz; //glow map
#else
	surf.emissive = material.emission.xyz; //just emissive parameter
#endif
}

void main(void)
{
	// initialize here to prevent warnings about possibly-unused variables
	Surface surface = Surface(vec4(1.0), vec3(1.0), 100.0, vec3(0, 0, 1), vec3(0), 1.0, 0.5, 0);
	getSurface(surface);

#ifdef ALPHA_TEST
	if (surface.color.a < 0.5)
		discard;
#endif

//directional lighting
#if (NUM_LIGHTS > 0)
	// ambient only make sense with lighting
	vec3 diffuse = scene.ambient.xyz;
	vec3 ambient = diffuse;
	vec3 V = normalize(-eyePos);

#ifdef USE_PBR
	// add hemisphere lighting
	// don't allow aNdotV to be less than 0.1 otherwise black artifacts occur
	float aNdotV = max(dot(V, surface.normal) , 0.1);
	vec3 aSpecFresnel = fresnelSchlick(aNdotV, surface.specular);
	float aSpec = (1.0 - aNdotV);
	float aG = GeometrySchlickSmithGGX(aNdotV, aNdotV, surface.roughness);
	// for reflected light NdotH is always 1 but this produces to much specular
	// limit NdotH to range 0.96 to 0.97
	float aNdotH = 0.010 * aSpec + 0.960;
	float aD = DistributionGGX(aNdotH, surface.roughness);
	// add hemishpere specular at glancing angles for all surfaces including glass
	// glass surfaces are those that have an alpha less than 1.0
	float glassAlpha = (1.0 - surface.color.a);
	float glassFresnel =  (aSpec + 0.25 * aG) * min(glassAlpha * 10.0, 0.2);
	float aNDA = 0.25 * (aNdotV * aG * aD);
	vec3 hemiSpec = ambient * (aSpecFresnel * aNDA + glassFresnel);
	surface.color.a += glassAlpha * aSpec;

	// fresnel and metal effect for hemi diffuse
	diffuse *= (1.0 - aSpecFresnel) * (1.0 - surface.metallic);
	// hemisphere diffuse  (multiple lights) 
	// aSpec provides back lighting from left, right, bottom, and top
	// aNdotV provides view orientated lighting
	// surface.normal.y is light from above
	// 2 * aSpec + aNdotV = 2 - aNdotV
	diffuse *= 0.5 * (2.0 - aNdotV + max(surface.normal.y, 0.0));

#endif
	// fresnel effect for diffuse

	vec3 specular = vec3(0.0);
	float intensity[4] = float[](
		lightIntensity.x,
		lightIntensity.y,
		lightIntensity.z,
		lightIntensity.w
	);

	for (int i=0; i<NUM_LIGHTS; ++i) {
#ifdef USE_PBR
		PbrDirectionalLight(uLight[i], intensity[i], surface, V, diffuse, specular);
#else
		BlinnPhongDirectionalLight(uLight[i], intensity[i], surface, V, diffuse, specular);
#endif
	}

#if 0
	// sturnclaw: toying with spherical gaussian indirect lighting.
	// Enable the ifdef and change the values below to play with it.
	SGaussian cosineLobe = CosineLobeSG(surface.normal);

	SGaussian lightingLobe;
	lightingLobe.Axis = mat3(uViewMatrix) * normalize(vec3(0, 1, 0.5));
	lightingLobe.Amplitude = vec3(1.0, 0.75, 0.6) / vec3(2.5);
	lightingLobe.Sharpness = 8.0;

	SGaussian atmosphereLobe;
	atmosphereLobe.Axis = mat3(uViewMatrix) * normalize(vec3(0, 1, 0.0));
	atmosphereLobe.Amplitude = vec3(1.0, 1.0, 1.0) / vec3(5.0);
	atmosphereLobe.Sharpness = 1;

	vec3 brdf = vec3(1) / PI;
	//diffuse += brdf * max(SGInnerProduct(lightingLobe, cosineLobe), 0.0);
	diffuse += brdf * max(SGInnerProduct(atmosphereLobe, cosineLobe), 0.0);
#endif

	vec3 final_color = (diffuse * surface.color.xyz) * surface.ambientOcclusion + specular;

#ifdef USE_PBR
	final_color += hemiSpec;
	// gamma correction
	//final_color *= (2.6 / PI);
#endif
	// emmission
	final_color += (vec3(1.0) - scene.ambient.xyz) * surface.emissive * 1.5;
	frag_color = vec4(final_color, surface.color.w);

#ifdef HEAT_COLOURING
	if (heatingNormal.a > 0.0)
	{
		float dphNn = clamp(dot(heatingNormal.xyz, surface.normal), 0.0, 1.0);
		float heatDot = heatingNormal.a * (dphNn * dphNn * dphNn);
		vec4 heatColour = texture(heatGradient, vec2(heatDot, 0.5)); //heat gradient blend
		frag_color.rgb = frag_color.rgb + heatColour.rgb; // override surface color based on heat color
	}
#endif // HEAT_COLOURING

#else
	frag_color = surface.color;
#endif
}
