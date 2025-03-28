// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

#include "sRGB.glsl"

float remap01(float v_min, float v_max, float val)
{
	return clamp((val - v_min) / (v_max - v_min), 0.0, 1.0);
}

// Simple ray-sphere intersection test, assuming ray starts at origin and rayDir is pre-normalized.
// Returns distance to first and second intersections in {x, y} or 0.0 if no intersection.
vec2 raySphereIntersect(in vec3 sphereCenter, in vec3 rayDir, in float radius)
{
	vec3 v = -sphereCenter;
	float b = -dot(v, rayDir);
	float det = (b * b) - dot(v, v) + (radius * radius);
	float sdet = sqrt(det);

	return det > 0.0 ? max(vec2(b - sdet, b + sdet), vec2(0.0)) : vec2(0.0);
}

// ray starts at origin, rayDir and axis are pre-normalized
// Returns distance to first and second intersections in {x, y} or 0.0 if no intersection.
vec2 rayCylinderIntersect(in vec3 rayDir, in vec3 cylinderCenter, in vec3 axis, in float radius)
{
    // tangent vectors (parallel to axis)
    vec3 tray = axis * dot(rayDir, axis);
    vec3 tcenter = axis * dot(cylinderCenter, axis);

    // normal vectors (perpendicular to axis)
    vec3 nray = rayDir - tray;
    vec3 ncenter = cylinderCenter - tcenter;

    // coefficient to move from projection to actual 3d space
    // e.g. if angle between axis and tray = 30deg, actual intersect should be doubled
    float scale = length(nray);

    // intersection given in main plane projection
    vec2 intersect = raySphereIntersect(ncenter, normalize(nray), radius);

    return (scale == 0.f) ? vec2(0.f) : intersect / scale;
}

// Phase functions
// https://www.scratchapixel.com/lessons/procedural-generation-virtual-worlds/simulating-sky/simulating-colors-of-the-sky.html
float miePhaseFunction(const float g, const float mu)
{
	/*
	 * Mie phase function:
	 */
	return 3.f / (8.f * 3.141592) * ((1.f - g * g) * (1.f + mu * mu)) / ((2.f + g * g) * pow(1.f + g * g - 2.f * g * mu, 1.5f));
}

float rayleighPhaseFunction(const float mu)
{
	/*
	 * Rayleigh phase function:
	 */
	return 3.f / (16.f * 3.141592) * (1 + mu * mu);
}

#ifdef FRAGMENT_SHADER

struct Surface {
	vec4 color;
	vec3 specular;
	float shininess;
	vec3 normal;
	vec3 emissive;
	float ambientOcclusion;
	float roughness;
	float metallic;
};


float FastHemiSphereLight(in vec3 normal)
{
	// normal must be in view space
    float intensity = 0.0;

	// use 6 lights that form a hemisphere orientated in view space
	float right = normal.z + normal.x;
	float left = normal.z - normal.x;

    intensity += max(normal.y + right, 0.0); // front top right
    intensity += max(normal.y + left, 0.0); // front top left
    intensity += max(normal.y - left, 0.0); // back top right
    intensity += max(normal.y - right, 0.0); // back top left
    intensity += max(normal.y, 0.0); // top
    intensity += max(normal.z, 0.0); // front
    intensity += max(normal.x, 0.0); // right
    intensity += max(-normal.x, 0.0); // left
    intensity *= 0.4;

	return intensity;
}

// controls specular reflection
// normal distribution function
float DistributionGGX(vec3 N, vec3 H, float a)
{
	float a2 = a * a;
	a2 *= a2;
	float NdotH = max(dot(N, H), 0.0);
	float base = NdotH * NdotH * (a2 - 1.0) + 1.001;
	base = /*PI */ base * base;

	return (a2 + 0.01) / base;
}

vec3 fresnelSchlick(float HdotV, vec3 F0)
{
	return F0 + (1.0 - F0) * pow(1.0001 - HdotV, 5.0);
}

float GeometrySchlickSmithGGX(float NdotL, float NdotV, float roughness)
{
    float k = roughness + 1.0;
    k = (k * k) / 8.0;
    float ggx2  = NdotV * (1.0 - k) + k;
    float ggx1  = NdotL * (1.0 - k) + k;

    return /*(NdotV * NdotL)*/1.0 / (ggx1 * ggx2);
}

void PbrDirectionalLight(in Light light, in float intensity, in Surface surf, in vec3 fragPos, inout vec3 diffuse, inout vec3 specular)
{
	// This code calculates directional lights
	vec3 L = normalize(light.position.xyz); // surface->light vector
	vec3 V = normalize(-fragPos); // surface->eye vector
	vec3 H = normalize(L + V); // halfway vector
	vec3 N = surf.normal;

	vec3 specFresnel = fresnelSchlick(max(dot(H, V), 0.0), surf.specular);

	// modified cook-torrance specular brdf
	float D = DistributionGGX(N, H, surf.roughness);
	float NdotL = max(dot(N, L), 0.0);
	float NdotV = max(dot(N, V), 0.0);
	float G = GeometrySchlickSmithGGX(NdotL, NdotV, surf.roughness);
    vec3 DGF = D * G * specFresnel;
	// add to outgoing specular radiance
	float denom = 4.0 /* max(NdotL * NdotV, 0.01)*/;
	specular += (DGF / denom) * light.specular.xyz * intensity * NdotL;

	// fresnel effect for diffuse
	vec3 kD = vec3(1.0) - specFresnel;
	kD *= 1.0 - surf.metallic;
	// add to outgoing diffuse radiance
	diffuse += kD * light.diffuse.xyz * intensity * NdotL /* (1.0 / PI)*/;
}

// Currently used by: hopefully everything
// Evaluates a standard blinn-phong diffuse+specular, with the addition of a
// light intensity term to scale the lighting contribution based on (pre-calculated)
// global occlusion
void BlinnPhongDirectionalLight(in Light light, in float intensity, in Surface surf, in vec3 fragPos, inout vec3 diffuse, inout vec3 specular)
{
	// This code calculates directional lights
	vec3 L = normalize(light.position.xyz); // surface->light vector
	vec3 V = normalize(-fragPos); // surface->eye vector
	vec3 H = normalize(L + V); // halfway vector
	diffuse += light.diffuse.xyz * intensity * max(dot(L, surf.normal), 0.0);
	specular += surf.specular * light.specular.xyz * intensity * pow(max(dot(H, surf.normal), 0.0), surf.shininess);
}

// Used by: geosphere shaders
// Calculate length*density product of a line through the atmosphere
// a - start coord (normalized relative to atmosphere radius)
// b - end coord " "
// centerDensity - atmospheric density at centre of sphere
// length - real length of line in meters
float AtmosLengthDensityProduct(vec3 a, vec3 b, float surfaceDensity, float len, float invScaleHeight)
{
	/* 4 samples */
	float ldprod = 0.0;

	vec3 dir = b-a;
	float ln = max(length(b)-1.0, 0.0);

	/* simple 6-tap raymarch through the atmosphere to sample an average density */
	ldprod = surfaceDensity * (
			exp(-invScaleHeight * (length(a)           -1.0)) +
			exp(-invScaleHeight * (length(a + 0.2*dir) -1.0)) +
			exp(-invScaleHeight * (length(a + 0.4*dir) -1.0)) +
			exp(-invScaleHeight * (length(a + 0.6*dir) -1.0)) +
			exp(-invScaleHeight * (length(a + 0.8*dir) -1.0)) +
			exp(-invScaleHeight * (ln)));

	ldprod *= len;
	return ldprod;
}

#endif
