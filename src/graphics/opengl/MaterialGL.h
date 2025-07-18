// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

#ifndef _OGL_MATERIAL_H
#define _OGL_MATERIAL_H
/*
 * Multi-purpose OGL material.
 *
 * Generally, the idea is that a Program contains uniforms but
 * a material sets them, using the standard parameters of Graphics::Material
 * or whatever necessary to achieve an effect
 *
 * Programs are owned by the renderer, since they are shared between materials.
 */
#include "OpenGLLibs.h"
#include "graphics/Material.h"
#include "graphics/opengl/UniformBuffer.h"

#include <memory>

namespace Graphics {

	class RendererOGL;

	namespace OGL {

		class CommandList;
		class Shader;
		class Program;
		class UniformBuffer;

		class Material : public Graphics::Material {
		public:
			Material() :
				m_shader(nullptr),
				m_activeVariant(nullptr),
				m_vertexState(0),
				m_vertexFormatHash(0) {}

			bool IsProgramLoaded() const final;
			virtual void SetShader(Shader *p);
			virtual const Shader *GetShader() const { return m_shader; }

			bool SetTexture(size_t name, Texture *tex) override;

			bool SetBufferDynamic(size_t name, void *buffer, size_t size) override;
			bool SetBuffer(size_t name, BufferBinding<Graphics::UniformBuffer> ub) override;

			bool SetPushConstant(size_t name, int i) override;
			bool SetPushConstant(size_t name, float f) override;
			bool SetPushConstant(size_t name, vector3f v3) override;
			bool SetPushConstant(size_t name, vector3f v4, float f4) override;
			bool SetPushConstant(size_t name, Color c) override;
			bool SetPushConstant(size_t name, matrix3x3f mat3) override;
			bool SetPushConstant(size_t name, matrix4x4f mat4) override;

		protected:
			friend class Graphics::RendererOGL;
			friend class OGL::CommandList;
			void Copy(OGL::Material *to) const;
			Program *EvaluateVariant();
			void UpdateDrawData();

			Shader *m_shader;
			Program *m_activeVariant;
			RendererOGL *m_renderer;
			GLuint m_vertexState; // vertex array object corresponding to the material's vertex format
			size_t m_vertexFormatHash;

			uint32_t m_perDrawBinding;

			std::unique_ptr<char[]> m_pushConstants;
			std::unique_ptr<Texture *[]> m_textureBindings;
			std::unique_ptr<BufferBinding<UniformBuffer>[]> m_bufferBindings;
		};
	} // namespace OGL
} // namespace Graphics
#endif
