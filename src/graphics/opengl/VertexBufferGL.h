// Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
// Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

#pragma once

#include "graphics/Types.h"
#include "graphics/VertexBuffer.h"
#include "graphics/opengl/GLBufferBase.h"

#include <memory>

namespace Graphics {
	class RendererOGL;

	namespace OGL {

		class VertexBuffer : public Graphics::VertexBuffer, public GLBufferBase {
		public:
			VertexBuffer(BufferUsage usage, uint32_t numVertices, uint32_t stride);
			~VertexBuffer();

			void Unmap() override;

			void UnmapRange(bool flush) override;

			void FlushRange(size_t, size_t) override;

			// change the buffer data without mapping
			void BufferData(const size_t, void *) final;

			// release the underlying GPU memory and recreate
			void Reset() final;

			void Bind() final;
			void Release() final;

		protected:
			uint8_t *MapInternal(BufferMapMode) override;
			uint8_t *MapRangeInternal(BufferMapMode, size_t, size_t) override;
			uint8_t *m_data;

			size_t m_mapStart;
			size_t m_mapLength;
		};

		class IndexBuffer : public Graphics::IndexBuffer, public GLBufferBase {
		public:
			IndexBuffer(Uint32 size, BufferUsage, IndexBufferSize);
			~IndexBuffer();

			Uint32 *Map(BufferMapMode) final;
			Uint16 *Map16(BufferMapMode) final;
			void Unmap() final;

			// change the buffer data without mapping
			void BufferData(const size_t, void *) final;

			void Bind() final;
			void Release() final;

		private:
			Uint32 *m_data;
			Uint16 *m_data16;
		};

		class MeshObject final : public Graphics::MeshObject {
		public:
			MeshObject(const Graphics::VertexFormatDesc &fmt, Graphics::VertexBuffer *vtx, Graphics::IndexBuffer *idx);
			~MeshObject() override;

			void Bind() override;
			void Release() override;

			Graphics::VertexBuffer *GetVertexBuffer() const override { return m_vtxBuffer.Get(); };
			Graphics::IndexBuffer *GetIndexBuffer() const override { return m_idxBuffer.Get(); };
			const Graphics::VertexFormatDesc &GetFormat() const override { return m_format; }

		protected:
			friend class Graphics::RendererOGL;

			GLuint GetVertexArrayObject() const { return m_vao; }
			RefCountedPtr<VertexBuffer> m_vtxBuffer;
			RefCountedPtr<IndexBuffer> m_idxBuffer;
			GLuint m_vao = 0;
			VertexFormatDesc m_format;
		};

		GLuint BuildVAOFromDesc(const Graphics::VertexFormatDesc desc);

	} // namespace OGL
} // namespace Graphics
