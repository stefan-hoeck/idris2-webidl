// Extracted from https://www.khronos.org/registry/webgl/specs/latest/2.0/
//            and https://www.khronos.org/registry/webgl/specs/latest/1.0/

typedef long long GLint64;
typedef unsigned long long GLuint64;

[Exposed=(Window,Worker)]
interface WebGLQuery : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLSampler : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLSync : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLTransformFeedback : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLVertexArrayObject : WebGLObject {
};

typedef ([AllowShared] Uint32Array or sequence<GLuint>) Uint32List;

interface mixin WebGL2RenderingContextBase
{
  const GLenum READ_BUFFER                                   = 0x0C02;
  const GLenum UNPACK_ROW_LENGTH                             = 0x0CF2;
  const GLenum UNPACK_SKIP_ROWS                              = 0x0CF3;
  const GLenum UNPACK_SKIP_PIXELS                            = 0x0CF4;
  const GLenum PACK_ROW_LENGTH                               = 0x0D02;
  const GLenum PACK_SKIP_ROWS                                = 0x0D03;
  const GLenum PACK_SKIP_PIXELS                              = 0x0D04;
  const GLenum COLOR                                         = 0x1800;
  const GLenum DEPTH                                         = 0x1801;
  const GLenum STENCIL                                       = 0x1802;
  const GLenum RED                                           = 0x1903;
  const GLenum RGB8                                          = 0x8051;
  const GLenum RGBA8                                         = 0x8058;
  const GLenum RGB10_A2                                      = 0x8059;
  const GLenum TEXTURE_BINDING_3D                            = 0x806A;
  const GLenum UNPACK_SKIP_IMAGES                            = 0x806D;
  const GLenum UNPACK_IMAGE_HEIGHT                           = 0x806E;
  const GLenum TEXTURE_3D                                    = 0x806F;
  const GLenum TEXTURE_WRAP_R                                = 0x8072;
  const GLenum MAX_3D_TEXTURE_SIZE                           = 0x8073;
  const GLenum UNSIGNED_INT_2_10_10_10_REV                   = 0x8368;
  const GLenum MAX_ELEMENTS_VERTICES                         = 0x80E8;
  const GLenum MAX_ELEMENTS_INDICES                          = 0x80E9;
  const GLenum TEXTURE_MIN_LOD                               = 0x813A;
  const GLenum TEXTURE_MAX_LOD                               = 0x813B;
  const GLenum TEXTURE_BASE_LEVEL                            = 0x813C;
  const GLenum TEXTURE_MAX_LEVEL                             = 0x813D;
  const GLenum MIN                                           = 0x8007;
  const GLenum MAX                                           = 0x8008;
  const GLenum DEPTH_COMPONENT24                             = 0x81A6;
  const GLenum MAX_TEXTURE_LOD_BIAS                          = 0x84FD;
  const GLenum TEXTURE_COMPARE_MODE                          = 0x884C;
  const GLenum TEXTURE_COMPARE_FUNC                          = 0x884D;
  const GLenum CURRENT_QUERY                                 = 0x8865;
  const GLenum QUERY_RESULT                                  = 0x8866;
  const GLenum QUERY_RESULT_AVAILABLE                        = 0x8867;
  const GLenum STREAM_READ                                   = 0x88E1;
  const GLenum STREAM_COPY                                   = 0x88E2;
  const GLenum STATIC_READ                                   = 0x88E5;
  const GLenum STATIC_COPY                                   = 0x88E6;
  const GLenum DYNAMIC_READ                                  = 0x88E9;
  const GLenum DYNAMIC_COPY                                  = 0x88EA;
  const GLenum MAX_DRAW_BUFFERS                              = 0x8824;
  const GLenum DRAW_BUFFER0                                  = 0x8825;
  const GLenum DRAW_BUFFER1                                  = 0x8826;
  const GLenum DRAW_BUFFER2                                  = 0x8827;
  const GLenum DRAW_BUFFER3                                  = 0x8828;
  const GLenum DRAW_BUFFER4                                  = 0x8829;
  const GLenum DRAW_BUFFER5                                  = 0x882A;
  const GLenum DRAW_BUFFER6                                  = 0x882B;
  const GLenum DRAW_BUFFER7                                  = 0x882C;
  const GLenum DRAW_BUFFER8                                  = 0x882D;
  const GLenum DRAW_BUFFER9                                  = 0x882E;
  const GLenum DRAW_BUFFER10                                 = 0x882F;
  const GLenum DRAW_BUFFER11                                 = 0x8830;
  const GLenum DRAW_BUFFER12                                 = 0x8831;
  const GLenum DRAW_BUFFER13                                 = 0x8832;
  const GLenum DRAW_BUFFER14                                 = 0x8833;
  const GLenum DRAW_BUFFER15                                 = 0x8834;
  const GLenum MAX_FRAGMENT_UNIFORM_COMPONENTS               = 0x8B49;
  const GLenum MAX_VERTEX_UNIFORM_COMPONENTS                 = 0x8B4A;
  const GLenum SAMPLER_3D                                    = 0x8B5F;
  const GLenum SAMPLER_2D_SHADOW                             = 0x8B62;
  const GLenum FRAGMENT_SHADER_DERIVATIVE_HINT               = 0x8B8B;
  const GLenum PIXEL_PACK_BUFFER                             = 0x88EB;
  const GLenum PIXEL_UNPACK_BUFFER                           = 0x88EC;
  const GLenum PIXEL_PACK_BUFFER_BINDING                     = 0x88ED;
  const GLenum PIXEL_UNPACK_BUFFER_BINDING                   = 0x88EF;
  const GLenum FLOAT_MAT2x3                                  = 0x8B65;
  const GLenum FLOAT_MAT2x4                                  = 0x8B66;
  const GLenum FLOAT_MAT3x2                                  = 0x8B67;
  const GLenum FLOAT_MAT3x4                                  = 0x8B68;
  const GLenum FLOAT_MAT4x2                                  = 0x8B69;
  const GLenum FLOAT_MAT4x3                                  = 0x8B6A;
  const GLenum SRGB                                          = 0x8C40;
  const GLenum SRGB8                                         = 0x8C41;
  const GLenum SRGB8_ALPHA8                                  = 0x8C43;
  const GLenum COMPARE_REF_TO_TEXTURE                        = 0x884E;
  const GLenum RGBA32F                                       = 0x8814;
  const GLenum RGB32F                                        = 0x8815;
  const GLenum RGBA16F                                       = 0x881A;
  const GLenum RGB16F                                        = 0x881B;
  const GLenum VERTEX_ATTRIB_ARRAY_INTEGER                   = 0x88FD;
  const GLenum MAX_ARRAY_TEXTURE_LAYERS                      = 0x88FF;
  const GLenum MIN_PROGRAM_TEXEL_OFFSET                      = 0x8904;
  const GLenum MAX_PROGRAM_TEXEL_OFFSET                      = 0x8905;
  const GLenum MAX_VARYING_COMPONENTS                        = 0x8B4B;
  const GLenum TEXTURE_2D_ARRAY                              = 0x8C1A;
  const GLenum TEXTURE_BINDING_2D_ARRAY                      = 0x8C1D;
  const GLenum R11F_G11F_B10F                                = 0x8C3A;
  const GLenum UNSIGNED_INT_10F_11F_11F_REV                  = 0x8C3B;
  const GLenum RGB9_E5                                       = 0x8C3D;
  const GLenum UNSIGNED_INT_5_9_9_9_REV                      = 0x8C3E;
  const GLenum TRANSFORM_FEEDBACK_BUFFER_MODE                = 0x8C7F;
  const GLenum MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS    = 0x8C80;
  const GLenum TRANSFORM_FEEDBACK_VARYINGS                   = 0x8C83;
  const GLenum TRANSFORM_FEEDBACK_BUFFER_START               = 0x8C84;
  const GLenum TRANSFORM_FEEDBACK_BUFFER_SIZE                = 0x8C85;
  const GLenum TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN         = 0x8C88;
  const GLenum RASTERIZER_DISCARD                            = 0x8C89;
  const GLenum MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = 0x8C8A;
  const GLenum MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS       = 0x8C8B;
  const GLenum INTERLEAVED_ATTRIBS                           = 0x8C8C;
  const GLenum SEPARATE_ATTRIBS                              = 0x8C8D;
  const GLenum TRANSFORM_FEEDBACK_BUFFER                     = 0x8C8E;
  const GLenum TRANSFORM_FEEDBACK_BUFFER_BINDING             = 0x8C8F;
  const GLenum RGBA32UI                                      = 0x8D70;
  const GLenum RGB32UI                                       = 0x8D71;
  const GLenum RGBA16UI                                      = 0x8D76;
  const GLenum RGB16UI                                       = 0x8D77;
  const GLenum RGBA8UI                                       = 0x8D7C;
  const GLenum RGB8UI                                        = 0x8D7D;
  const GLenum RGBA32I                                       = 0x8D82;
  const GLenum RGB32I                                        = 0x8D83;
  const GLenum RGBA16I                                       = 0x8D88;
  const GLenum RGB16I                                        = 0x8D89;
  const GLenum RGBA8I                                        = 0x8D8E;
  const GLenum RGB8I                                         = 0x8D8F;
  const GLenum RED_INTEGER                                   = 0x8D94;
  const GLenum RGB_INTEGER                                   = 0x8D98;
  const GLenum RGBA_INTEGER                                  = 0x8D99;
  const GLenum SAMPLER_2D_ARRAY                              = 0x8DC1;
  const GLenum SAMPLER_2D_ARRAY_SHADOW                       = 0x8DC4;
  const GLenum SAMPLER_CUBE_SHADOW                           = 0x8DC5;
  const GLenum UNSIGNED_INT_VEC2                             = 0x8DC6;
  const GLenum UNSIGNED_INT_VEC3                             = 0x8DC7;
  const GLenum UNSIGNED_INT_VEC4                             = 0x8DC8;
  const GLenum INT_SAMPLER_2D                                = 0x8DCA;
  const GLenum INT_SAMPLER_3D                                = 0x8DCB;
  const GLenum INT_SAMPLER_CUBE                              = 0x8DCC;
  const GLenum INT_SAMPLER_2D_ARRAY                          = 0x8DCF;
  const GLenum UNSIGNED_INT_SAMPLER_2D                       = 0x8DD2;
  const GLenum UNSIGNED_INT_SAMPLER_3D                       = 0x8DD3;
  const GLenum UNSIGNED_INT_SAMPLER_CUBE                     = 0x8DD4;
  const GLenum UNSIGNED_INT_SAMPLER_2D_ARRAY                 = 0x8DD7;
  const GLenum DEPTH_COMPONENT32F                            = 0x8CAC;
  const GLenum DEPTH32F_STENCIL8                             = 0x8CAD;
  const GLenum FLOAT_32_UNSIGNED_INT_24_8_REV                = 0x8DAD;
  const GLenum FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING         = 0x8210;
  const GLenum FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE         = 0x8211;
  const GLenum FRAMEBUFFER_ATTACHMENT_RED_SIZE               = 0x8212;
  const GLenum FRAMEBUFFER_ATTACHMENT_GREEN_SIZE             = 0x8213;
  const GLenum FRAMEBUFFER_ATTACHMENT_BLUE_SIZE              = 0x8214;
  const GLenum FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE             = 0x8215;
  const GLenum FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE             = 0x8216;
  const GLenum FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE           = 0x8217;
  const GLenum FRAMEBUFFER_DEFAULT                           = 0x8218;
  const GLenum UNSIGNED_INT_24_8                             = 0x84FA;
  const GLenum DEPTH24_STENCIL8                              = 0x88F0;
  const GLenum UNSIGNED_NORMALIZED                           = 0x8C17;
  const GLenum DRAW_FRAMEBUFFER_BINDING                      = 0x8CA6; /* Same as FRAMEBUFFER_BINDING */
  const GLenum READ_FRAMEBUFFER                              = 0x8CA8;
  const GLenum DRAW_FRAMEBUFFER                              = 0x8CA9;
  const GLenum READ_FRAMEBUFFER_BINDING                      = 0x8CAA;
  const GLenum RENDERBUFFER_SAMPLES                          = 0x8CAB;
  const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER          = 0x8CD4;
  const GLenum MAX_COLOR_ATTACHMENTS                         = 0x8CDF;
  const GLenum COLOR_ATTACHMENT1                             = 0x8CE1;
  const GLenum COLOR_ATTACHMENT2                             = 0x8CE2;
  const GLenum COLOR_ATTACHMENT3                             = 0x8CE3;
  const GLenum COLOR_ATTACHMENT4                             = 0x8CE4;
  const GLenum COLOR_ATTACHMENT5                             = 0x8CE5;
  const GLenum COLOR_ATTACHMENT6                             = 0x8CE6;
  const GLenum COLOR_ATTACHMENT7                             = 0x8CE7;
  const GLenum COLOR_ATTACHMENT8                             = 0x8CE8;
  const GLenum COLOR_ATTACHMENT9                             = 0x8CE9;
  const GLenum COLOR_ATTACHMENT10                            = 0x8CEA;
  const GLenum COLOR_ATTACHMENT11                            = 0x8CEB;
  const GLenum COLOR_ATTACHMENT12                            = 0x8CEC;
  const GLenum COLOR_ATTACHMENT13                            = 0x8CED;
  const GLenum COLOR_ATTACHMENT14                            = 0x8CEE;
  const GLenum COLOR_ATTACHMENT15                            = 0x8CEF;
  const GLenum FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            = 0x8D56;
  const GLenum MAX_SAMPLES                                   = 0x8D57;
  const GLenum HALF_FLOAT                                    = 0x140B;
  const GLenum RG                                            = 0x8227;
  const GLenum RG_INTEGER                                    = 0x8228;
  const GLenum R8                                            = 0x8229;
  const GLenum RG8                                           = 0x822B;
  const GLenum R16F                                          = 0x822D;
  const GLenum R32F                                          = 0x822E;
  const GLenum RG16F                                         = 0x822F;
  const GLenum RG32F                                         = 0x8230;
  const GLenum R8I                                           = 0x8231;
  const GLenum R8UI                                          = 0x8232;
  const GLenum R16I                                          = 0x8233;
  const GLenum R16UI                                         = 0x8234;
  const GLenum R32I                                          = 0x8235;
  const GLenum R32UI                                         = 0x8236;
  const GLenum RG8I                                          = 0x8237;
  const GLenum RG8UI                                         = 0x8238;
  const GLenum RG16I                                         = 0x8239;
  const GLenum RG16UI                                        = 0x823A;
  const GLenum RG32I                                         = 0x823B;
  const GLenum RG32UI                                        = 0x823C;
  const GLenum VERTEX_ARRAY_BINDING                          = 0x85B5;
  const GLenum R8_SNORM                                      = 0x8F94;
  const GLenum RG8_SNORM                                     = 0x8F95;
  const GLenum RGB8_SNORM                                    = 0x8F96;
  const GLenum RGBA8_SNORM                                   = 0x8F97;
  const GLenum SIGNED_NORMALIZED                             = 0x8F9C;
  const GLenum COPY_READ_BUFFER                              = 0x8F36;
  const GLenum COPY_WRITE_BUFFER                             = 0x8F37;
  const GLenum COPY_READ_BUFFER_BINDING                      = 0x8F36; /* Same as COPY_READ_BUFFER */
  const GLenum COPY_WRITE_BUFFER_BINDING                     = 0x8F37; /* Same as COPY_WRITE_BUFFER */
  const GLenum UNIFORM_BUFFER                                = 0x8A11;
  const GLenum UNIFORM_BUFFER_BINDING                        = 0x8A28;
  const GLenum UNIFORM_BUFFER_START                          = 0x8A29;
  const GLenum UNIFORM_BUFFER_SIZE                           = 0x8A2A;
  const GLenum MAX_VERTEX_UNIFORM_BLOCKS                     = 0x8A2B;
  const GLenum MAX_FRAGMENT_UNIFORM_BLOCKS                   = 0x8A2D;
  const GLenum MAX_COMBINED_UNIFORM_BLOCKS                   = 0x8A2E;
  const GLenum MAX_UNIFORM_BUFFER_BINDINGS                   = 0x8A2F;
  const GLenum MAX_UNIFORM_BLOCK_SIZE                        = 0x8A30;
  const GLenum MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS        = 0x8A31;
  const GLenum MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS      = 0x8A33;
  const GLenum UNIFORM_BUFFER_OFFSET_ALIGNMENT               = 0x8A34;
  const GLenum ACTIVE_UNIFORM_BLOCKS                         = 0x8A36;
  const GLenum UNIFORM_TYPE                                  = 0x8A37;
  const GLenum UNIFORM_SIZE                                  = 0x8A38;
  const GLenum UNIFORM_BLOCK_INDEX                           = 0x8A3A;
  const GLenum UNIFORM_OFFSET                                = 0x8A3B;
  const GLenum UNIFORM_ARRAY_STRIDE                          = 0x8A3C;
  const GLenum UNIFORM_MATRIX_STRIDE                         = 0x8A3D;
  const GLenum UNIFORM_IS_ROW_MAJOR                          = 0x8A3E;
  const GLenum UNIFORM_BLOCK_BINDING                         = 0x8A3F;
  const GLenum UNIFORM_BLOCK_DATA_SIZE                       = 0x8A40;
  const GLenum UNIFORM_BLOCK_ACTIVE_UNIFORMS                 = 0x8A42;
  const GLenum UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES          = 0x8A43;
  const GLenum UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER     = 0x8A44;
  const GLenum UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER   = 0x8A46;
  const GLenum INVALID_INDEX                                 = 0xFFFFFFFF;
  const GLenum MAX_VERTEX_OUTPUT_COMPONENTS                  = 0x9122;
  const GLenum MAX_FRAGMENT_INPUT_COMPONENTS                 = 0x9125;
  const GLenum MAX_SERVER_WAIT_TIMEOUT                       = 0x9111;
  const GLenum OBJECT_TYPE                                   = 0x9112;
  const GLenum SYNC_CONDITION                                = 0x9113;
  const GLenum SYNC_STATUS                                   = 0x9114;
  const GLenum SYNC_FLAGS                                    = 0x9115;
  const GLenum SYNC_FENCE                                    = 0x9116;
  const GLenum SYNC_GPU_COMMANDS_COMPLETE                    = 0x9117;
  const GLenum UNSIGNALED                                    = 0x9118;
  const GLenum SIGNALED                                      = 0x9119;
  const GLenum ALREADY_SIGNALED                              = 0x911A;
  const GLenum TIMEOUT_EXPIRED                               = 0x911B;
  const GLenum CONDITION_SATISFIED                           = 0x911C;
  const GLenum WAIT_FAILED                                   = 0x911D;
  const GLenum SYNC_FLUSH_COMMANDS_BIT                       = 0x00000001;
  const GLenum VERTEX_ATTRIB_ARRAY_DIVISOR                   = 0x88FE;
  const GLenum ANY_SAMPLES_PASSED                            = 0x8C2F;
  const GLenum ANY_SAMPLES_PASSED_CONSERVATIVE               = 0x8D6A;
  const GLenum SAMPLER_BINDING                               = 0x8919;
  const GLenum RGB10_A2UI                                    = 0x906F;
  const GLenum INT_2_10_10_10_REV                            = 0x8D9F;
  const GLenum TRANSFORM_FEEDBACK                            = 0x8E22;
  const GLenum TRANSFORM_FEEDBACK_PAUSED                     = 0x8E23;
  const GLenum TRANSFORM_FEEDBACK_ACTIVE                     = 0x8E24;
  const GLenum TRANSFORM_FEEDBACK_BINDING                    = 0x8E25;
  const GLenum TEXTURE_IMMUTABLE_FORMAT                      = 0x912F;
  const GLenum MAX_ELEMENT_INDEX                             = 0x8D6B;
  const GLenum TEXTURE_IMMUTABLE_LEVELS                      = 0x82DF;

  const GLint64 TIMEOUT_IGNORED                              = -1;

  /* WebGL-specific enums */
  const GLenum MAX_CLIENT_WAIT_TIMEOUT_WEBGL                 = 0x9247;

  /* Buffer objects */
  undefined copyBufferSubData(GLenum readTarget, GLenum writeTarget, GLintptr readOffset,
                              GLintptr writeOffset, GLsizeiptr size);
  // MapBufferRange, in particular its read-only and write-only modes,
  // can not be exposed safely to JavaScript. GetBufferSubData
  // replaces it for the purpose of fetching data back from the GPU.
  undefined getBufferSubData(GLenum target, GLintptr srcByteOffset, [AllowShared] ArrayBufferView dstBuffer,
                             optional GLuint dstOffset = 0, optional GLuint length = 0);

  /* Framebuffer objects */
  undefined blitFramebuffer(GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0,
                            GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter);
  undefined framebufferTextureLayer(GLenum target, GLenum attachment, WebGLTexture? texture, GLint level,
                                    GLint layer);
  undefined invalidateFramebuffer(GLenum target, sequence<GLenum> attachments);
  undefined invalidateSubFramebuffer(GLenum target, sequence<GLenum> attachments,
                                     GLint x, GLint y, GLsizei width, GLsizei height);
  undefined readBuffer(GLenum src);

  /* Renderbuffer objects */
  any getInternalformatParameter(GLenum target, GLenum internalformat, GLenum pname);
  undefined renderbufferStorageMultisample(GLenum target, GLsizei samples, GLenum internalformat,
                                           GLsizei width, GLsizei height);

  /* Texture objects */
  undefined texStorage2D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width,
                         GLsizei height);
  undefined texStorage3D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width,
                         GLsizei height, GLsizei depth);

  undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLsizei depth, GLint border, GLenum format, GLenum type, GLintptr pboOffset);
  undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLsizei depth, GLint border, GLenum format, GLenum type,
                       TexImageSource source); // May throw DOMException
  undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLsizei depth, GLint border, GLenum format, GLenum type, [AllowShared] ArrayBufferView? srcData);
  undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLsizei depth, GLint border, GLenum format, GLenum type, [AllowShared] ArrayBufferView srcData,
                       GLuint srcOffset);

  undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                          GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                          GLintptr pboOffset);
  undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                          GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                          TexImageSource source); // May throw DOMException
  undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                          GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                          [AllowShared] ArrayBufferView? srcData, optional GLuint srcOffset = 0);

  undefined copyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                              GLint x, GLint y, GLsizei width, GLsizei height);

  undefined compressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                 GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, GLintptr offset);
  undefined compressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                 GLsizei height, GLsizei depth, GLint border, [AllowShared] ArrayBufferView srcData,
                                 optional GLuint srcOffset = 0, optional GLuint srcLengthOverride = 0);

  undefined compressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                    GLint zoffset, GLsizei width, GLsizei height, GLsizei depth,
                                    GLenum format, GLsizei imageSize, GLintptr offset);
  undefined compressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                    GLint zoffset, GLsizei width, GLsizei height, GLsizei depth,
                                    GLenum format, [AllowShared] ArrayBufferView srcData,
                                    optional GLuint srcOffset = 0,
                                    optional GLuint srcLengthOverride = 0);

  /* Programs and shaders */
  [WebGLHandlesContextLoss] GLint getFragDataLocation(WebGLProgram program, DOMString name);

  /* Uniforms */
  undefined uniform1ui(WebGLUniformLocation? location, GLuint v0);
  undefined uniform2ui(WebGLUniformLocation? location, GLuint v0, GLuint v1);
  undefined uniform3ui(WebGLUniformLocation? location, GLuint v0, GLuint v1, GLuint v2);
  undefined uniform4ui(WebGLUniformLocation? location, GLuint v0, GLuint v1, GLuint v2, GLuint v3);

  undefined uniform1uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                        optional GLuint srcLength = 0);
  undefined uniform2uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                        optional GLuint srcLength = 0);
  undefined uniform3uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                        optional GLuint srcLength = 0);
  undefined uniform4uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                        optional GLuint srcLength = 0);
  undefined uniformMatrix3x2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
  undefined uniformMatrix4x2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

  undefined uniformMatrix2x3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
  undefined uniformMatrix4x3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

  undefined uniformMatrix2x4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
  undefined uniformMatrix3x4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

  /* Vertex attribs */
  undefined vertexAttribI4i(GLuint index, GLint x, GLint y, GLint z, GLint w);
  undefined vertexAttribI4iv(GLuint index, Int32List values);
  undefined vertexAttribI4ui(GLuint index, GLuint x, GLuint y, GLuint z, GLuint w);
  undefined vertexAttribI4uiv(GLuint index, Uint32List values);
  undefined vertexAttribIPointer(GLuint index, GLint size, GLenum type, GLsizei stride, GLintptr offset);

  /* Writing to the drawing buffer */
  undefined vertexAttribDivisor(GLuint index, GLuint divisor);
  undefined drawArraysInstanced(GLenum mode, GLint first, GLsizei count, GLsizei instanceCount);
  undefined drawElementsInstanced(GLenum mode, GLsizei count, GLenum type, GLintptr offset, GLsizei instanceCount);
  undefined drawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, GLintptr offset);

  /* Multiple Render Targets */
  undefined drawBuffers(sequence<GLenum> buffers);

  undefined clearBufferfv(GLenum buffer, GLint drawbuffer, Float32List values,
                          optional GLuint srcOffset = 0);
  undefined clearBufferiv(GLenum buffer, GLint drawbuffer, Int32List values,
                          optional GLuint srcOffset = 0);
  undefined clearBufferuiv(GLenum buffer, GLint drawbuffer, Uint32List values,
                           optional GLuint srcOffset = 0);

  undefined clearBufferfi(GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil);

  /* Query Objects */
  WebGLQuery? createQuery();
  undefined deleteQuery(WebGLQuery? query);
  [WebGLHandlesContextLoss] GLboolean isQuery(WebGLQuery? query);
  undefined beginQuery(GLenum target, WebGLQuery query);
  undefined endQuery(GLenum target);
  WebGLQuery? getQuery(GLenum target, GLenum pname);
  any getQueryParameter(WebGLQuery query, GLenum pname);

  /* Sampler Objects */
  WebGLSampler? createSampler();
  undefined deleteSampler(WebGLSampler? sampler);
  [WebGLHandlesContextLoss] GLboolean isSampler(WebGLSampler? sampler);
  undefined bindSampler(GLuint unit, WebGLSampler? sampler);
  undefined samplerParameteri(WebGLSampler sampler, GLenum pname, GLint param);
  undefined samplerParameterf(WebGLSampler sampler, GLenum pname, GLfloat param);
  any getSamplerParameter(WebGLSampler sampler, GLenum pname);

  /* Sync objects */
  WebGLSync? fenceSync(GLenum condition, GLbitfield flags);
  [WebGLHandlesContextLoss] GLboolean isSync(WebGLSync? sync);
  undefined deleteSync(WebGLSync? sync);
  GLenum clientWaitSync(WebGLSync sync, GLbitfield flags, GLuint64 timeout);
  undefined waitSync(WebGLSync sync, GLbitfield flags, GLint64 timeout);
  any getSyncParameter(WebGLSync sync, GLenum pname);

  /* Transform Feedback */
  WebGLTransformFeedback? createTransformFeedback();
  undefined deleteTransformFeedback(WebGLTransformFeedback? tf);
  [WebGLHandlesContextLoss] GLboolean isTransformFeedback(WebGLTransformFeedback? tf);
  undefined bindTransformFeedback (GLenum target, WebGLTransformFeedback? tf);
  undefined beginTransformFeedback(GLenum primitiveMode);
  undefined endTransformFeedback();
  undefined transformFeedbackVaryings(WebGLProgram program, sequence<DOMString> varyings, GLenum bufferMode);
  WebGLActiveInfo? getTransformFeedbackVarying(WebGLProgram program, GLuint index);
  undefined pauseTransformFeedback();
  undefined resumeTransformFeedback();

  /* Uniform Buffer Objects and Transform Feedback Buffers */
  undefined bindBufferBase(GLenum target, GLuint index, WebGLBuffer? buffer);
  undefined bindBufferRange(GLenum target, GLuint index, WebGLBuffer? buffer, GLintptr offset, GLsizeiptr size);
  any getIndexedParameter(GLenum target, GLuint index);
  sequence<GLuint>? getUniformIndices(WebGLProgram program, sequence<DOMString> uniformNames);
  any getActiveUniforms(WebGLProgram program, sequence<GLuint> uniformIndices, GLenum pname);
  GLuint getUniformBlockIndex(WebGLProgram program, DOMString uniformBlockName);
  any getActiveUniformBlockParameter(WebGLProgram program, GLuint uniformBlockIndex, GLenum pname);
  DOMString? getActiveUniformBlockName(WebGLProgram program, GLuint uniformBlockIndex);
  undefined uniformBlockBinding(WebGLProgram program, GLuint uniformBlockIndex, GLuint uniformBlockBinding);

  /* Vertex Array Objects */
  WebGLVertexArrayObject? createVertexArray();
  undefined deleteVertexArray(WebGLVertexArrayObject? vertexArray);
  [WebGLHandlesContextLoss] GLboolean isVertexArray(WebGLVertexArrayObject? vertexArray);
  undefined bindVertexArray(WebGLVertexArrayObject? array);
};

interface mixin WebGL2RenderingContextOverloads
{
  // WebGL1:
  undefined bufferData(GLenum target, GLsizeiptr size, GLenum usage);
  undefined bufferData(GLenum target, [AllowShared] BufferSource? srcData, GLenum usage);
  undefined bufferSubData(GLenum target, GLintptr dstByteOffset, [AllowShared] BufferSource srcData);
  // WebGL2:
  undefined bufferData(GLenum target, [AllowShared] ArrayBufferView srcData, GLenum usage, GLuint srcOffset,
                       optional GLuint length = 0);
  undefined bufferSubData(GLenum target, GLintptr dstByteOffset, [AllowShared] ArrayBufferView srcData,
                          GLuint srcOffset, optional GLuint length = 0);

  // WebGL1 legacy entrypoints:
  undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                       GLsizei width, GLsizei height, GLint border, GLenum format,
                       GLenum type, [AllowShared] ArrayBufferView? pixels);
  undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                       GLenum format, GLenum type, TexImageSource source); // May throw DOMException

  undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                          GLsizei width, GLsizei height,
                          GLenum format, GLenum type, [AllowShared] ArrayBufferView? pixels);
  undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                          GLenum format, GLenum type, TexImageSource source); // May throw DOMException

  // WebGL2 entrypoints:
  undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLint border, GLenum format, GLenum type, GLintptr pboOffset);
  undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLint border, GLenum format, GLenum type,
                       TexImageSource source); // May throw DOMException
  undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                       GLint border, GLenum format, GLenum type, [AllowShared] ArrayBufferView srcData,
                       GLuint srcOffset);

  undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                          GLsizei height, GLenum format, GLenum type, GLintptr pboOffset);
  undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                          GLsizei height, GLenum format, GLenum type,
                          TexImageSource source); // May throw DOMException
  undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                          GLsizei height, GLenum format, GLenum type, [AllowShared] ArrayBufferView srcData,
                          GLuint srcOffset);

  undefined compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                 GLsizei height, GLint border, GLsizei imageSize, GLintptr offset);
  undefined compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                 GLsizei height, GLint border, [AllowShared] ArrayBufferView srcData,
                                 optional GLuint srcOffset = 0, optional GLuint srcLengthOverride = 0);

  undefined compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                    GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, GLintptr offset);
  undefined compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                    GLsizei width, GLsizei height, GLenum format,
                                    [AllowShared] ArrayBufferView srcData,
                                    optional GLuint srcOffset = 0,
                                    optional GLuint srcLengthOverride = 0);

  undefined uniform1fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);
  undefined uniform2fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);
  undefined uniform3fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);
  undefined uniform4fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);

  undefined uniform1iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);
  undefined uniform2iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);
  undefined uniform3iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);
  undefined uniform4iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                       optional GLuint srcLength = 0);

  undefined uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                             optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
  undefined uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                             optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
  undefined uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                             optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

  /* Reading back pixels */
  // WebGL1:
  undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
                       [AllowShared] ArrayBufferView? dstData);
  // WebGL2:
  undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
                       GLintptr offset);
  undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
                       [AllowShared] ArrayBufferView dstData, GLuint dstOffset);
};

[Exposed=(Window,Worker)]
interface WebGL2RenderingContext
{
};
WebGL2RenderingContext includes WebGLRenderingContextBase;
WebGL2RenderingContext includes WebGL2RenderingContextBase;
WebGL2RenderingContext includes WebGL2RenderingContextOverloads;

typedef unsigned long  GLenum;
typedef boolean        GLboolean;
typedef unsigned long  GLbitfield;
typedef byte           GLbyte;         /* 'byte' should be a signed 8 bit type. */
typedef short          GLshort;
typedef long           GLint;
typedef long           GLsizei;
typedef long long      GLintptr;
typedef long long      GLsizeiptr;
// Ideally the typedef below would use 'unsigned byte', but that doesn't currently exist in Web IDL.
typedef octet          GLubyte;        /* 'octet' should be an unsigned 8 bit type. */
typedef unsigned short GLushort;
typedef unsigned long  GLuint;
typedef unrestricted float GLfloat;
typedef unrestricted float GLclampf;

// The power preference settings are documented in the WebGLContextAttributes
// section of the specification.
enum WebGLPowerPreference { "default", "low-power", "high-performance" };

dictionary WebGLContextAttributes {
    boolean alpha = true;
    boolean depth = true;
    boolean stencil = false;
    boolean antialias = true;
    boolean premultipliedAlpha = true;
    boolean preserveDrawingBuffer = false;
    WebGLPowerPreference powerPreference = "default";
    boolean failIfMajorPerformanceCaveat = false;
    boolean desynchronized = false;
};

[Exposed=(Window,Worker)]
interface WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLBuffer : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLFramebuffer : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLProgram : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLRenderbuffer : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLShader : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLTexture : WebGLObject {
};

[Exposed=(Window,Worker)]
interface WebGLUniformLocation {
};

[Exposed=(Window,Worker)]
interface WebGLActiveInfo {
    readonly attribute GLint size;
    readonly attribute GLenum type;
    readonly attribute DOMString name;
};

[Exposed=(Window,Worker)]
interface WebGLShaderPrecisionFormat {
    readonly attribute GLint rangeMin;
    readonly attribute GLint rangeMax;
    readonly attribute GLint precision;
};

typedef (ImageBitmap or
         ImageData or
         HTMLImageElement or
         HTMLCanvasElement or
         HTMLVideoElement or
         OffscreenCanvas) TexImageSource;

typedef ([AllowShared] Float32Array or sequence<GLfloat>) Float32List;
typedef ([AllowShared] Int32Array or sequence<GLint>) Int32List;

interface mixin WebGLRenderingContextBase
{

    /* ClearBufferMask */
    const GLenum DEPTH_BUFFER_BIT               = 0x00000100;
    const GLenum STENCIL_BUFFER_BIT             = 0x00000400;
    const GLenum COLOR_BUFFER_BIT               = 0x00004000;

    /* BeginMode */
    const GLenum POINTS                         = 0x0000;
    const GLenum LINES                          = 0x0001;
    const GLenum LINE_LOOP                      = 0x0002;
    const GLenum LINE_STRIP                     = 0x0003;
    const GLenum TRIANGLES                      = 0x0004;
    const GLenum TRIANGLE_STRIP                 = 0x0005;
    const GLenum TRIANGLE_FAN                   = 0x0006;

    /* AlphaFunction (not supported in ES20) */
    /*      NEVER */
    /*      LESS */
    /*      EQUAL */
    /*      LEQUAL */
    /*      GREATER */
    /*      NOTEQUAL */
    /*      GEQUAL */
    /*      ALWAYS */

    /* BlendingFactorDest */
    const GLenum ZERO                           = 0;
    const GLenum ONE                            = 1;
    const GLenum SRC_COLOR                      = 0x0300;
    const GLenum ONE_MINUS_SRC_COLOR            = 0x0301;
    const GLenum SRC_ALPHA                      = 0x0302;
    const GLenum ONE_MINUS_SRC_ALPHA            = 0x0303;
    const GLenum DST_ALPHA                      = 0x0304;
    const GLenum ONE_MINUS_DST_ALPHA            = 0x0305;

    /* BlendingFactorSrc */
    /*      ZERO */
    /*      ONE */
    const GLenum DST_COLOR                      = 0x0306;
    const GLenum ONE_MINUS_DST_COLOR            = 0x0307;
    const GLenum SRC_ALPHA_SATURATE             = 0x0308;
    /*      SRC_ALPHA */
    /*      ONE_MINUS_SRC_ALPHA */
    /*      DST_ALPHA */
    /*      ONE_MINUS_DST_ALPHA */

    /* BlendEquationSeparate */
    const GLenum FUNC_ADD                       = 0x8006;
    const GLenum BLEND_EQUATION                 = 0x8009;
    const GLenum BLEND_EQUATION_RGB             = 0x8009;   /* same as BLEND_EQUATION */
    const GLenum BLEND_EQUATION_ALPHA           = 0x883D;

    /* BlendSubtract */
    const GLenum FUNC_SUBTRACT                  = 0x800A;
    const GLenum FUNC_REVERSE_SUBTRACT          = 0x800B;

    /* Separate Blend Functions */
    const GLenum BLEND_DST_RGB                  = 0x80C8;
    const GLenum BLEND_SRC_RGB                  = 0x80C9;
    const GLenum BLEND_DST_ALPHA                = 0x80CA;
    const GLenum BLEND_SRC_ALPHA                = 0x80CB;
    const GLenum CONSTANT_COLOR                 = 0x8001;
    const GLenum ONE_MINUS_CONSTANT_COLOR       = 0x8002;
    const GLenum CONSTANT_ALPHA                 = 0x8003;
    const GLenum ONE_MINUS_CONSTANT_ALPHA       = 0x8004;
    const GLenum BLEND_COLOR                    = 0x8005;

    /* Buffer Objects */
    const GLenum ARRAY_BUFFER                   = 0x8892;
    const GLenum ELEMENT_ARRAY_BUFFER           = 0x8893;
    const GLenum ARRAY_BUFFER_BINDING           = 0x8894;
    const GLenum ELEMENT_ARRAY_BUFFER_BINDING   = 0x8895;

    const GLenum STREAM_DRAW                    = 0x88E0;
    const GLenum STATIC_DRAW                    = 0x88E4;
    const GLenum DYNAMIC_DRAW                   = 0x88E8;

    const GLenum BUFFER_SIZE                    = 0x8764;
    const GLenum BUFFER_USAGE                   = 0x8765;

    const GLenum CURRENT_VERTEX_ATTRIB          = 0x8626;

    /* CullFaceMode */
    const GLenum FRONT                          = 0x0404;
    const GLenum BACK                           = 0x0405;
    const GLenum FRONT_AND_BACK                 = 0x0408;

    /* DepthFunction */
    /*      NEVER */
    /*      LESS */
    /*      EQUAL */
    /*      LEQUAL */
    /*      GREATER */
    /*      NOTEQUAL */
    /*      GEQUAL */
    /*      ALWAYS */

    /* EnableCap */
    /* TEXTURE_2D */
    const GLenum CULL_FACE                      = 0x0B44;
    const GLenum BLEND                          = 0x0BE2;
    const GLenum DITHER                         = 0x0BD0;
    const GLenum STENCIL_TEST                   = 0x0B90;
    const GLenum DEPTH_TEST                     = 0x0B71;
    const GLenum SCISSOR_TEST                   = 0x0C11;
    const GLenum POLYGON_OFFSET_FILL            = 0x8037;
    const GLenum SAMPLE_ALPHA_TO_COVERAGE       = 0x809E;
    const GLenum SAMPLE_COVERAGE                = 0x80A0;

    /* ErrorCode */
    const GLenum NO_ERROR                       = 0;
    const GLenum INVALID_ENUM                   = 0x0500;
    const GLenum INVALID_VALUE                  = 0x0501;
    const GLenum INVALID_OPERATION              = 0x0502;
    const GLenum OUT_OF_MEMORY                  = 0x0505;

    /* FrontFaceDirection */
    const GLenum CW                             = 0x0900;
    const GLenum CCW                            = 0x0901;

    /* GetPName */
    const GLenum LINE_WIDTH                     = 0x0B21;
    const GLenum ALIASED_POINT_SIZE_RANGE       = 0x846D;
    const GLenum ALIASED_LINE_WIDTH_RANGE       = 0x846E;
    const GLenum CULL_FACE_MODE                 = 0x0B45;
    const GLenum FRONT_FACE                     = 0x0B46;
    const GLenum DEPTH_RANGE                    = 0x0B70;
    const GLenum DEPTH_WRITEMASK                = 0x0B72;
    const GLenum DEPTH_CLEAR_VALUE              = 0x0B73;
    const GLenum DEPTH_FUNC                     = 0x0B74;
    const GLenum STENCIL_CLEAR_VALUE            = 0x0B91;
    const GLenum STENCIL_FUNC                   = 0x0B92;
    const GLenum STENCIL_FAIL                   = 0x0B94;
    const GLenum STENCIL_PASS_DEPTH_FAIL        = 0x0B95;
    const GLenum STENCIL_PASS_DEPTH_PASS        = 0x0B96;
    const GLenum STENCIL_REF                    = 0x0B97;
    const GLenum STENCIL_VALUE_MASK             = 0x0B93;
    const GLenum STENCIL_WRITEMASK              = 0x0B98;
    const GLenum STENCIL_BACK_FUNC              = 0x8800;
    const GLenum STENCIL_BACK_FAIL              = 0x8801;
    const GLenum STENCIL_BACK_PASS_DEPTH_FAIL   = 0x8802;
    const GLenum STENCIL_BACK_PASS_DEPTH_PASS   = 0x8803;
    const GLenum STENCIL_BACK_REF               = 0x8CA3;
    const GLenum STENCIL_BACK_VALUE_MASK        = 0x8CA4;
    const GLenum STENCIL_BACK_WRITEMASK         = 0x8CA5;
    const GLenum VIEWPORT                       = 0x0BA2;
    const GLenum SCISSOR_BOX                    = 0x0C10;
    /*      SCISSOR_TEST */
    const GLenum COLOR_CLEAR_VALUE              = 0x0C22;
    const GLenum COLOR_WRITEMASK                = 0x0C23;
    const GLenum UNPACK_ALIGNMENT               = 0x0CF5;
    const GLenum PACK_ALIGNMENT                 = 0x0D05;
    const GLenum MAX_TEXTURE_SIZE               = 0x0D33;
    const GLenum MAX_VIEWPORT_DIMS              = 0x0D3A;
    const GLenum SUBPIXEL_BITS                  = 0x0D50;
    const GLenum RED_BITS                       = 0x0D52;
    const GLenum GREEN_BITS                     = 0x0D53;
    const GLenum BLUE_BITS                      = 0x0D54;
    const GLenum ALPHA_BITS                     = 0x0D55;
    const GLenum DEPTH_BITS                     = 0x0D56;
    const GLenum STENCIL_BITS                   = 0x0D57;
    const GLenum POLYGON_OFFSET_UNITS           = 0x2A00;
    /*      POLYGON_OFFSET_FILL */
    const GLenum POLYGON_OFFSET_FACTOR          = 0x8038;
    const GLenum TEXTURE_BINDING_2D             = 0x8069;
    const GLenum SAMPLE_BUFFERS                 = 0x80A8;
    const GLenum SAMPLES                        = 0x80A9;
    const GLenum SAMPLE_COVERAGE_VALUE          = 0x80AA;
    const GLenum SAMPLE_COVERAGE_INVERT         = 0x80AB;

    /* GetTextureParameter */
    /*      TEXTURE_MAG_FILTER */
    /*      TEXTURE_MIN_FILTER */
    /*      TEXTURE_WRAP_S */
    /*      TEXTURE_WRAP_T */

    const GLenum COMPRESSED_TEXTURE_FORMATS     = 0x86A3;

    /* HintMode */
    const GLenum DONT_CARE                      = 0x1100;
    const GLenum FASTEST                        = 0x1101;
    const GLenum NICEST                         = 0x1102;

    /* HintTarget */
    const GLenum GENERATE_MIPMAP_HINT            = 0x8192;

    /* DataType */
    const GLenum BYTE                           = 0x1400;
    const GLenum UNSIGNED_BYTE                  = 0x1401;
    const GLenum SHORT                          = 0x1402;
    const GLenum UNSIGNED_SHORT                 = 0x1403;
    const GLenum INT                            = 0x1404;
    const GLenum UNSIGNED_INT                   = 0x1405;
    const GLenum FLOAT                          = 0x1406;

    /* PixelFormat */
    const GLenum DEPTH_COMPONENT                = 0x1902;
    const GLenum ALPHA                          = 0x1906;
    const GLenum RGB                            = 0x1907;
    const GLenum RGBA                           = 0x1908;
    const GLenum LUMINANCE                      = 0x1909;
    const GLenum LUMINANCE_ALPHA                = 0x190A;

    /* PixelType */
    /*      UNSIGNED_BYTE */
    const GLenum UNSIGNED_SHORT_4_4_4_4         = 0x8033;
    const GLenum UNSIGNED_SHORT_5_5_5_1         = 0x8034;
    const GLenum UNSIGNED_SHORT_5_6_5           = 0x8363;

    /* Shaders */
    const GLenum FRAGMENT_SHADER                  = 0x8B30;
    const GLenum VERTEX_SHADER                    = 0x8B31;
    const GLenum MAX_VERTEX_ATTRIBS               = 0x8869;
    const GLenum MAX_VERTEX_UNIFORM_VECTORS       = 0x8DFB;
    const GLenum MAX_VARYING_VECTORS              = 0x8DFC;
    const GLenum MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D;
    const GLenum MAX_VERTEX_TEXTURE_IMAGE_UNITS   = 0x8B4C;
    const GLenum MAX_TEXTURE_IMAGE_UNITS          = 0x8872;
    const GLenum MAX_FRAGMENT_UNIFORM_VECTORS     = 0x8DFD;
    const GLenum SHADER_TYPE                      = 0x8B4F;
    const GLenum DELETE_STATUS                    = 0x8B80;
    const GLenum LINK_STATUS                      = 0x8B82;
    const GLenum VALIDATE_STATUS                  = 0x8B83;
    const GLenum ATTACHED_SHADERS                 = 0x8B85;
    const GLenum ACTIVE_UNIFORMS                  = 0x8B86;
    const GLenum ACTIVE_ATTRIBUTES                = 0x8B89;
    const GLenum SHADING_LANGUAGE_VERSION         = 0x8B8C;
    const GLenum CURRENT_PROGRAM                  = 0x8B8D;

    /* StencilFunction */
    const GLenum NEVER                          = 0x0200;
    const GLenum LESS                           = 0x0201;
    const GLenum EQUAL                          = 0x0202;
    const GLenum LEQUAL                         = 0x0203;
    const GLenum GREATER                        = 0x0204;
    const GLenum NOTEQUAL                       = 0x0205;
    const GLenum GEQUAL                         = 0x0206;
    const GLenum ALWAYS                         = 0x0207;

    /* StencilOp */
    /*      ZERO */
    const GLenum KEEP                           = 0x1E00;
    const GLenum REPLACE                        = 0x1E01;
    const GLenum INCR                           = 0x1E02;
    const GLenum DECR                           = 0x1E03;
    const GLenum INVERT                         = 0x150A;
    const GLenum INCR_WRAP                      = 0x8507;
    const GLenum DECR_WRAP                      = 0x8508;

    /* StringName */
    const GLenum VENDOR                         = 0x1F00;
    const GLenum RENDERER                       = 0x1F01;
    const GLenum VERSION                        = 0x1F02;

    /* TextureMagFilter */
    const GLenum NEAREST                        = 0x2600;
    const GLenum LINEAR                         = 0x2601;

    /* TextureMinFilter */
    /*      NEAREST */
    /*      LINEAR */
    const GLenum NEAREST_MIPMAP_NEAREST         = 0x2700;
    const GLenum LINEAR_MIPMAP_NEAREST          = 0x2701;
    const GLenum NEAREST_MIPMAP_LINEAR          = 0x2702;
    const GLenum LINEAR_MIPMAP_LINEAR           = 0x2703;

    /* TextureParameterName */
    const GLenum TEXTURE_MAG_FILTER             = 0x2800;
    const GLenum TEXTURE_MIN_FILTER             = 0x2801;
    const GLenum TEXTURE_WRAP_S                 = 0x2802;
    const GLenum TEXTURE_WRAP_T                 = 0x2803;

    /* TextureTarget */
    const GLenum TEXTURE_2D                     = 0x0DE1;
    const GLenum TEXTURE                        = 0x1702;

    const GLenum TEXTURE_CUBE_MAP               = 0x8513;
    const GLenum TEXTURE_BINDING_CUBE_MAP       = 0x8514;
    const GLenum TEXTURE_CUBE_MAP_POSITIVE_X    = 0x8515;
    const GLenum TEXTURE_CUBE_MAP_NEGATIVE_X    = 0x8516;
    const GLenum TEXTURE_CUBE_MAP_POSITIVE_Y    = 0x8517;
    const GLenum TEXTURE_CUBE_MAP_NEGATIVE_Y    = 0x8518;
    const GLenum TEXTURE_CUBE_MAP_POSITIVE_Z    = 0x8519;
    const GLenum TEXTURE_CUBE_MAP_NEGATIVE_Z    = 0x851A;
    const GLenum MAX_CUBE_MAP_TEXTURE_SIZE      = 0x851C;

    /* TextureUnit */
    const GLenum TEXTURE0                       = 0x84C0;
    const GLenum TEXTURE1                       = 0x84C1;
    const GLenum TEXTURE2                       = 0x84C2;
    const GLenum TEXTURE3                       = 0x84C3;
    const GLenum TEXTURE4                       = 0x84C4;
    const GLenum TEXTURE5                       = 0x84C5;
    const GLenum TEXTURE6                       = 0x84C6;
    const GLenum TEXTURE7                       = 0x84C7;
    const GLenum TEXTURE8                       = 0x84C8;
    const GLenum TEXTURE9                       = 0x84C9;
    const GLenum TEXTURE10                      = 0x84CA;
    const GLenum TEXTURE11                      = 0x84CB;
    const GLenum TEXTURE12                      = 0x84CC;
    const GLenum TEXTURE13                      = 0x84CD;
    const GLenum TEXTURE14                      = 0x84CE;
    const GLenum TEXTURE15                      = 0x84CF;
    const GLenum TEXTURE16                      = 0x84D0;
    const GLenum TEXTURE17                      = 0x84D1;
    const GLenum TEXTURE18                      = 0x84D2;
    const GLenum TEXTURE19                      = 0x84D3;
    const GLenum TEXTURE20                      = 0x84D4;
    const GLenum TEXTURE21                      = 0x84D5;
    const GLenum TEXTURE22                      = 0x84D6;
    const GLenum TEXTURE23                      = 0x84D7;
    const GLenum TEXTURE24                      = 0x84D8;
    const GLenum TEXTURE25                      = 0x84D9;
    const GLenum TEXTURE26                      = 0x84DA;
    const GLenum TEXTURE27                      = 0x84DB;
    const GLenum TEXTURE28                      = 0x84DC;
    const GLenum TEXTURE29                      = 0x84DD;
    const GLenum TEXTURE30                      = 0x84DE;
    const GLenum TEXTURE31                      = 0x84DF;
    const GLenum ACTIVE_TEXTURE                 = 0x84E0;

    /* TextureWrapMode */
    const GLenum REPEAT                         = 0x2901;
    const GLenum CLAMP_TO_EDGE                  = 0x812F;
    const GLenum MIRRORED_REPEAT                = 0x8370;

    /* Uniform Types */
    const GLenum FLOAT_VEC2                     = 0x8B50;
    const GLenum FLOAT_VEC3                     = 0x8B51;
    const GLenum FLOAT_VEC4                     = 0x8B52;
    const GLenum INT_VEC2                       = 0x8B53;
    const GLenum INT_VEC3                       = 0x8B54;
    const GLenum INT_VEC4                       = 0x8B55;
    const GLenum BOOL                           = 0x8B56;
    const GLenum BOOL_VEC2                      = 0x8B57;
    const GLenum BOOL_VEC3                      = 0x8B58;
    const GLenum BOOL_VEC4                      = 0x8B59;
    const GLenum FLOAT_MAT2                     = 0x8B5A;
    const GLenum FLOAT_MAT3                     = 0x8B5B;
    const GLenum FLOAT_MAT4                     = 0x8B5C;
    const GLenum SAMPLER_2D                     = 0x8B5E;
    const GLenum SAMPLER_CUBE                   = 0x8B60;

    /* Vertex Arrays */
    const GLenum VERTEX_ATTRIB_ARRAY_ENABLED        = 0x8622;
    const GLenum VERTEX_ATTRIB_ARRAY_SIZE           = 0x8623;
    const GLenum VERTEX_ATTRIB_ARRAY_STRIDE         = 0x8624;
    const GLenum VERTEX_ATTRIB_ARRAY_TYPE           = 0x8625;
    const GLenum VERTEX_ATTRIB_ARRAY_NORMALIZED     = 0x886A;
    const GLenum VERTEX_ATTRIB_ARRAY_POINTER        = 0x8645;
    const GLenum VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 0x889F;

    /* Read Format */
    const GLenum IMPLEMENTATION_COLOR_READ_TYPE   = 0x8B9A;
    const GLenum IMPLEMENTATION_COLOR_READ_FORMAT = 0x8B9B;

    /* Shader Source */
    const GLenum COMPILE_STATUS                 = 0x8B81;

    /* Shader Precision-Specified Types */
    const GLenum LOW_FLOAT                      = 0x8DF0;
    const GLenum MEDIUM_FLOAT                   = 0x8DF1;
    const GLenum HIGH_FLOAT                     = 0x8DF2;
    const GLenum LOW_INT                        = 0x8DF3;
    const GLenum MEDIUM_INT                     = 0x8DF4;
    const GLenum HIGH_INT                       = 0x8DF5;

    /* Framebuffer Object. */
    const GLenum FRAMEBUFFER                    = 0x8D40;
    const GLenum RENDERBUFFER                   = 0x8D41;

    const GLenum RGBA4                          = 0x8056;
    const GLenum RGB5_A1                        = 0x8057;
    const GLenum RGB565                         = 0x8D62;
    const GLenum DEPTH_COMPONENT16              = 0x81A5;
    const GLenum STENCIL_INDEX8                 = 0x8D48;
    const GLenum DEPTH_STENCIL                  = 0x84F9;

    const GLenum RENDERBUFFER_WIDTH             = 0x8D42;
    const GLenum RENDERBUFFER_HEIGHT            = 0x8D43;
    const GLenum RENDERBUFFER_INTERNAL_FORMAT   = 0x8D44;
    const GLenum RENDERBUFFER_RED_SIZE          = 0x8D50;
    const GLenum RENDERBUFFER_GREEN_SIZE        = 0x8D51;
    const GLenum RENDERBUFFER_BLUE_SIZE         = 0x8D52;
    const GLenum RENDERBUFFER_ALPHA_SIZE        = 0x8D53;
    const GLenum RENDERBUFFER_DEPTH_SIZE        = 0x8D54;
    const GLenum RENDERBUFFER_STENCIL_SIZE      = 0x8D55;

    const GLenum FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE           = 0x8CD0;
    const GLenum FRAMEBUFFER_ATTACHMENT_OBJECT_NAME           = 0x8CD1;
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL         = 0x8CD2;
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3;

    const GLenum COLOR_ATTACHMENT0              = 0x8CE0;
    const GLenum DEPTH_ATTACHMENT               = 0x8D00;
    const GLenum STENCIL_ATTACHMENT             = 0x8D20;
    const GLenum DEPTH_STENCIL_ATTACHMENT       = 0x821A;

    const GLenum NONE                           = 0;

    const GLenum FRAMEBUFFER_COMPLETE                      = 0x8CD5;
    const GLenum FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = 0x8CD6;
    const GLenum FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7;
    const GLenum FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = 0x8CD9;
    const GLenum FRAMEBUFFER_UNSUPPORTED                   = 0x8CDD;

    const GLenum FRAMEBUFFER_BINDING            = 0x8CA6;
    const GLenum RENDERBUFFER_BINDING           = 0x8CA7;
    const GLenum MAX_RENDERBUFFER_SIZE          = 0x84E8;

    const GLenum INVALID_FRAMEBUFFER_OPERATION  = 0x0506;

    /* WebGL-specific enums */
    const GLenum UNPACK_FLIP_Y_WEBGL            = 0x9240;
    const GLenum UNPACK_PREMULTIPLY_ALPHA_WEBGL = 0x9241;
    const GLenum CONTEXT_LOST_WEBGL             = 0x9242;
    const GLenum UNPACK_COLORSPACE_CONVERSION_WEBGL = 0x9243;
    const GLenum BROWSER_DEFAULT_WEBGL          = 0x9244;

    [Exposed=Window] readonly attribute (HTMLCanvasElement or OffscreenCanvas) canvas;
    readonly attribute GLsizei drawingBufferWidth;
    readonly attribute GLsizei drawingBufferHeight;

    [WebGLHandlesContextLoss] WebGLContextAttributes? getContextAttributes();
    [WebGLHandlesContextLoss] boolean isContextLost();

    sequence<DOMString>? getSupportedExtensions();
    object? getExtension(DOMString name);

    undefined activeTexture(GLenum texture);
    undefined attachShader(WebGLProgram program, WebGLShader shader);
    undefined bindAttribLocation(WebGLProgram program, GLuint index, DOMString name);
    undefined bindBuffer(GLenum target, WebGLBuffer? buffer);
    undefined bindFramebuffer(GLenum target, WebGLFramebuffer? framebuffer);
    undefined bindRenderbuffer(GLenum target, WebGLRenderbuffer? renderbuffer);
    undefined bindTexture(GLenum target, WebGLTexture? texture);
    undefined blendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
    undefined blendEquation(GLenum mode);
    undefined blendEquationSeparate(GLenum modeRGB, GLenum modeAlpha);
    undefined blendFunc(GLenum sfactor, GLenum dfactor);
    undefined blendFuncSeparate(GLenum srcRGB, GLenum dstRGB,
                                GLenum srcAlpha, GLenum dstAlpha);

    [WebGLHandlesContextLoss] GLenum checkFramebufferStatus(GLenum target);
    undefined clear(GLbitfield mask);
    undefined clearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
    undefined clearDepth(GLclampf depth);
    undefined clearStencil(GLint s);
    undefined colorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
    undefined compileShader(WebGLShader shader);

    undefined copyTexImage2D(GLenum target, GLint level, GLenum internalformat,
                             GLint x, GLint y, GLsizei width, GLsizei height,
                             GLint border);
    undefined copyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                GLint x, GLint y, GLsizei width, GLsizei height);

    WebGLBuffer? createBuffer();
    WebGLFramebuffer? createFramebuffer();
    WebGLProgram? createProgram();
    WebGLRenderbuffer? createRenderbuffer();
    WebGLShader? createShader(GLenum type);
    WebGLTexture? createTexture();

    undefined cullFace(GLenum mode);

    undefined deleteBuffer(WebGLBuffer? buffer);
    undefined deleteFramebuffer(WebGLFramebuffer? framebuffer);
    undefined deleteProgram(WebGLProgram? program);
    undefined deleteRenderbuffer(WebGLRenderbuffer? renderbuffer);
    undefined deleteShader(WebGLShader? shader);
    undefined deleteTexture(WebGLTexture? texture);

    undefined depthFunc(GLenum func);
    undefined depthMask(GLboolean flag);
    undefined depthRange(GLclampf zNear, GLclampf zFar);
    undefined detachShader(WebGLProgram program, WebGLShader shader);
    undefined disable(GLenum cap);
    undefined disableVertexAttribArray(GLuint index);
    undefined drawArrays(GLenum mode, GLint first, GLsizei count);
    undefined drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset);

    undefined enable(GLenum cap);
    undefined enableVertexAttribArray(GLuint index);
    undefined finish();
    undefined flush();
    undefined framebufferRenderbuffer(GLenum target, GLenum attachment,
                                      GLenum renderbuffertarget,
                                      WebGLRenderbuffer? renderbuffer);
    undefined framebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget,
                                   WebGLTexture? texture, GLint level);
    undefined frontFace(GLenum mode);

    undefined generateMipmap(GLenum target);

    WebGLActiveInfo? getActiveAttrib(WebGLProgram program, GLuint index);
    WebGLActiveInfo? getActiveUniform(WebGLProgram program, GLuint index);
    sequence<WebGLShader>? getAttachedShaders(WebGLProgram program);

    [WebGLHandlesContextLoss] GLint getAttribLocation(WebGLProgram program, DOMString name);

    any getBufferParameter(GLenum target, GLenum pname);
    any getParameter(GLenum pname);

    [WebGLHandlesContextLoss] GLenum getError();

    any getFramebufferAttachmentParameter(GLenum target, GLenum attachment,
                                          GLenum pname);
    any getProgramParameter(WebGLProgram program, GLenum pname);
    DOMString? getProgramInfoLog(WebGLProgram program);
    any getRenderbufferParameter(GLenum target, GLenum pname);
    any getShaderParameter(WebGLShader shader, GLenum pname);
    WebGLShaderPrecisionFormat? getShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype);
    DOMString? getShaderInfoLog(WebGLShader shader);

    DOMString? getShaderSource(WebGLShader shader);

    any getTexParameter(GLenum target, GLenum pname);

    any getUniform(WebGLProgram program, WebGLUniformLocation location);

    WebGLUniformLocation? getUniformLocation(WebGLProgram program, DOMString name);

    any getVertexAttrib(GLuint index, GLenum pname);

    [WebGLHandlesContextLoss] GLintptr getVertexAttribOffset(GLuint index, GLenum pname);

    undefined hint(GLenum target, GLenum mode);
    [WebGLHandlesContextLoss] GLboolean isBuffer(WebGLBuffer? buffer);
    [WebGLHandlesContextLoss] GLboolean isEnabled(GLenum cap);
    [WebGLHandlesContextLoss] GLboolean isFramebuffer(WebGLFramebuffer? framebuffer);
    [WebGLHandlesContextLoss] GLboolean isProgram(WebGLProgram? program);
    [WebGLHandlesContextLoss] GLboolean isRenderbuffer(WebGLRenderbuffer? renderbuffer);
    [WebGLHandlesContextLoss] GLboolean isShader(WebGLShader? shader);
    [WebGLHandlesContextLoss] GLboolean isTexture(WebGLTexture? texture);
    undefined lineWidth(GLfloat width);
    undefined linkProgram(WebGLProgram program);
    undefined pixelStorei(GLenum pname, GLint param);
    undefined polygonOffset(GLfloat factor, GLfloat units);

    undefined renderbufferStorage(GLenum target, GLenum internalformat,
                                  GLsizei width, GLsizei height);
    undefined sampleCoverage(GLclampf value, GLboolean invert);
    undefined scissor(GLint x, GLint y, GLsizei width, GLsizei height);

    undefined shaderSource(WebGLShader shader, DOMString source);

    undefined stencilFunc(GLenum func, GLint ref, GLuint mask);
    undefined stencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask);
    undefined stencilMask(GLuint mask);
    undefined stencilMaskSeparate(GLenum face, GLuint mask);
    undefined stencilOp(GLenum fail, GLenum zfail, GLenum zpass);
    undefined stencilOpSeparate(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);

    undefined texParameterf(GLenum target, GLenum pname, GLfloat param);
    undefined texParameteri(GLenum target, GLenum pname, GLint param);

    undefined uniform1f(WebGLUniformLocation? location, GLfloat x);
    undefined uniform2f(WebGLUniformLocation? location, GLfloat x, GLfloat y);
    undefined uniform3f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z);
    undefined uniform4f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);

    undefined uniform1i(WebGLUniformLocation? location, GLint x);
    undefined uniform2i(WebGLUniformLocation? location, GLint x, GLint y);
    undefined uniform3i(WebGLUniformLocation? location, GLint x, GLint y, GLint z);
    undefined uniform4i(WebGLUniformLocation? location, GLint x, GLint y, GLint z, GLint w);

    undefined useProgram(WebGLProgram? program);
    undefined validateProgram(WebGLProgram program);

    undefined vertexAttrib1f(GLuint index, GLfloat x);
    undefined vertexAttrib2f(GLuint index, GLfloat x, GLfloat y);
    undefined vertexAttrib3f(GLuint index, GLfloat x, GLfloat y, GLfloat z);
    undefined vertexAttrib4f(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w);

    undefined vertexAttrib1fv(GLuint index, Float32List values);
    undefined vertexAttrib2fv(GLuint index, Float32List values);
    undefined vertexAttrib3fv(GLuint index, Float32List values);
    undefined vertexAttrib4fv(GLuint index, Float32List values);

    undefined vertexAttribPointer(GLuint index, GLint size, GLenum type,
                                  GLboolean normalized, GLsizei stride, GLintptr offset);

    undefined viewport(GLint x, GLint y, GLsizei width, GLsizei height);
};

interface mixin WebGLRenderingContextOverloads
{
    undefined bufferData(GLenum target, GLsizeiptr size, GLenum usage);
    undefined bufferData(GLenum target, [AllowShared] BufferSource? data, GLenum usage);
    undefined bufferSubData(GLenum target, GLintptr offset, [AllowShared] BufferSource data);

    undefined compressedTexImage2D(GLenum target, GLint level, GLenum internalformat,
                                   GLsizei width, GLsizei height, GLint border,
                                   [AllowShared] ArrayBufferView data);
    undefined compressedTexSubImage2D(GLenum target, GLint level,
                                      GLint xoffset, GLint yoffset,
                                      GLsizei width, GLsizei height, GLenum format,
                                      [AllowShared] ArrayBufferView data);

    undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height,
                         GLenum format, GLenum type, [AllowShared] ArrayBufferView? pixels);

    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLsizei width, GLsizei height, GLint border, GLenum format,
                         GLenum type, [AllowShared] ArrayBufferView? pixels);
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, TexImageSource source); // May throw DOMException

    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLsizei width, GLsizei height,
                            GLenum format, GLenum type, [AllowShared] ArrayBufferView? pixels);
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, TexImageSource source); // May throw DOMException

    undefined uniform1fv(WebGLUniformLocation? location, Float32List v);
    undefined uniform2fv(WebGLUniformLocation? location, Float32List v);
    undefined uniform3fv(WebGLUniformLocation? location, Float32List v);
    undefined uniform4fv(WebGLUniformLocation? location, Float32List v);

    undefined uniform1iv(WebGLUniformLocation? location, Int32List v);
    undefined uniform2iv(WebGLUniformLocation? location, Int32List v);
    undefined uniform3iv(WebGLUniformLocation? location, Int32List v);
    undefined uniform4iv(WebGLUniformLocation? location, Int32List v);

    undefined uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List value);
    undefined uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List value);
    undefined uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List value);
};

[Exposed=(Window,Worker)]
interface WebGLRenderingContext
{
};
WebGLRenderingContext includes WebGLRenderingContextBase;
WebGLRenderingContext includes WebGLRenderingContextOverloads;
