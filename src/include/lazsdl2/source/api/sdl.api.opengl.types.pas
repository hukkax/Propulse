unit SDL.Api.OpenGL.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$MACRO ON}

interface

uses
  ctypes;

const
  GL_MAX_EVAL_ORDER                 = $0D30;
  GL_MAX_LIGHTS                     = $0D31;
  GL_MAX_CLIP_PLANES                = $0D32;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_PIXEL_MAP_TABLE            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH      = $0D36;
  GL_MAX_NAME_STACK_DEPTH           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH        = $0D39;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = $0D3B;

type

  GLvoid = pointer;
  GLenum = cuint;
  GLboolean = cuchar;
  GLbitfield = cuint;
  GLbyte = byte;
  GLshort = cshort;
  GLint = cint;
  GLsizei = cint;
  GLubyte = cuchar;
  GLushort = cushort;
  GLuint = cuint;
  GLfloat = cfloat;
  GLclampf = cfloat;
  GLfixed = longint;
  GLclampx = longint;
  GLchar = PAnsiChar;
  GLdouble = cdouble;

  GLintptr = ptrint;
  GLsizeiptr = sizeint;

  PGLfloat = ^GLfloat;
  PGLvoid = ^GLvoid;
  PGLubyte = ^GLubyte;
  PGLint = ^GLint;
  PGLboolean = ^GLboolean;
  PGLuint = ^GLuint;
  PGLfixed = ^GLfixed;
  PGLchar = ^GLchar;
  PGLsizei = ^GLsizei;

  GL_Texture = GLint;
  GL_Pipeline = GLuint;
  GL_Program = GLuint;
  GL_Shader = GLuint;
  GL_Query = GLuint;
  GL_VertexAtr = GLuint;
  GL_Buffer = GLuint;
  GL_FrmBuffer = GLuint;
  GL_Image = GLuint;
  GL_RenderBuff = GLuint;
  GL_Sampler = GLuint;

  PGL_Texture = ^GL_Texture;
  PGL_Pipeline = ^GL_Pipeline;
  PGL_Program = ^GL_Program;
  PGL_Shader = ^GL_Shader;
  PGL_Query = ^GL_Query;
  PGL_VertexAtr = ^GL_VertexAtr;
  PGL_Buffer = ^GL_Buffer;
  PGL_FrmBuffer = ^GL_FrmBuffer;
  PGL_Image = ^GL_Image;
  PGL_RenderBuff = ^GL_RenderBuff;
  PGL_Sampler = ^GL_Sampler;

  {$IFNDEF ANDROID}
  GLsync = Pointer;
  PGLsync = ^GLSync;
  {$ENDIF}

  GL_ClearBufferType = (
    GL_DEPTH_BUFFER_BIT   = $00000100,
    {$IFNDEF ANDROID}
    GL_ACCUM_BUFFER_BIT   = $00000200,
    {$ENDIF}
    GL_STENCIL_BUFFER_BIT = $00000400,
    GL_COLOR_BUFFER_BIT   = $00004000
  );

  GL_ClearBufferTypes = record Value: UInt32; end;

  GL_AlphaFunction = (
    GL_NEVER = $0200,
    GL_LESS = $0201,
    GL_EQUAL = $0202,
    GL_LEQUAL = $0203,
    GL_GREATER = $0204,
    GL_NOTEQUAL = $0205,
    GL_GEQUAL = $0206,
    GL_ALWAYS = $0207
  );

  GL_Enums = (
    GL_TEXTURE_1D_ARB                  = $0DE,
    GL_TEXTURE_2D_ARB                  = $0DE1,
    GL_TEXTURE_3D_ARB                  = $806F,
    GL_TEXTURE_CUBE_MAP_ARB            = $8513,
    GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A,
    GL_TEXTURE_1D_ARRAY_ARB            = $8C18,
    GL_TEXTURE_2D_ARRAY_ARB            = $8C1A,
    GL_TEXTURE_CUBE_MAP_ARRAY_ARB      = $9009
  );

  GL_TexImageTarget = (
    GL_TEXTURE_2D_TXIMG = Integer(GL_TEXTURE_2D_ARB),
    GL_TEXTURE_CUBE_MAP_POSITIVE_X = Integer(GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB),
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X = Integer(GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB),
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y = Integer(GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB),
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = Integer(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB),
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z = Integer(GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB),
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = Integer(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB)
  );

  GL_Tex3DImageTarget = (
    GL_TEXTURE_3D_TXIMG            = Integer(GL_TEXTURE_3D_ARB),
    GL_PROXY_TEXTURE_3D            = $8070
  );

  GL_BindTarget = (
    GL_TEXTURE_1D                    = Integer(GL_TEXTURE_1D_ARB),
    GL_TEXTURE_2D                    = Integer(GL_TEXTURE_2D_ARB),
    GL_TEXTURE_3D                    = Integer(GL_TEXTURE_3D_ARB),
    GL_TEXTURE_RECTANGLE             = $84F5,
    GL_TEXTURE_CUBE_MAP              = Integer(GL_TEXTURE_CUBE_MAP_ARB),
    GL_TEXTURE_1D_ARRAY              = Integer(GL_TEXTURE_1D_ARRAY_ARB),
    GL_TEXTURE_2D_ARRAY              = Integer(GL_TEXTURE_2D_ARRAY_ARB),
    GL_TEXTURE_BUFFER                = $8C2A,
    GL_TEXTURE_CUBE_MAP_ARRAY        = Integer(GL_TEXTURE_CUBE_MAP_ARRAY_ARB),
    GL_TEXTURE_2D_MULTISAMPLE        = $9100,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY  = $9102
  );

  GL_MinimapTarget = (
    GL_TEXTURE_1D_MIN                = Integer(GL_TEXTURE_1D_ARB),
    GL_TEXTURE_2D_MIN                = Integer(GL_TEXTURE_2D_ARB),
    GL_TEXTURE_3D_MIN                = Integer(GL_TEXTURE_3D_ARB),
    GL_TEXTURE_CUBE_MAP_MIN          = Integer(GL_TEXTURE_CUBE_MAP_ARB),
    GL_TEXTURE_1D_ARRAY_MIN          = Integer(GL_TEXTURE_1D_ARRAY_ARB),
    GL_TEXTURE_2D_ARRAY_MIN          = Integer(GL_TEXTURE_2D_ARRAY_ARB),
    GL_TEXTURE_CUBE_MAP_ARRAY_MIN    = Integer(GL_TEXTURE_CUBE_MAP_ARRAY_ARB)
  );

  GL_Capability = (
    GL_POINT_SMOOTH                  = $0B10,
    GL_LINE_SMOOTH                   = $0B20,
    GL_LINE_STIPPLE                  = $0B24,
    GL_POLYGON_SMOOTH                = $0B41,
    GL_POLYGON_STIPPLE               = $0B42,
    GL_CULL_FACE                     = $0B44,
    GL_LIGHTING                      = $0B50,
    GL_COLOR_MATERIAL                = $0B57,
    GL_FOG                           = $0B60,
    GL_DEPTH_TEST                    = $0B71,
    GL_STENCIL_TEST                  = $0B90,
    GL_NORMALIZE                     = $0BA1,
    GL_ALPHA_TEST                    = $0BC0,
    GL_DITHER                        = $0BD0,
    GL_BLEND                         = $0BE2,
    GL_INDEX_LOGIC_OP                = $0BF1,
    GL_COLOR_LOGIC_OP                = $0BF2,
    GL_SCISSOR_TEST                  = $0C11,
    GL_TEXTURE_GEN_S                 = $0C60,
    GL_TEXTURE_GEN_T                 = $0C61,
    GL_TEXTURE_GEN_R                 = $0C62,
    GL_TEXTURE_GEN_Q                 = $0C63,
    GL_AUTO_NORMAL                   = $0D80,
    GL_MAP1_COLOR_4                  = $0D90,
    GL_MAP1_INDEX                    = $0D91,
    GL_MAP1_NORMAL                   = $0D92,
    GL_MAP1_TEXTURE_COORD_1          = $0D93,
    GL_MAP1_TEXTURE_COORD_2          = $0D94,
    GL_MAP1_TEXTURE_COORD_3          = $0D95,
    GL_MAP1_TEXTURE_COORD_4          = $0D96,
    GL_MAP1_VERTEX_3                 = $0D97,
    GL_MAP1_VERTEX_4                 = $0D98,
    GL_MAP2_COLOR_4                  = $0DB0,
    GL_MAP2_INDEX                    = $0DB1,
    GL_MAP2_NORMAL                   = $0DB2,
    GL_MAP2_TEXTURE_COORD_1          = $0DB3,
    GL_MAP2_TEXTURE_COORD_2          = $0DB4,
    GL_MAP2_TEXTURE_COORD_3          = $0DB5,
    GL_MAP2_TEXTURE_COORD_4          = $0DB6,
    GL_MAP2_VERTEX_3                 = $0DB7,
    GL_MAP2_VERTEX_4                 = $0DB8,
    GL_POLYGON_OFFSET_POINT          = $2A01,
    GL_POLYGON_OFFSET_LINE           = $2A02,
    GL_CLIP_PLANE0                   = $3000,
    GL_CLIP_PLANE1                   = $3001,
    GL_CLIP_PLANE2                   = $3002,
    GL_CLIP_PLANE3                   = $3003,
    GL_CLIP_PLANE4                   = $3004,
    GL_CLIP_PLANE5                   = $3005,
    GL_CLIP_PLANE_MAX                = Integer(GL_CLIP_PLANE0) + GL_MAX_CLIP_PLANES - 1,
    GL_LIGHT0                        = $4000,
    GL_LIGHT1                        = $4001,
    GL_LIGHT2                        = $4002,
    GL_LIGHT3                        = $4003,
    GL_LIGHT4                        = $4004,
    GL_LIGHT5                        = $4005,
    GL_LIGHT6                        = $4006,
    GL_LIGHT7                        = $4007,
    GL_LIGHT_MAX                     = Integer(GL_LIGHT0) + GL_MAX_LIGHTS - 1,
    GL_CONVOLUTION_1D                = $8010,
    GL_CONVOLUTION_2D                = $8011,
    GL_SEPARABLE_2D                  = $8012,
    GL_HISTOGRAM                     = $8024,
    GL_MINMAX                        = $802E,
    GL_POLYGON_OFFSET_FILL           = $8037,
    GL_RESCALE_NORMAL                = $803A,
    GL_MULTISAMPLE                   = $809D,
    GL_SAMPLE_ALPHA_TO_COVERAGE      = $809E,
    GL_SAMPLE_ALPHA_TO_ONE           = $809F,
    GL_SAMPLE_COVERAGE               = $80A0,
    GL_COLOR_TABLE                   = $80D0,
    GL_POST_CONVOLUTION_COLOR_TABLE  = $80D1,
    GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2,
    GL_COLOR_SUM                     = $8458,
    GL_VERTEX_PROGRAM_POINT_SIZE     = $8642,
    GL_VERTEX_PROGRAM_TWO_SIDE       = $8643,
    GL_POINT_SPRITE                  = $8861
  );

  GL_ClipPlane = GL_CLIP_PLANE0..GL_CLIP_PLANE_MAX;

  GL_TextureFormat = (
    // PixelFormat
    GL_COLOR_INDEX                    = $1900,
    GL_STENCIL_INDEX                  = $1901,
    GL_DEPTH_COMPONENT                = $1902,
    GL_RED                            = $1903,
    GL_GREEN                          = $1904,
    GL_BLUE                           = $1905,
    GL_ALPHA                          = $1906,
    GL_RGB                            = $1907,
    GL_RGBA                           = $1908,
    GL_LUMINANCE                      = $1909,
    GL_LUMINANCE_ALPHA                = $190A,
    // texture
    GL_R3_G3_B2                       = $2A10,
    GL_ALPHA4                         = $803B,
    GL_ALPHA8                         = $803C,
    GL_ALPHA12                        = $803D,
    GL_ALPHA16                        = $803E,
    GL_LUMINANCE4                     = $803F,
    GL_LUMINANCE8                     = $8040,
    GL_LUMINANCE12                    = $8041,
    GL_LUMINANCE16                    = $8042,
    GL_LUMINANCE4_ALPHA4              = $8043,
    GL_LUMINANCE6_ALPHA2              = $8044,
    GL_LUMINANCE8_ALPHA8              = $8045,
    GL_LUMINANCE12_ALPHA4             = $8046,
    GL_LUMINANCE12_ALPHA12            = $8047,
    GL_LUMINANCE16_ALPHA16            = $8048,
    GL_INTENSITY                      = $8049,
    GL_INTENSITY4                     = $804A,
    GL_INTENSITY8                     = $804B,
    GL_INTENSITY12                    = $804C,
    GL_INTENSITY16                    = $804D,
    GL_RGB4                           = $804F,
    GL_RGB5                           = $8050,
    GL_RGB8                           = $8051,
    GL_RGB10                          = $8052,
    GL_RGB12                          = $8053,
    GL_RGB16                          = $8054,
    GL_RGBA2                          = $8055,
    GL_RGBA4                          = $8056,
    GL_RGB5_A1                        = $8057,
    GL_RGBA8                          = $8058,
    GL_RGB10_A2                       = $8059,
    GL_RGBA12                         = $805A,
    GL_RGBA16                         = $805B,
    GL_TEXTURE_RED_SIZE               = $805C,
    GL_TEXTURE_GREEN_SIZE             = $805D,
    GL_TEXTURE_BLUE_SIZE              = $805E,
    GL_TEXTURE_ALPHA_SIZE             = $805F,
    GL_TEXTURE_LUMINANCE_SIZE         = $8060,
    GL_TEXTURE_INTENSITY_SIZE         = $8061,
    GL_PROXY_TEXTURE_1D               = $8063,
    GL_PROXY_TEXTURE_2D               = $8064,
    //GL_EXT_texture_compression_s3tc
    GL_COMPRESSED_RGB_S3TC_DXT1_EXT   = $83F0,
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT  = $83F1,
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT  = $83F2,
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT  = $83F3
  );

  GL_PixelFormat = GL_COLOR_INDEX..GL_LUMINANCE_ALPHA;
  GL_CompresedTextureFormat = GL_COMPRESSED_RGB_S3TC_DXT1_EXT..GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;

  GL_TexEnvTarget = (
    GL_TEXTURE_ENV_TEXENV               = $2300,
    GL_TEXTURE_FILTER_CONTROL_TEXENV    = $8500,
    GL_POINT_SPRITE_TEXENV              = Integer(GL_POINT_SPRITE)
  );

  GL_TexEnvTargetV = GL_TEXTURE_ENV_TEXENV..GL_TEXTURE_FILTER_CONTROL_TEXENV;

  GL_TextEnvParamName = (
    GL_ALPHA_SCALE               = $0D1C,
    GL_TEXTURE_ENV_MODE          = $2200,
    GL_TEXTURE_LOD_BIAS          = $8501,
    GL_COMBINE_RGB               = $8571,
    GL_COMBINE_ALPHA             = $8572,
    GL_RGB_SCALE                 = $8573,
    GL_SRC0_RGB                  = $8580,
    GL_SRC1_RGB                  = $8581,
    GL_SRC2_RGB                  = $8582,
    GL_SRC0_ALPHA                = $8588,
    GL_SRC1_ALPHA                = $8589,
    GL_SRC2_ALPHA                = $858A,
    GL_OPERAND0_RGB              = $8590,
    GL_OPERAND1_RGB              = $8591,
    GL_OPERAND2_RGB              = $8592,
    GL_OPERAND0_ALPHA            = $8598,
    GL_OPERAND1_ALPHA            = $8599,
    GL_OPERAND2_ALPHA            = $859A,
    GL_COORD_REPLACE             = $8862
  );

  GL_TextEnvParamNameV = (
    GL_TEXTURE_ENV_MODE_V        = Integer(GL_TEXTURE_ENV_MODE),
    GL_TEXTURE_ENV_COLOR         = $2201,
    GL_TEXTURE_LOD_BIAS_V        = Integer(GL_TEXTURE_LOD_BIAS)
  );

  GL_TextureEnum = (
    GL_TEXTURE0           = $84C0,
    GL_TEXTURE1           = $84C1,
    GL_TEXTURE2           = $84C2,
    GL_TEXTURE3           = $84C3,
    GL_TEXTURE4           = $84C4,
    GL_TEXTURE5           = $84C5,
    GL_TEXTURE6           = $84C6,
    GL_TEXTURE7           = $84C7,
    GL_TEXTURE8           = $84C8,
    GL_TEXTURE9           = $84C9,
    GL_TEXTURE10          = $84CA,
    GL_TEXTURE11          = $84CB,
    GL_TEXTURE12          = $84CC,
    GL_TEXTURE13          = $84CD,
    GL_TEXTURE14          = $84CE,
    GL_TEXTURE15          = $84CF,
    GL_TEXTURE16          = $84D0,
    GL_TEXTURE17          = $84D1,
    GL_TEXTURE18          = $84D2,
    GL_TEXTURE19          = $84D3,
    GL_TEXTURE20          = $84D4,
    GL_TEXTURE21          = $84D5,
    GL_TEXTURE22          = $84D6,
    GL_TEXTURE23          = $84D7,
    GL_TEXTURE24          = $84D8,
    GL_TEXTURE25          = $84D9,
    GL_TEXTURE26          = $84DA,
    GL_TEXTURE27          = $84DB,
    GL_TEXTURE28          = $84DC,
    GL_TEXTURE29          = $84DD,
    GL_TEXTURE30          = $84DE,
    GL_TEXTURE31          = $84DF
  );

  GL_TextEnvParam = (
    GL_ADD_TEXENV                = $0104,
    GL_BLEND_TEXENV              = $0BE2,
    GL_REPLACE_TEXENV            = $1E01,
    GL_MODULATE                  = $2100,
    GL_DECAL                     = $2101,
    GL_TEXTURE0_TEXENV           = Integer(GL_TEXTURE0),
    GL_TEXTURE1_TEXENV           = Integer(GL_TEXTURE1),
    GL_TEXTURE2_TEXENV           = Integer(GL_TEXTURE2),
    GL_TEXTURE3_TEXENV           = Integer(GL_TEXTURE3),
    GL_TEXTURE4_TEXENV           = Integer(GL_TEXTURE4),
    GL_TEXTURE5_TEXENV           = Integer(GL_TEXTURE5),
    GL_TEXTURE6_TEXENV           = Integer(GL_TEXTURE6),
    GL_TEXTURE7_TEXENV           = Integer(GL_TEXTURE7),
    GL_TEXTURE8_TEXENV           = Integer(GL_TEXTURE8),
    GL_TEXTURE9_TEXENV           = Integer(GL_TEXTURE9),
    GL_TEXTURE10_TEXENV          = Integer(GL_TEXTURE10),
    GL_TEXTURE11_TEXENV          = Integer(GL_TEXTURE11),
    GL_TEXTURE12_TEXENV          = Integer(GL_TEXTURE12),
    GL_TEXTURE13_TEXENV          = Integer(GL_TEXTURE13),
    GL_TEXTURE14_TEXENV          = Integer(GL_TEXTURE14),
    GL_TEXTURE15_TEXENV          = Integer(GL_TEXTURE15),
    GL_TEXTURE16_TEXENV          = Integer(GL_TEXTURE16),
    GL_TEXTURE17_TEXENV          = Integer(GL_TEXTURE17),
    GL_TEXTURE18_TEXENV          = Integer(GL_TEXTURE18),
    GL_TEXTURE19_TEXENV          = Integer(GL_TEXTURE19),
    GL_TEXTURE20_TEXENV          = Integer(GL_TEXTURE20),
    GL_TEXTURE21_TEXENV          = Integer(GL_TEXTURE21),
    GL_TEXTURE22_TEXENV          = Integer(GL_TEXTURE22),
    GL_TEXTURE23_TEXENV          = Integer(GL_TEXTURE23),
    GL_TEXTURE24_TEXENV          = Integer(GL_TEXTURE24),
    GL_TEXTURE25_TEXENV          = Integer(GL_TEXTURE25),
    GL_TEXTURE26_TEXENV          = Integer(GL_TEXTURE26),
    GL_TEXTURE27_TEXENV          = Integer(GL_TEXTURE27),
    GL_TEXTURE28_TEXENV          = Integer(GL_TEXTURE28),
    GL_TEXTURE29_TEXENV          = Integer(GL_TEXTURE29),
    GL_TEXTURE30_TEXENV          = Integer(GL_TEXTURE30),
    GL_TEXTURE31_TEXENV          = Integer(GL_TEXTURE31),
    GL_SUBTRACT                  = $84E7,
    GL_COMBINE                   = $8570,
    GL_ADD_SIGNED                = $8574,
    GL_INTERPOLATE               = $8575,
    GL_CONSTANT                  = $8576,
    GL_PRIMARY_COLOR             = $8577,
    GL_PREVIOUS                  = $8578{,
    GL_SRC_COLOR                 = $0300,
    GL_ONE_MINUS_SRC_COLOR       = $0301,
    GL_SRC_ALPHA                 = $0302,
    GL_ONE_MINUS_SRC_ALPHA       = $0303 }
  );

  GL_QueryTarget = (
    GL_ANY_SAMPLES_PASSED                    = $8C2F,
    GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88,
    GL_ANY_SAMPLES_PASSED_CONSERVATIVE       = $8D6A
  );

  GL_Modes = (
    GL_POINTS_ARB                         = $0000,
    GL_LINES_ARB                          = $0001,
    GL_LINE_LOOP_ARB                      = $0002,
    GL_LINE_STRIP_ARB                     = $0003,
    GL_TRIANGLES_ARB                      = $0004,
    GL_TRIANGLE_STRIP_ARB                 = $0005,
    GL_TRIANGLE_FAN_ARB                   = $0006,
    GL_QUADS_ARB                          = $0007,
    GL_QUAD_STRIP_ARB                     = $0008,
    GL_POLYGON_ARB                        = $0009
  );

  GL_PrimitiveMode = (
    GL_POINTS_PM     = Integer(GL_POINTS_ARB),
    GL_LINES_PM      = Integer(GL_LINES_ARB),
    GL_TRIANGLES_PM  = Integer(GL_TRIANGLES_ARB)
  );

  GL_DrawMode = (
    GL_POINTS          = Integer(GL_POINTS_ARB),
    GL_LINES           = Integer(GL_LINES_ARB),
    GL_LINE_LOOP       = Integer(GL_LINE_LOOP_ARB),
    GL_LINE_STRIP      = Integer(GL_LINE_STRIP_ARB),
    GL_TRIANGLES       = Integer(GL_TRIANGLES_ARB),
    GL_TRIANGLE_STRIP  = Integer(GL_TRIANGLE_STRIP_ARB),
    GL_TRIANGLE_FAN    = Integer(GL_TRIANGLE_FAN_ARB)
  );

  GL_BufferTarget = (
    GL_ARRAY_BUFFER                   = $8892,
    GL_ELEMENT_ARRAY_BUFFER           = $8893,
    GL_PIXEL_PACK_BUFFER              = $88EB,
    GL_PIXEL_UNPACK_BUFFER            = $88EC,
    GL_UNIFORM_BUFFER                 = $8A11,
    GL_TEXTURE_BUFFER_TRG             = Integer(GL_TEXTURE_BUFFER),
    GL_TRANSFORM_FEEDBACK_BUFFER      = $8C8E,
    GL_COPY_READ_BUFFER               = $8F36,
    GL_COPY_WRITE_BUFFER              = $8F37,
    GL_DRAW_INDIRECT_BUFFER           = $8F3F,
    GL_SHADER_STORAGE_BUFFER          = $90D2,
    GL_DISPATCH_INDIRECT_BUFFER       = $90EE,
    GL_QUERY_BUFFER                   = $9192,
    GL_ATOMIC_COUNTER_BUFFER          = $92C0
  );

  GL_SubBufferTarget = GL_ARRAY_BUFFER..GL_ELEMENT_ARRAY_BUFFER;

  GL_BufferBaseTarget = (
    GL_UNIFORM_BUFFER_BASE            = Integer(GL_UNIFORM_BUFFER),
    GL_TRANSFORM_FEEDBACK_BUFFER_BASE = Integer(GL_TRANSFORM_FEEDBACK_BUFFER)
  );

  GL_BufferRangeTarget = (
    GL_UNIFORM_BUFFER_RNG             = Integer(GL_UNIFORM_BUFFER),
    GL_TRANSFORM_FEEDBACK_BUFFER_RNG  = Integer(GL_TRANSFORM_FEEDBACK_BUFFER),
    GL_SHADER_STORAGE_BUFFER_RNG      = Integer(GL_SHADER_STORAGE_BUFFER),
    GL_ATOMIC_COUNTER_BUFFER_RNG      = Integer(GL_ATOMIC_COUNTER_BUFFER)
  );

  GL_FrameBufferTarget = (
    GL_READ_FRAMEBUFFER               = $8CA8,
    GL_DRAW_FRAMEBUFFER               = $8CA9,
    GL_FRAMEBUFFER                    = $8D40
  );

  GL_Access = (
    GL_READ_ONLY                      = $88B8,
    GL_WRITE_ONLY                     = $88B9,
    GL_READ_WRITE                     = $88BA
  );

  GL_ImageUnitFormat = (
    GL_R32F                           = $822E,
    GL_RG16F                          = $822F,
    GL_R8I                            = $8231,
    GL_R8UI                           = $8232,
    GL_R16I                           = $8233,
    GL_R16UI                          = $8234,
    GL_R32I                           = $8235,
    GL_R32UI                          = $8236,
    GL_RG8I                           = $8237,
    GL_RG8UI                          = $8238,
    GL_RG16I                          = $8239,
    GL_RG16UI                         = $823A,
    GL_RG32I                          = $823B,
    GL_RG32UI                         = $823C,
    GL_RGBA32F                        = $8814,
    GL_RGB32F                         = $8815,
    GL_RGBA16F                        = $881A,
    GL_RGB16F                         = $881B,
    GL_R11F_G11F_B10F                 = $8C3A,
    GL_RGBA32UI                       = $8D70,
    GL_RGB32UI                        = $8D71,
    GL_RGBA16UI                       = $8D76,
    GL_RGB16UI                        = $8D77,
    GL_RGBA8UI                        = $8D7C,
    GL_RGB8UI                         = $8D7D,
    GL_RGBA32I                        = $8D82,
    GL_RGB32I                         = $8D83,
    GL_RGBA16I                        = $8D88,
    GL_RGB16I                         = $8D89,
    GL_RGBA8I                         = $8D8E,
    GL_RGB8I                          = $8D8F,
    GL_R8_SNORM                       = $8F94,
    GL_RG8_SNORM                      = $8F95,
    GL_RGBA8_SNORM                    = $8F97,
    GL_R16_SNORM                      = $8F98,
    GL_RG16_SNORM                     = $8F99,
    GL_RGBA16_SNORM                   = $8F9B,
    GL_RGB10_A2UI                     = $906F
  );

  GL_RenderBufferTarget = (
    GL_RENDERBUFFER = $8D41
  );

  GL_TexParamTarget = (
    GL_TEXTURE_1D_TEX = Integer(GL_TEXTURE_1D_ARB),
    GL_TEXTURE_2D_TEX = Integer(GL_TEXTURE_2D_ARB),
    GL_TEXTURE_3D_TEX = Integer(GL_TEXTURE_3D_ARB),
    GL_TEXTURE_CUBE_MAP_TEX = Integer(GL_TEXTURE_CUBE_MAP_ARB)
  );

  GL_TexParamVTarget = (
    GL_TEXTURE_1D_TEXV = Integer(GL_TEXTURE_1D_ARB),
    GL_TEXTURE_2D_TEXV = Integer(GL_TEXTURE_2D_ARB),
    GL_TEXTURE_3D_TEXV = Integer(GL_TEXTURE_3D_ARB)
  );

  GL_TexParamName = (
    GL_TEXTURE_MAG_FILTER             = $2800,
    GL_TEXTURE_MIN_FILTER             = $2801,
    GL_TEXTURE_WRAP_S                 = $2802,
    GL_TEXTURE_WRAP_T                 = $2803,
    GL_TEXTURE_PRIORITY               = $8066,
    GL_TEXTURE_WRAP_R                 = $8072,
    GL_TEXTURE_MIN_LOD                = $813A,
    GL_TEXTURE_MAX_LOD                = $813B,
    GL_TEXTURE_BASE_LEVEL             = $813C,
    GL_TEXTURE_MAX_LEVEL              = $813D,
    GL_GENERATE_MIPMAP                = $8191,
    GL_DEPTH_TEXTURE_MODE             = $884B,
    GL_TEXTURE_COMPARE_FUNC           = $884D
  );

  GL_TexParamValue = (
    GL_NEAREST                        = $2600,
    GL_LINEAR                         = $2601,
    GL_NEAREST_MIPMAP_NEAREST         = $2700,
    GL_LINEAR_MIPMAP_NEAREST          = $2701,
    GL_NEAREST_MIPMAP_LINEAR          = $2702,
    GL_LINEAR_MIPMAP_LINEAR           = $2703,
    GL_REPEAT                         = $2901,
    GL_CLAMP_TO_EDGE                  = $812F
  );

  GL_PixelDataType = (
    GL_BYTE                           = $1400,
    GL_UNSIGNED_BYTE                  = $1401,
    GL_SHORT                          = $1402,
    GL_UNSIGNED_SHORT                 = $1403,
    GL_INT                            = $1404,
    GL_UNSIGNED_INT                   = $1405,
    GL_FLOAT                          = $1406,
    GL_BITMAP                         = $1A00,
    GL_UNSIGNED_BYTE_3_3_2            = $8032,
    GL_UNSIGNED_SHORT_4_4_4_4         = $8033,
    GL_UNSIGNED_SHORT_5_5_5_1         = $8034,
    GL_UNSIGNED_INT_10_10_10_2        = $8036,
    GL_UNSIGNED_BYTE_2_3_3_REV        = $8362,
    GL_UNSIGNED_SHORT_5_6_5           = $8363,
    GL_UNSIGNED_SHORT_5_6_5_REV       = $8364,
    GL_UNSIGNED_SHORT_4_4_4_4_REV     = $8365,
    GL_UNSIGNED_SHORT_1_5_5_5_REV     = $8366,
    GL_UNSIGNED_INT_8_8_8_8_REV       = $8367,
    GL_UNSIGNED_INT_2_10_10_10_REV    = $8368
  );

  GL_IndicesType = (
    GL_UNSIGNED_BYTE_IND = Integer(GL_UNSIGNED_BYTE),
    GL_UNSIGNED_SHORT_IND = Integer(GL_UNSIGNED_SHORT)
  );

  GL_ClientState = (
    GL_VERTEX_ARRAY                   = $8074,
    GL_NORMAL_ARRAY                   = $8075,
    GL_COLOR_ARRAY                    = $8076,
    GL_TEXTURE_COORD_ARRAY            = $8078
  );

  GL_CordDataType = (
    GL_SHORT_DATA                     = Integer(GL_SHORT),
    GL_FLOAT_DATA                     = Integer(GL_FLOAT),
    GL_DOUBLE_DATA                    = $140A
  );

  GL_MatrixMode = (
    GL_MODELVIEW                      = $1700,
    GL_PROJECTION                     = $1701,
    GL_TEXTURE_MD                     = $1702
  );

  GL_ShaderType = (
    GL_FRAGMENT_SHADER = $8B30,
    GL_VERTEX_SHADER = $8B31
  );

  GL_ShaderParam = (
    GL_SHADER_TYPE = $8B4F,
    GL_SHADER_DELETE_STATUS = $8B80,
    GL_COMPILE_STATUS = $8B81,
    GL_INFO_SHADER_LOG_LENGTH = $8B84,
    GL_SHADER_SOURCE_LENGTH = $8B88
  );

  GL_ProgramParam = (
    GL_PROGRAM_DELETE_STATUS = $8B80,
    GL_LINK_STATUS = $8B82,
    GL_VALIDATE_STATUS = $8B83,
    GL_INFO_PROGRAM_LOG_LENGTH = $8B84,
    GL_ATTACHED_SHADERS = $8B85,
    GL_ACTIVE_UNIFORMS = $8B86,
    GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87,
    GL_ACTIVE_ATTRIBUTES = $8B89,
    GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A
  );

  GL_BlendFactors = (
    GL_ZERO = 0,
    GL_ONE = 1,
    GL_SRC_COLOR = $0300,
    GL_ONE_MINUS_SRC_COLOR = $0301,
    GL_SRC_ALPHA = $0302,
    GL_ONE_MINUS_SRC_ALPHA = $0303,
    GL_DST_ALPHA = $0304,
    GL_ONE_MINUS_DST_ALPHA = $0305,
    GL_DST_COLOR = $0306,
    GL_ONE_MINUS_DST_COLOR = $0307,
    GL_SRC_ALPHA_SATURATE = $0308,
    GL_CONSTANT_COLOR = $8001,
    GL_ONE_MINUS_CONSTANT_COLOR = $8002,
    GL_CONSTANT_ALPHA = $8003,
    GL_ONE_MINUS_CONSTANT_ALPHA = $8004
  );

  GL_Boolean = (
    GL_FALSE,
    GL_TRUE
  );

  GL_BlendEquation = (
    GL_FUNC_ADD = $00008006,
    GL_FUNC_SUBTRACT = $0000800a,
    GL_FUNC_REVERSE_SUBTRACT = $0000800b
  );

  GL_BufferDataUsage = (
    GL_STREAM_DRAW = $000088e0,
    GL_STREAM_READ = $000088E1,
    GL_STREAM_COPY = $000088E2,
    GL_STATIC_DRAW = $000088E4,
    GL_STATIC_READ = $000088E5,
    GL_STATIC_COPY = $000088E6,
    GL_DYNAMIC_DRAW = $000088E8,
    GL_DYNAMIC_READ = $000088E9,
    GL_DYNAMIC_COPY = $000088EA
  );

  FL_FrameBufferTargetStatus = (
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $00008cd6,
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $00008cd7,
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $00008cd9,
    GL_FRAMEBUFFER_UNSUPPORTED = $00008cdd
  );

  GL_CullFaceMode = (
    GL_FRONT = $00000404,
    GL_BACK = $00000405,
    GL_FRONT_AND_BACK = $00000408
  );

  GL_RenderBufferAttachemnt = (
    GL_COLOR_ATTACHMENT1 = $000008CE1,
    GL_COLOR_ATTACHMENT2 = $000008CE2,
    GL_COLOR_ATTACHMENT3 = $000008CE3,
    GL_COLOR_ATTACHMENT4 = $000008CE4,
    GL_COLOR_ATTACHMENT5 = $000008CE5,
    GL_COLOR_ATTACHMENT6 = $000008CE6,
    GL_COLOR_ATTACHMENT7 = $000008CE7,
    GL_COLOR_ATTACHMENT8 = $000008CE8,
    GL_COLOR_ATTACHMENT9 = $000008CE9,
    GL_COLOR_ATTACHMENT10 = $000008CEA,
    GL_COLOR_ATTACHMENT11 = $000008CEB,
    GL_COLOR_ATTACHMENT12 = $000008CEC,
    GL_COLOR_ATTACHMENT13 = $000008CED,
    GL_COLOR_ATTACHMENT14 = $000008CEE,
    GL_COLOR_ATTACHMENT15 = $000008CEF,
    GL_DEPTH_ATTACHMENT = $000008D00,
    GL_STENCIL_ATTACHMENT = $000008D20
  );

  GL_Error = (
    GL_NO_ERROR = $00000000,
    GL_INVALID_ENUM = $00000500,
    GL_INVALID_VALUE = $00000501,
    GL_INVALID_OPERATION = $00000502,
    GL_OUT_OF_MEMORY = $00000505,
    GL_INVALID_FRAMEBUFFER_OPERATION = $00000506
  );

  PGL_Plane = ^GL_Plane;
  GL_Plane = record
    A: GLfloat;
    B: GLfloat;
    C: GLfloat;
    D: GLfloat;
  end;

  Vector4f = array [0..3] of GLfloat;

  PVector4f = ^Vector4f;

  { TOpenGLVector }

  TOpenGLVector = record
  type
    TOpenGLVectorData = array [0..2] of GLfloat;
  private
    Data: TOpenGLVectorData;
    function GetItems(Index: Integer): GLfloat;
    procedure SetItems(Index: Integer; AValue: GLfloat);
  public
    procedure Normalize;
    procedure cross(v2: TOpenGLVector; out result: TOpenGLVector);

    property Items[Index: Integer]: GLfloat read GetItems write SetItems; default;
  end;

  { TOpenGLMatrix }

  POpenGLMatrix = ^TOpenGLMatrix;
  TOpenGLMatrix = record
  type
    TOpenGLMatrixData = array[0..3,0..3] of GLfloat;
  const
    PI = 3.1415926535897932384626433832795;
    PI_OVER_180 = 0.017453292519943295769236907684886;
    PI_OVER_360 = 0.0087266462599716478846184538424431;
  private
    Data: TOpenGLMatrixData;
    function GetItems(Y, X: Integer): GLfloat;
    procedure SetItems(Y, X: Integer; AValue: GLfloat);
  public
    procedure glLoadIdentity;
    procedure glMultMatrix(MatrixA: TOpenGLMatrix);
    procedure gluPerspective(fov, aspect,zNear, zFar: GLfloat);
    procedure glFrustum(left, right, bottom, top, znear, zfar: GLfloat);
    procedure glRotate(a, x, y, z: GLfloat);
    procedure glScalef(x, y, z: GLfloat);
    procedure glTranslatef(x, y, z: GLfloat);
    procedure gluLookAt(eyeX: GLfloat; eyeY: GLfloat; eyeZ: GLfloat; centerX: GLfloat; centerY: GLfloat; centerZ:GLfloat; upX:GLfloat; upY:GLfloat; upZ:GLfloat);

    property Items[Y, X: Integer]: GLfloat read GetItems write SetItems; default;
  end;

  { GL_Matrix }

  PGL_Matrix = ^GL_Matrix;
  GL_Matrix = record
  private
    function GetItem(x: Integer; y: Integer): GLfloat;
    procedure SetItem(x: Integer; y: Integer; AValue: GLfloat);
  public
    m: array[0..15] of GLfloat;

    property Item[x: Integer; y: Integer]: GLfloat read GetItem write SetItem;
  end;

  { GL_Textures }

  GL_Textures = record
  public
    class function Texture(num: Integer): GL_TextureEnum; static;
  end;

  { GL_Color }

  PGL_Color = ^GL_Color;
  GL_Color = record
  public
    r: GLfloat;
    g: GLfloat;
    b: GLfloat;
    a: GLfloat;

    procedure Make(out Color: GL_Color; ar, ag, ab, aa: GLfloat);
    procedure Make(out Color: GL_Color; ar, ag, ab, aa: GLbyte);
    procedure Make(out Color: GL_Color; Clr: GLint; Alpha: GLbyte);

    function Make(ar, ag, ab, aa: GLfloat): GL_Color;
    function Make(ar, ag, ab, aa: GLbyte): GL_Color;
    function Make(Clr: GLint; Alpha: GLbyte): GL_Color;
  end;
  GL_Colors = array of GL_Color;

{$i sdl.api.opengl.operators.h.inc}

implementation

uses
  SDL.Api.OpenGL.Helpers, math;

{$i sdl.api.opengl.operators.b.inc}

end.

