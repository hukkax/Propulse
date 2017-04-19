unit SDL.Api.OpenGL;

{$mode objfpc}{$H+}
{$MACRO ON}
{$IFDEF Windows}
  {$DEFINE extdecl := stdcall}
{$ELSE}
  {$DEFINE extdecl := cdecl}
  {$IFDEF MorphOS}
    {$INLINE ON}
    {$DEFINE GL_UNIT}
  {$ELSE}
   {$IFNDEF OS2}
    {$LINKLIB c}
   {$ENDIF OS2}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DLibrary, SDL.Api.OpenGL.Types;

type

  { TOpenGLEs }

  TOpenGLEs = class abstract(TSubLibrary)
  end;

  { TOpenGLEs_2_0 }

  TOpenGLEs_2_0 = class(TOpenGLEs)
  private type
    TglActiveTexture =  procedure(texture: GL_TextureEnum) extdecl;
    TglAttachShader = procedure(prog: GL_Program; shader: GL_Shader) extdecl;
    TglBindAttribLocation = procedure(prog: GL_Program; index: GL_VertexAtr; const name: GLchar) extdecl;
    TglBindBuffer = procedure(target: GL_BufferTarget; buffer: GL_Buffer) extdecl;
    TglBindFramebuffer = procedure(target: GL_FrameBufferTarget; framebuffer: GL_FrmBuffer) extdecl;
    TglBindRenderbuffer = procedure(target: GL_RenderBufferTarget; renderbuffer: GL_RenderBuff) extdecl;
    TglBindTexture = procedure(target: GL_BindTarget; texture: GL_Texture) extdecl;
    TglBlendColor = procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf) extdecl;
    TglBlendEquation = procedure(mode: GL_BlendEquation) extdecl;
    TglBlendEquationSeparate = procedure(modeRGB: GL_BlendEquation;  modeAlpha: GL_BlendEquation) extdecl;
    TglBlendFunc = procedure(sfactor, dfactor: GL_BlendFactors) extdecl; (* recheck *)
    TglBlendFuncSeparate = procedure(srcRGB, dstRGB, srcAlpha, dstAlpha: GL_BlendFactors) extdecl; (* recheck *)
    TglBufferData = procedure(target: GL_BufferTarget; size: GLsizeiptr; data: GLvoid; usage: GL_BufferDataUsage) extdecl;
    TglBufferSubData = procedure(target: GL_SubBufferTarget; offset: GLintptr; size: GLsizeiptr; data: GLvoid) extdecl;
    TglCheckFramebufferStatus = function(target: GL_FrameBufferTarget): FL_FrameBufferTargetStatus  extdecl;
    TglClear = procedure(mask: GL_ClearBufferTypes); extdecl;
    TglClearColor = procedure (red, green, blue, alpha: GLclampf); extdecl;
    TglClearDepthf = procedure(depth: GLfloat) extdecl;
    TglClearStencil = procedure(s: GLint) extdecl;
    TglColorMask = procedure(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean) extdecl;
    TglCompileShader = procedure(shader: GL_Shader) extdecl;
    TglCompressedTexImage2D = procedure(target: GL_TexImageTarget; level: GLint; internalformat: GL_CompresedTextureFormat; width, height: GLsizei; border: GLint; imageSize: GLsizei; data: PGLvoid) extdecl;
    TglCompressedTexSubImage2D = procedure(target: GL_TexImageTarget; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GL_CompresedTextureFormat; imageSize: GLsizei; data: GLvoid) extdecl;
    TglCopyTexImage2D = procedure(target: GL_TexImageTarget; level: GLint; internalformat: GL_CompresedTextureFormat; x, y: GLint; width, height: GLsizei; border: GLint) extdecl;
    TglCopyTexSubImage2D = procedure(target: GL_TexImageTarget; level: GLint; xoffset, yoffset: GLint; x, y: GLint; width, height: GLsizei) extdecl;
    TglCreateProgram = function(): GL_Program extdecl;
    TglCreateShader = function(shaderType: GL_ShaderType): GL_Shader extdecl;
    TglCullFace = procedure(mode: GL_CullFaceMode) extdecl;
    TglDeleteBuffers = procedure(n: GLsizei; buffers: PGL_Buffer) extdecl;
    TglDeleteFramebuffers = procedure(n: GLint; framebuffers: PGL_FrmBuffer) extdecl;
    TglDeleteProgram = procedure(p: GL_Program) extdecl;
    TglDeleteRenderbuffers = procedure(n: GLsizei; renderbuffers: PGL_RenderBuff) extdecl;
    TglDeleteShader = procedure(shader: GL_Shader) extdecl;
    TglDeleteTextures = procedure(n: GLsizei; textures: PGL_Texture) extdecl;
    TglDepthFunc = procedure(func: GL_AlphaFunction) extdecl;
    TglDepthMask = procedure(flag: GLboolean) extdecl;
    TglDepthRangef = procedure(n, f: GLfloat) extdecl;
    TglDetachShader = procedure(p: GL_Program; shader: GL_Shader) extdecl;
    TglDisable =  procedure(cap: GL_Capability) extdecl;
    TglDisableVertexAttribArray = procedure(index: GLuint) extdecl;
    TglDrawArrays = procedure(mode: GL_DrawMode; first: GLint; count: GLsizei) extdecl;
    TglDrawElements =  procedure(mode: GL_DrawMode; count: GLsizei; indType: GL_IndicesType; indices: GLvoid) extdecl;
    TglEnable = procedure(cap: GL_Capability) extdecl;
    TglEnableVertexAttribArray = procedure(index: GLuint) extdecl;
    TglFinish = procedure() extdecl;
    TglFlush = procedure() extdecl;
    TglFramebufferRenderbuffer = procedure(target: GL_FrmBuffer; attachment: GL_RenderBufferAttachemnt; renderbuffertarget: GL_RenderBufferTarget; renderbuffer: GL_RenderBuff) extdecl;
    TglFramebufferTexture2D = procedure() extdecl;
    TglFrontFace = procedure() extdecl;
    TglGenBuffers = procedure() extdecl;
    TglGenerateMipmap = procedure(target: GL_MinimapTarget) extdecl;
    TglGenFramebuffers = procedure() extdecl;
    TglGenRenderbuffers = procedure() extdecl;
    TglGenTextures = procedure(n: GLsizei; textures: PGL_Texture) extdecl;
    TglGetActiveAttrib = procedure() extdecl;
    TglGetActiveUniform = procedure() extdecl;
    TglGetAttachedShaders = procedure() extdecl;
    TglGetAttribLocation = function(p: GL_Program; name: GLchar): GLint extdecl;
    TglGetBooleanv = procedure() extdecl;
    TglGetBufferParameteriv = procedure() extdecl;
    TglGetError = function(): GL_Error extdecl;
    TglGetFloatv = procedure() extdecl;
    TglGetFramebufferAttachmentParameteriv = procedure() extdecl;
    TglGetIntegerv = procedure() extdecl;
    TglGetPerfMonitorCounterDataAMD = procedure() extdecl;
    TglGetPerfMonitorCounterInfoAMD = procedure() extdecl;
    TglGetPerfMonitorCountersAMD = procedure() extdecl;
    TglGetPerfMonitorCounterStringAMD = procedure() extdecl;
    TglGetPerfMonitorGroupsAMD = procedure() extdecl;
    TglGetPerfMonitorGroupStringAMD = procedure() extdecl;
    TglGetProgramBinaryOES = procedure() extdecl;
    TglGetProgramInfoLog = procedure() extdecl;
    TglGetProgramiv = procedure(p: GL_Program; pname: GL_ProgramParam;  params: PGLint) extdecl;
    TglGetRenderbufferParameteriv = procedure() extdecl;
    TglGetShaderInfoLog = procedure(shader: GL_Shader; maxLength: GLsizei; length: PGLsizei; infoLog: GLchar) extdecl;
    TglGetShaderiv = procedure(shader: GL_Shader; pname: GL_ShaderParam; params: PGLint) extdecl;
    TglGetShaderPrecisionFormat = procedure() extdecl;
    TglGetShaderSource = procedure() extdecl;
    TglGetString = procedure() extdecl;
    TglGetTexParameterfv = procedure() extdecl;
    TglGetTexParameteriv = procedure() extdecl;
    TglGetUniformfv = procedure() extdecl;
    TglGetUniformiv = procedure() extdecl;
    TglGetUniformLocation = function(p: GL_Program; name: GLchar): GLint extdecl;
    TglGetVertexAttribfv = procedure() extdecl;
    TglGetVertexAttribiv = procedure() extdecl;
    TglGetVertexAttribPointerv = procedure() extdecl;
    TglHint = procedure() extdecl;
    TglIsBuffer = procedure() extdecl;
    TglIsEnabled = procedure() extdecl;
    TglIsFramebuffer = procedure() extdecl;
    TglIsProgram = procedure() extdecl;
    TglIsRenderbuffer = procedure() extdecl;
    TglIsShader = procedure() extdecl;
    TglIsTexture = procedure() extdecl;
    TglLineWidth = procedure() extdecl;
    TglLinkProgram = procedure(p: GL_Program) extdecl;
    TglPixelStorei = procedure() extdecl;
    TglPolygonOffset = procedure() extdecl;
    TglReadPixels = procedure() extdecl;
    TglReleaseShaderCompiler = procedure() extdecl;
    TglRenderbufferStorage = procedure() extdecl;
    TglSampleCoverage = procedure() extdecl;
    TglScissor = procedure() extdecl;
    TglShaderBinary = procedure() extdecl;
    TglShaderSource = procedure(shader: GL_Shader; count: GLsizei; source: PGLchar; length: PGLint) extdecl;
    TglStencilFunc = procedure() extdecl;
    TglStencilFuncSeparate = procedure() extdecl;
    TglStencilMask = procedure() extdecl;
    TglStencilMaskSeparate = procedure() extdecl;
    TglStencilOp = procedure() extdecl;
    TglStencilOpSeparate = procedure() extdecl;
    TglTexImage2D = procedure(target: GL_TexImageTarget; level: GLint; internalFormat: GL_CompresedTextureFormat; width, height: GLsizei; border: GLint; format: GL_TextureFormat; _type: GL_PixelDataType; data: GLvoid) extdecl;
    TglTexParameterf = procedure() extdecl;
    TglTexParameterfv = procedure() extdecl;
    TglTexParameteri = procedure(target: GL_TexParamTarget; pname: GL_TexParamName; param: GL_TexParamValue) extdecl;
    TglTexParameteriv = procedure() extdecl;
    TglTexSubImage2D = procedure() extdecl;
    TglUniform1f = procedure(location: GLint; v0: GLfloat) extdecl;
    TglUniform1fv = procedure(location: GLint; count: GLsizei; value: PGLfloat) extdecl;
    TglUniform1i = procedure(location: GLint; v0: GLint) extdecl;
    TglUniform1iv = procedure(location: GLint; count: GLsizei; value: PGLint) extdecl;
    TglUniform2f = procedure(location: GLint; v0, v1: GLfloat) extdecl;
    TglUniform2fv = procedure(location: GLint; count: GLsizei; value: PGLfloat) extdecl;
    TglUniform2i = procedure(location: GLint; v0, v1: GLint) extdecl;
    TglUniform2iv = procedure(location: GLint; count: GLsizei; value: PGLint) extdecl;
    TglUniform3f = procedure(location: GLint; v0, v1, v2: GLfloat) extdecl;
    TglUniform3fv = procedure(location: GLint; count: GLsizei; value: PGLfloat) extdecl;
    TglUniform3i = procedure(location: GLint; v0, v1, v2: GLint) extdecl;
    TglUniform3iv = procedure(location: GLint; count: GLsizei; value: PGLint) extdecl;
    TglUniform4f = procedure(location: GLint; v0, v1, v2, v3: GLfloat) extdecl;
    TglUniform4fv = procedure(location: GLint; count: GLsizei; value: PGLfloat) extdecl;
    TglUniform4i = procedure(location: GLint; v0, v1, v2, v3: GLint) extdecl;
    TglUniform4iv = procedure(location: GLint; count: GLsizei; value: PGLint) extdecl;
    TglUniformMatrix2fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat) extdecl;
    TglUniformMatrix3fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat) extdecl;
    TglUniformMatrix4fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: POpenGLMatrix) extdecl;
    TglUseProgram = procedure(p: GL_Program) extdecl;
    TglValidateProgram = procedure() extdecl;
    TglVertexAttrib1f = procedure() extdecl;
    TglVertexAttrib1fv = procedure() extdecl;
    TglVertexAttrib2f = procedure() extdecl;
    TglVertexAttrib2fv = procedure() extdecl;
    TglVertexAttrib3f = procedure() extdecl;
    TglVertexAttrib3fv = procedure() extdecl;
    TglVertexAttrib4f = procedure() extdecl;
    TglVertexAttrib4fv = procedure() extdecl;
    TglVertexAttribPointer = procedure(index: GLuint; size: GLint; _type: GL_CordDataType; normalized: GLboolean; stride: GLsizei; pointer: GLvoid) extdecl;
    TglViewport = procedure(x, y: GLint;  width, height: GLsizei) extdecl;
  public
    glActiveTexture: TglActiveTexture;
    glAttachShader: TglAttachShader;
    glBindAttribLocation: TglBindAttribLocation;
    glBindBuffer: TglBindBuffer;
    glBindFramebuffer: TglBindFramebuffer;
    glBindRenderbuffer: TglBindRenderbuffer;
    glBindTexture: TglBindTexture;
    glBlendColor: TglBlendColor;
    glBlendEquation: TglBlendEquation;
    glBlendEquationSeparate: TglBlendEquationSeparate;
    glBlendFunc: TglBlendFunc;
    glBlendFuncSeparate: TglBlendFuncSeparate;
    glBufferData: TglBufferData;
    glBufferSubData: TglBufferSubData;
    glCheckFramebufferStatus: TglCheckFramebufferStatus;
    glClear: TglClear;
    glClearColor: TglClearColor;
    glClearDepthf: TglClearDepthf;
    glClearStencil: TglClearStencil;
    glColorMask: TglColorMask;
    glCompileShader: TglCompileShader;
    glCompressedTexImage2D: TglCompressedTexImage2D;
    glCompressedTexSubImage2D: TglCompressedTexSubImage2D;
    glCopyTexImage2D: TglCopyTexImage2D;
    glCopyTexSubImage2D: TglCopyTexSubImage2D;
    glCreateProgram: TglCreateProgram;
    glCreateShader: TglCreateShader;
    glCullFace: TglCullFace;
    glDeleteBuffers: TglDeleteBuffers;
    glDeleteFramebuffers: TglDeleteFramebuffers;
    glDeleteProgram: TglDeleteProgram;
    glDeleteRenderbuffers: TglDeleteRenderbuffers;
    glDeleteShader: TglDeleteShader;
    glDeleteTextures: TglDeleteTextures;
    glDepthFunc: TglDepthFunc;
    glDepthMask: TglDepthMask;
    glDepthRangef: TglDepthRangef;
    glDetachShader: TglDetachShader;
    glDisable: TglDisable;
    glDisableVertexAttribArray: TglDisableVertexAttribArray;
    glDrawArrays: TglDrawArrays;
    glDrawElements: TglDrawElements;
    glEnable: TglEnable;
    glEnableVertexAttribArray: TglEnableVertexAttribArray;
    glFinish: TglFinish;
    glFlush: TglFlush;
    glFramebufferRenderbuffer: TglFramebufferRenderbuffer;
    glFramebufferTexture2D: TglFramebufferTexture2D;
    glFrontFace: TglFrontFace;
    glGenBuffers: TglGenBuffers;
    glGenerateMipmap: TglGenerateMipmap;
    glGenFramebuffers: TglGenFramebuffers;
    glGenRenderbuffers: TglGenRenderbuffers;
    glGenTextures: TglGenTextures;
    glGetActiveAttrib: TglGetActiveAttrib;
    glGetActiveUniform: TglGetActiveUniform;
    glGetAttachedShaders: TglGetAttachedShaders;
    glGetAttribLocation: TglGetAttribLocation;
    glGetBooleanv: TglGetBooleanv;
    glGetBufferParameteriv: TglGetBufferParameteriv;
    glGetError: TglGetError;
    glGetFloatv: TglGetFloatv;
    glGetFramebufferAttachmentParameteriv: TglGetFramebufferAttachmentParameteriv;
    glGetIntegerv: TglGetIntegerv;
    glGetPerfMonitorCounterDataAMD: TglGetPerfMonitorCounterDataAMD;
    glGetPerfMonitorCounterInfoAMD: TglGetPerfMonitorCounterInfoAMD;
    glGetPerfMonitorCountersAMD: TglGetPerfMonitorCountersAMD;
    glGetPerfMonitorCounterStringAMD: TglGetPerfMonitorCounterStringAMD;
    glGetPerfMonitorGroupsAMD: TglGetPerfMonitorGroupsAMD;
    glGetPerfMonitorGroupStringAMD: TglGetPerfMonitorGroupStringAMD;
    glGetProgramBinaryOES: TglGetProgramBinaryOES;
    glGetProgramInfoLog: TglGetProgramInfoLog;
    glGetProgramiv: TglGetProgramiv;
    glGetRenderbufferParameteriv: TglGetRenderbufferParameteriv;
    glGetShaderInfoLog: TglGetShaderInfoLog;
    glGetShaderiv: TglGetShaderiv;
    glGetShaderPrecisionFormat: TglGetShaderPrecisionFormat;
    glGetShaderSource: TglGetShaderSource;
    glGetString: TglGetString;
    glGetTexParameterfv: TglGetTexParameterfv;
    glGetTexParameteriv: TglGetTexParameteriv;
    glGetUniformfv: TglGetUniformfv;
    glGetUniformiv: TglGetUniformiv;
    glGetUniformLocation: TglGetUniformLocation;
    glGetVertexAttribfv: TglGetVertexAttribfv;
    glGetVertexAttribiv: TglGetVertexAttribiv;
    glGetVertexAttribPointerv: TglGetVertexAttribPointerv;
    glHint: TglHint;
    glIsBuffer: TglIsBuffer;
    glIsEnabled: TglIsEnabled;
    glIsFramebuffer: TglIsFramebuffer;
    glIsProgram: TglIsProgram;
    glIsRenderbuffer: TglIsRenderbuffer;
    glIsShader: TglIsShader;
    glIsTexture: TglIsTexture;
    glLineWidth: TglLineWidth;
    glLinkProgram: TglLinkProgram;
    glPixelStorei: TglPixelStorei;
    glPolygonOffset: TglPolygonOffset;
    glReadPixels: TglReadPixels;
    glReleaseShaderCompiler: TglReleaseShaderCompiler;
    glRenderbufferStorage: TglRenderbufferStorage;
    glSampleCoverage: TglSampleCoverage;
    glScissor: TglScissor;
    glShaderBinary: TglShaderBinary;
    glShaderSource: TglShaderSource;
    glStencilFunc: TglStencilFunc;
    glStencilFuncSeparate: TglStencilFuncSeparate;
    glStencilMask: TglStencilMask;
    glStencilMaskSeparate: TglStencilMaskSeparate;
    glStencilOp: TglStencilOp;
    glStencilOpSeparate: TglStencilOpSeparate;
    glTexImage2D: TglTexImage2D;
    glTexParameterf: TglTexParameterf;
    glTexParameterfv: TglTexParameterfv;
    glTexParameteri: TglTexParameteri;
    glTexParameteriv: TglTexParameteriv;
    glTexSubImage2D: TglTexSubImage2D;
    glUniform1f: TglUniform1f;
    glUniform1fv: TglUniform1fv;
    glUniform1i: TglUniform1i;
    glUniform1iv: TglUniform1iv;
    glUniform2f: TglUniform2f;
    glUniform2fv: TglUniform2fv;
    glUniform2i: TglUniform2i;
    glUniform2iv: TglUniform2iv;
    glUniform3f: TglUniform3f;
    glUniform3fv: TglUniform3fv;
    glUniform3i: TglUniform3i;
    glUniform3iv: TglUniform3iv;
    glUniform4f: TglUniform4f;
    glUniform4fv: TglUniform4fv;
    glUniform4i: TglUniform4i;
    glUniform4iv: TglUniform4iv;
    glUniformMatrix2fv: TglUniformMatrix2fv;
    glUniformMatrix3fv: TglUniformMatrix3fv;
    glUniformMatrix4fv: TglUniformMatrix4fv;
    glUseProgram: TglUseProgram;
    glValidateProgram: TglValidateProgram;
    glVertexAttrib1f: TglVertexAttrib1f;
    glVertexAttrib1fv: TglVertexAttrib1fv;
    glVertexAttrib2f: TglVertexAttrib2f;
    glVertexAttrib2fv: TglVertexAttrib2fv;
    glVertexAttrib3f: TglVertexAttrib3f;
    glVertexAttrib3fv: TglVertexAttrib3fv;
    glVertexAttrib4f: TglVertexAttrib4f;
    glVertexAttrib4fv: TglVertexAttrib4fv;
    glVertexAttribPointer: TglVertexAttribPointer;
    glViewport: TglViewport;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    procedure LogGLError(const Msg: String);
  end;

  { TOpenGLLibrary }

  TOpenGLLibrary = class(TOpenGLEs_2_0)
  end;

implementation

{ TOpenGLEs_2_0 }

procedure TOpenGLEs_2_0.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('glActiveTexture', @glActiveTexture);
  List.Add('glAttachShader', @glAttachShader);
  List.Add('glBindAttribLocation', @glBindAttribLocation);
  List.Add('glBindBuffer', @glBindBuffer);
  List.Add('glBindFramebuffer', @glBindFramebuffer);
  List.Add('glBindRenderbuffer', @glBindRenderbuffer);
  List.Add('glBindTexture', @glBindTexture);
  List.Add('glBlendColor', @glBlendColor);
  List.Add('glBlendEquation', @glBlendEquation);
  List.Add('glBlendEquationSeparate', @glBlendEquationSeparate);
  List.Add('glBlendFunc', @glBlendFunc);
  List.Add('glBlendFuncSeparate', @glBlendFuncSeparate);
  List.Add('glBufferData', @glBufferData);
  List.Add('glBufferSubData', @glBufferSubData);
  List.Add('glCheckFramebufferStatus', @glCheckFramebufferStatus);
  List.Add('glClear', @glClear);
  List.Add('glClearColor', @glClearColor);
  List.Add('glClearDepthf', @glClearDepthf);
  List.Add('glClearStencil', @glClearStencil);
  List.Add('glColorMask', @glColorMask);
  List.Add('glCompileShader', @glCompileShader);
  List.Add('glCompressedTexImage2D', @glCompressedTexImage2D);
  List.Add('glCompressedTexSubImage2D', @glCompressedTexSubImage2D);
  List.Add('glCopyTexImage2D', @glCopyTexImage2D);
  List.Add('glCopyTexSubImage2D', @glCopyTexSubImage2D);
  List.Add('glCreateProgram', @glCreateProgram);
  List.Add('glCreateShader', @glCreateShader);
  List.Add('glCullFace', @glCullFace);
  List.Add('glDeleteBuffers', @glDeleteBuffers);
  List.Add('glDeleteFramebuffers', @glDeleteFramebuffers);
  List.Add('glDeleteProgram', @glDeleteProgram);
  List.Add('glDeleteRenderbuffers', @glDeleteRenderbuffers);
  List.Add('glDeleteShader', @glDeleteShader);
  List.Add('glDeleteTextures', @glDeleteTextures);
  List.Add('glDepthFunc', @glDepthFunc);
  List.Add('glDepthMask', @glDepthMask);
  List.Add('glDepthRangef', @glDepthRangef);
  List.Add('glDetachShader', @glDetachShader);
  List.Add('glDisable', @glDisable);
  List.Add('glDisableVertexAttribArray', @glDisableVertexAttribArray);
  List.Add('glDrawArrays', @glDrawArrays);
  List.Add('glDrawElements', @glDrawElements);
  List.Add('glEnable', @glEnable);
  List.Add('glEnableVertexAttribArray', @glEnableVertexAttribArray);
  List.Add('glFinish', @glFinish);
  List.Add('glFlush', @glFlush);
  List.Add('glFramebufferRenderbuffer', @glFramebufferRenderbuffer);
  List.Add('glFramebufferTexture2D', @glFramebufferTexture2D);
  List.Add('glFrontFace', @glFrontFace);
  List.Add('glGenBuffers', @glGenBuffers);
  List.Add('glGenerateMipmap', @glGenerateMipmap);
  List.Add('glGenFramebuffers', @glGenFramebuffers);
  List.Add('glGenRenderbuffers', @glGenRenderbuffers);
  List.Add('glGenTextures', @glGenTextures);
  List.Add('glGetActiveAttrib', @glGetActiveAttrib);
  List.Add('glGetActiveUniform', @glGetActiveUniform);
  List.Add('glGetAttachedShaders', @glGetAttachedShaders);
  List.Add('glGetAttribLocation', @glGetAttribLocation);
  List.Add('glGetBooleanv', @glGetBooleanv);
  List.Add('glGetBufferParameteriv', @glGetBufferParameteriv);
  List.Add('glGetError', @glGetError);
  List.Add('glGetFloatv', @glGetFloatv);
  List.Add('glGetFramebufferAttachmentParameteriv', @glGetFramebufferAttachmentParameteriv);
  List.Add('glGetIntegerv', @glGetIntegerv);
  List.Add('glGetPerfMonitorCounterDataAMD', @glGetPerfMonitorCounterDataAMD);
  List.Add('glGetPerfMonitorCounterInfoAMD', @glGetPerfMonitorCounterInfoAMD);
  List.Add('glGetPerfMonitorCountersAMD', @glGetPerfMonitorCountersAMD);
  List.Add('glGetPerfMonitorCounterStringAMD', @glGetPerfMonitorCounterStringAMD);
  List.Add('glGetPerfMonitorGroupsAMD', @glGetPerfMonitorGroupsAMD);
  List.Add('glGetPerfMonitorGroupStringAMD', @glGetPerfMonitorGroupStringAMD);
  List.Add('glGetProgramBinaryOES', @glGetProgramBinaryOES);
  List.Add('glGetProgramInfoLog', @glGetProgramInfoLog);
  List.Add('glGetProgramiv', @glGetProgramiv);
  List.Add('glGetRenderbufferParameteriv', @glGetRenderbufferParameteriv);
  List.Add('glGetShaderInfoLog', @glGetShaderInfoLog);
  List.Add('glGetShaderiv', @glGetShaderiv);
  List.Add('glGetShaderPrecisionFormat', @glGetShaderPrecisionFormat);
  List.Add('glGetShaderSource', @glGetShaderSource);
  List.Add('glGetString', @glGetString);
  List.Add('glGetTexParameterfv', @glGetTexParameterfv);
  List.Add('glGetTexParameteriv', @glGetTexParameteriv);
  List.Add('glGetUniformfv', @glGetUniformfv);
  List.Add('glGetUniformiv', @glGetUniformiv);
  List.Add('glGetUniformLocation', @glGetUniformLocation);
  List.Add('glGetVertexAttribfv', @glGetVertexAttribfv);
  List.Add('glGetVertexAttribiv', @glGetVertexAttribiv);
  List.Add('glGetVertexAttribPointerv', @glGetVertexAttribPointerv);
  List.Add('glHint', @glHint);
  List.Add('glIsBuffer', @glIsBuffer);
  List.Add('glIsEnabled', @glIsEnabled);
  List.Add('glIsFramebuffer', @glIsFramebuffer);
  List.Add('glIsProgram', @glIsProgram);
  List.Add('glIsRenderbuffer', @glIsRenderbuffer);
  List.Add('glIsShader', @glIsShader);
  List.Add('glIsTexture', @glIsTexture);
  List.Add('glLineWidth', @glLineWidth);
  List.Add('glLinkProgram', @glLinkProgram);
  List.Add('glPixelStorei', @glPixelStorei);
  List.Add('glPolygonOffset', @glPolygonOffset);
  List.Add('glReadPixels', @glReadPixels);
  List.Add('glReleaseShaderCompiler', @glReleaseShaderCompiler);
  List.Add('glRenderbufferStorage', @glRenderbufferStorage);
  List.Add('glSampleCoverage', @glSampleCoverage);
  List.Add('glScissor', @glScissor);
  List.Add('glShaderBinary', @glShaderBinary);
  List.Add('glShaderSource', @glShaderSource);
  List.Add('glStencilFunc', @glStencilFunc);
  List.Add('glStencilFuncSeparate', @glStencilFuncSeparate);
  List.Add('glStencilMask', @glStencilMask);
  List.Add('glStencilMaskSeparate', @glStencilMaskSeparate);
  List.Add('glStencilOp', @glStencilOp);
  List.Add('glStencilOpSeparate', @glStencilOpSeparate);
  List.Add('glTexImage2D', @glTexImage2D);
  List.Add('glTexParameterf', @glTexParameterf);
  List.Add('glTexParameterfv', @glTexParameterfv);
  List.Add('glTexParameteri', @glTexParameteri);
  List.Add('glTexParameteriv', @glTexParameteriv);
  List.Add('glTexSubImage2D', @glTexSubImage2D);
  List.Add('glUniform1f', @glUniform1f);
  List.Add('glUniform1fv', @glUniform1fv);
  List.Add('glUniform1i', @glUniform1i);
  List.Add('glUniform1iv', @glUniform1iv);
  List.Add('glUniform2f', @glUniform2f);
  List.Add('glUniform2fv', @glUniform2fv);
  List.Add('glUniform2i', @glUniform2i);
  List.Add('glUniform2iv', @glUniform2iv);
  List.Add('glUniform3f', @glUniform3f);
  List.Add('glUniform3fv', @glUniform3fv);
  List.Add('glUniform3i', @glUniform3i);
  List.Add('glUniform3iv', @glUniform3iv);
  List.Add('glUniform4f', @glUniform4f);
  List.Add('glUniform4fv', @glUniform4fv);
  List.Add('glUniform4i', @glUniform4i);
  List.Add('glUniform4iv', @glUniform4iv);
  List.Add('glUniformMatrix2fv', @glUniformMatrix2fv);
  List.Add('glUniformMatrix3fv', @glUniformMatrix3fv);
  List.Add('glUniformMatrix4fv', @glUniformMatrix4fv);
  List.Add('glUseProgram', @glUseProgram);
  List.Add('glValidateProgram', @glValidateProgram);
  List.Add('glVertexAttrib1f', @glVertexAttrib1f);
  List.Add('glVertexAttrib1fv', @glVertexAttrib1fv);
  List.Add('glVertexAttrib2f', @glVertexAttrib2f);
  List.Add('glVertexAttrib2fv', @glVertexAttrib2fv);
  List.Add('glVertexAttrib3f', @glVertexAttrib3f);
  List.Add('glVertexAttrib3fv', @glVertexAttrib3fv);
  List.Add('glVertexAttrib4f', @glVertexAttrib4f);
  List.Add('glVertexAttrib4fv', @glVertexAttrib4fv);
  List.Add('glVertexAttribPointer', @glVertexAttribPointer);
  List.Add('glViewport', @glViewport);
end;

procedure TOpenGLEs_2_0.LogGLError(const Msg: String);
var
  GLError: GL_Error;
begin
  GLError := glGetError();

  if (GLError <> GL_NO_ERROR) and Assigned(Logger) then
    Logger.Log(lgError, Msg + '(' + IntToStr(Ord(GLError)) + ')')
end;

end.

