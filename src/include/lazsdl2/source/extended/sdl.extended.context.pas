unit SDL.Extended.Context;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.OpenGL.Types;

type

  PSDLTextureContext = ^TSDLTextureContext;
  POpenGLShaderContext = ^TOpenGLShaderContext;

  { TSDLTextureContext }

  TSDLTextureContext = record
  public
    Width: GLint;
    Height: GLint;
    CompressedSize: GLint;
    ID: GL_Texture;
  end;

  TOpenGLShaderContext = record
    ShaderProgram: GL_Program;
    VertexShader: GL_Shader;
    FragmentShader: GL_Shader;
  end;

implementation

end.

