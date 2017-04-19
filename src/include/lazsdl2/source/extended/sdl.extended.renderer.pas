unit SDL.Extended.Renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Extended.LibObject, SDL.Extended.LibraryProvider,
  SDL.Api.Video, SDL.Extended.Camera, SDL.Extended.Context, SDL.Api.Types,
  SDL.Api.OpenGL.Types, SDL.Api.OpenGL, SDL.Api.Consts, SDL.Extended.Types;

type

  { TSDLCustomRenderer }

  TSDLCustomRenderer = class(TSDLWindowedObject, IRenderer)
  private type
    TGLShaderData = record
      Vertex: String;
      Fragment: String;
    end;

  private const
    cLineShaderCode: TGLShaderData = (
      Vertex:
        'uniform mat4 uMVPMatrix;' +
        'attribute vec4 vPosition;' +
        'void main()' +
        '{' +
        '  gl_Position = uMVPMatrix * vPosition;' +
        '}';
      Fragment:
        {$IFDEF ANDROID}
        'precision mediump float;' +
        {$ENDIF}
        'uniform vec4 vColor;' +
        'void main()' +
        '{' +
        '  gl_FragColor = vColor;' +
        '}';
    );

    cColorsShaderCode: TGLShaderData = (
      Vertex:
        'uniform mat4 uMVPMatrix;' +
        'attribute vec4 vPosition;' +
        'attribute vec4 vColors;' +
        'varying vec4 vColor;' +
        'void main()' +
        '{' +
        '  gl_Position = uMVPMatrix * vPosition;' +
        '  vColor = vColors;'+
        '}';
      Fragment:
        {$IFDEF ANDROID}
        'precision mediump float;' +
        {$ENDIF}
        'varying vec4 vColor;' +
        'void main()' +
        '{' +
        '  gl_FragColor = vColor;' +
        '}';
    );

    cTextureShaderCode: TGLShaderData = (
      Vertex:
        'uniform mat4 uMVPMatrix;' +
        'attribute vec4 vPosition;' +
        'attribute vec2 a_texCoord;' +
        'varying vec2 v_texCoord;' +
        'void main()' +
        '{' +
        '  gl_Position = uMVPMatrix * vPosition;' +
        '  v_texCoord = a_texCoord;'+
        '}';
      Fragment:
        {$IFDEF ANDROID}
        'precision mediump float;' +
        {$ENDIF}
        'varying vec2 v_texCoord;' +
        'uniform sampler2D s_texture;' +
        'void main()' +
        '{' +
        '  gl_FragColor = texture2D( s_texture,v_texCoord );' +
        '}';
    );
  private
    FOnInit: TNotifyEvent;
    FShapeShader: record
      Shader: TOpenGLShaderContext;
      PositionHandle: GLuint;
      ColorHandle: GLuint;
      MVPMatrixHandle: GLuint;
    end;
    FColorsShader: record
      Shader: TOpenGLShaderContext;
      PositionHandle: GLuint;
      ColorHandle: GLuint;
      MVPMatrixHandle: GLuint;
    end;
    FTextureShader: record
      Shader: TOpenGLShaderContext;
      PositionHandle: GLuint;
      TextureCordHandle: GLuint;
      MVPMatrixHandle: GLuint;
      SamplerHandle: GLuint;
    end;
    FActive: Boolean;
    FCamera: TSDLCustomCamera;
    FOnRender: TSDLOnRenderEvent;
    FVideo: TSDLVideoApi;
    FLibOGL: TOpenGLLibrary;
    procedure SetActive(AValue: Boolean);
    procedure SetCamera(AValue: TSDLCustomCamera);
    procedure SetOnInit(AValue: TNotifyEvent);
    procedure SetOnRender(AValue: TSDLOnRenderEvent);
    procedure InternalSetOnTransparentColorOptions; inline;
    procedure InternalSetOffTransparentColorOptions; inline;
  protected
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer); override;
    procedure DoRender; virtual;
    procedure DoReload; override;
    procedure DoInit;
  public
    property Active: Boolean read FActive write SetActive; (** Enables or disables rendering *)
    property Camera: TSDLCustomCamera read FCamera write SetCamera; (** Information abbout position in the world *)

    function LoadShaderProgramm(VertexSource: TStream; FragmentSource: TStream; out ShaderProgramm: TOpenGLShaderContext): Boolean;
    function LoadShader(ShaderSource: TStream; ShaderType: GL_ShaderType; out Shader: GL_Shader): Boolean;
    function DrawTextures(const Vertex: TSDLVertex; Count: Integer; const Cords: TSDLVertex; const Textures: TSDLTextures): Boolean;
    function DrawTexture2f(const Vertex, TextCoords: TOpenGLVertex2f; const Texture: GL_Texture; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentTexture2f(const Vertex, TextCoords: TOpenGLVertex2f; const Texture: GL_Texture; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
    function DrawTransparentArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;

    procedure MakeVertex3f(X, Y, W, H: GLfloat; out Vertex: TOpenGLVertex3f);
    procedure MakeVertex2f(X, Y, W, H: GLfloat; out Vertex: TOpenGLVertex2f);
    procedure ApplyCamera(const Matrix: POpenGLMatrix);

    property OnRender: TSDLOnRenderEvent read FOnRender write SetOnRender;
    property OnInit: TNotifyEvent read FOnInit write SetOnInit;
  end;

  TSDLRenderer = class(TSDLCustomRenderer)
  published
    property Active;
    property Camera;
    property Provider;
    property Window; (** Window on witch perform rendering *)

    property OnRender;
  end;

implementation

uses
  LMessages, SDL.Extended.Application, math;

{ TSDLCustomRenderer }

procedure TSDLCustomRenderer.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  inherited FPOObservedChanged(ASender, Operation, Data);

  if (ASender = Window) and (Operation = ooCustom) and ({%H-}PtrUInt(Data) = LM_PAINT) then
  begin
    if FActive and Assigned(Provider) and Provider.Active then
      DoRender
  end
  else
  if (ASender = Window) and (Operation = ooChange) and ({%H-}PtrUInt(Data) = LM_CREATE) then
  begin
    if FActive and Assigned(Provider) and Provider.Active then
      DoInit
  end
end;

procedure TSDLCustomRenderer.DoRender;
begin
  if Assigned(FOnRender) then
    FOnRender(Self)
end;

procedure TSDLCustomRenderer.DoReload;
begin
  if Assigned(Provider) and Provider.Active then
  begin
    FVideo := TSDLVideoApi.Create(Provider.Libs[LIB_SDL]);
    FVideo.Init;

    if not FVideo.Valid then
      RaiseSDLException;

    FLibOGL := Provider.Libs[LIB_OPENGL] as TOpenGLLibrary;
    if not Assigned(FLibOGL) then
      RaiseOpenGLException;
  end
  else
  begin
    FLibOGL := nil;
    FreeAndNil(FVideo)
  end
end;

procedure TSDLCustomRenderer.DoInit;
var
  VertSrc, FragSrc: TStringStream;
begin
  if FShapeShader.Shader.ShaderProgram = 0 then
  begin
    VertSrc := TStringStream.Create(cLineShaderCode.Vertex);
    FragSrc := TStringStream.Create(cLineShaderCode.Fragment);
    try
      LoadShaderProgramm(VertSrc, FragSrc, FShapeShader.Shader);

      FLibOGL.glUseProgram(FShapeShader.Shader.ShaderProgram);
      FShapeShader.PositionHandle := FLibOGL.glGetAttribLocation(FShapeShader.Shader.ShaderProgram, 'vPosition');
      FShapeShader.ColorHandle := FLibOGL.glGetUniformLocation(FShapeShader.Shader.ShaderProgram, 'vColor');
      FShapeShader.MVPMatrixHandle := FLibOGL.glGetUniformLocation(FShapeShader.Shader.ShaderProgram, 'uMVPMatrix');
      FLibOGL.glUseProgram(0);
    finally
      VertSrc.Free;
      FragSrc.Free;
    end;
  end;

  if FColorsShader.Shader.ShaderProgram = 0 then
  begin
    VertSrc := TStringStream.Create(cColorsShaderCode.Vertex);
    FragSrc := TStringStream.Create(cColorsShaderCode.Fragment);
    try
      LoadShaderProgramm(VertSrc, FragSrc, FColorsShader.Shader);

      FLibOGL.glUseProgram(FColorsShader.Shader.ShaderProgram);
      FColorsShader.PositionHandle := FLibOGL.glGetAttribLocation(FColorsShader.Shader.ShaderProgram, 'vPosition');
      FColorsShader.ColorHandle := FLibOGL.glGetAttribLocation(FColorsShader.Shader.ShaderProgram, 'vColors');
      FColorsShader.MVPMatrixHandle := FLibOGL.glGetUniformLocation(FColorsShader.Shader.ShaderProgram, 'uMVPMatrix');
      FLibOGL.glUseProgram(0);
    finally
      VertSrc.Free;
      FragSrc.Free;
    end;
  end;

  if FTextureShader.Shader.ShaderProgram = 0 then
  begin
    VertSrc := TStringStream.Create(cTextureShaderCode.Vertex);
    FragSrc := TStringStream.Create(cTextureShaderCode.Fragment);
    try
      LoadShaderProgramm(VertSrc, FragSrc, FTextureShader.Shader);

      FLibOGL.glUseProgram(FTextureShader.Shader.ShaderProgram);
      FTextureShader.PositionHandle := FLibOGL.glGetAttribLocation(FTextureShader.Shader.ShaderProgram, 'vPosition');
      FTextureShader.TextureCordHandle := FLibOGL.glGetAttribLocation(FTextureShader.Shader.ShaderProgram, 'a_texCoord');
      FTextureShader.MVPMatrixHandle := FLibOGL.glGetUniformLocation(FTextureShader.Shader.ShaderProgram, 'uMVPMatrix');
      FTextureShader.SamplerHandle := FLibOGL.glGetUniformLocation(FTextureShader.Shader.ShaderProgram, 's_texture');
      FLibOGL.glUseProgram(0);
    finally
      VertSrc.Free;
      FragSrc.Free;
    end;
  end;

  if Assigned(FOnInit) then
    FOnInit(Self);
end;

function TSDLCustomRenderer.LoadShaderProgramm(VertexSource: TStream;
  FragmentSource: TStream; out ShaderProgramm: TOpenGLShaderContext): Boolean;
var
  Linked: GLint;
begin
  ShaderProgramm.ShaderProgram := FLibOGL.glCreateProgram();

  if ShaderProgramm.ShaderProgram = 0 then
    Exit(False);

  if not LoadShader(VertexSource, GL_VERTEX_SHADER, ShaderProgramm.VertexShader) then
    Exit(False);
  FLibOGL.glAttachShader(ShaderProgramm.ShaderProgram, ShaderProgramm.VertexShader);

  if not LoadShader(FragmentSource, GL_FRAGMENT_SHADER, ShaderProgramm.FragmentShader) then
    Exit(False);
  FLibOGL.glAttachShader(ShaderProgramm.ShaderProgram, ShaderProgramm.FragmentShader);

  FLibOGL.glLinkProgram(ShaderProgramm.ShaderProgram);

  FLibOGL.glGetProgramiv(ShaderProgramm.ShaderProgram, GL_LINK_STATUS, @Linked);
  if Linked = Ord(GL_FALSE) then
  begin
    Application.Log(etError, 'Linking failed');
    Exit(False)
  end;

  Result := True;
end;

function TSDLCustomRenderer.LoadShader(ShaderSource: TStream;
  ShaderType: GL_ShaderType; out Shader: GL_Shader): Boolean;
var
  Compiled: GLint;
  Source: TStringStream;
  SourceSize, Length: GLint;
  GLSource: GLchar;
  ErrorLog: array [0..1023] of AnsiChar;
begin
  Shader := FLibOGL.glCreateShader(ShaderType);

  if Shader = 0 then
    Exit(False);

  Source := TStringStream.Create('');
  try
    Source.CopyFrom(ShaderSource, 0);
    Source.Position := 0;
    SourceSize := Source.Size;
    GLSource := GLchar(Source.DataString);
    FLibOGL.glShaderSource(Shader, 1, @GLSource, @SourceSize);
  finally
    Source.Free;
  end;

  FLibOGL.glCompileShader(Shader);
  FLibOGL.glGetShaderiv(Shader, GL_COMPILE_STATUS, @Compiled);

  if Compiled = Ord(GL_FALSE) then
  begin
    FLibOGL.glGetShaderiv(Shader, GL_INFO_SHADER_LOG_LENGTH, @Length);
    FLibOGL.glGetShaderInfoLog(Shader, SizeOf(ErrorLog), @Length, @ErrorLog[0]);
    Application.Log(etError, ErrorLog);
    Exit(False)
  end;

  Result := True;
end;

function TSDLCustomRenderer.DrawTextures(const Vertex: TSDLVertex;
  Count: Integer; const Cords: TSDLVertex; const Textures: TSDLTextures): Boolean;
begin
end;

function TSDLCustomRenderer.DrawTexture2f(const Vertex, TextCoords: TOpenGLVertex2f; const Texture: GL_Texture; Matrix: POpenGLMatrix): Boolean;
const
  indices: array [0..5] of Byte = (0,1,2,0,2,3);
begin
  with FTextureShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(FTextureShader.PositionHandle);
    FLibOGL.glEnableVertexAttribArray(FTextureShader.TextureCordHandle);
    try
      FLibOGL.glActiveTexture(GL_TEXTURE0);
      FLibOGL.glBindTexture(GL_TEXTURE_2D, Texture);

      FLibOGL.glVertexAttribPointer(FTextureShader.PositionHandle, 2, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex2f), @Vertex[0]);
      FLibOGL.glVertexAttribPointer(FTextureShader.TextureCordHandle, 2, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex2f), @TextCoords[0]);
      FLibOGL.glUniformMatrix4fv(FTextureShader.MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glUniform1i(FTextureShader.SamplerHandle, 0);
      FLibOGL.glDrawElements(GL_TRIANGLES, Length(indices), GL_UNSIGNED_BYTE_IND, @indices[0]);
    finally
      FLibOGL.glDisableVertexAttribArray(FTextureShader.PositionHandle);
      FLibOGL.glDisableVertexAttribArray(FTextureShader.TextureCordHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True;
end;

function TSDLCustomRenderer.DrawTransparentTexture2f(const Vertex, TextCoords: TOpenGLVertex2f; const Texture: GL_Texture; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawTexture2f(Vertex, TextCoords, Texture, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawRectangle2f(const Vertex: TOpenGLVertex2f;
  const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
const
  indices: array [0..5] of Byte = (0,1,2,0,2,3);
begin
  with FShapeShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(FShapeShader.PositionHandle);
    try
      FLibOGL.glVertexAttribPointer(FShapeShader.PositionHandle, 2, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex2f), @Vertex[0]);
      FLibOGL.glUniform4fv(FShapeShader.ColorHandle, 1, @Color);
      FLibOGL.glUniformMatrix4fv(FShapeShader.MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawElements(GL_TRIANGLES, Length(indices), GL_UNSIGNED_BYTE_IND, @indices[0]);
    finally
      FLibOGL.glDisableVertexAttribArray(FShapeShader.PositionHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True;
end;

function TSDLCustomRenderer.DrawRectangle2f(const Vertex: TOpenGLVertex2f;
  const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
const
  indices: array [0..5] of Byte = (0,1,2,0,2,3);
begin
  with FColorsShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(PositionHandle);
    FLibOGL.glEnableVertexAttribArray(ColorHandle);
    try
      FLibOGL.glVertexAttribPointer(PositionHandle, 2, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex2f), @Vertex[0]);
      FLibOGL.glVertexAttribPointer(ColorHandle, 4, GL_FLOAT_DATA, Ord(false), SizeOf(GL_Color), @Color[0]);
      FLibOGL.glUniformMatrix4fv(MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawElements(GL_TRIANGLES, Length(indices), GL_UNSIGNED_BYTE_IND, @indices[0]);
    finally
      FLibOGL.glDisableVertexAttribArray(PositionHandle);
      FLibOGL.glDisableVertexAttribArray(ColorHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True;
end;

function TSDLCustomRenderer.DrawRectangle3f(const Vertex: TOpenGLVertex3f;
  const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
const
  indices: array [0..5] of Byte = (0,1,2,0,2,3);
begin
  with FShapeShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(FShapeShader.PositionHandle);
    try
      FLibOGL.glVertexAttribPointer(FShapeShader.PositionHandle, 3, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex3f), @Vertex[0]);
      FLibOGL.glUniform4fv(FShapeShader.ColorHandle, 1, @Color);
      FLibOGL.glUniformMatrix4fv(FShapeShader.MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawElements(GL_TRIANGLES, Length(indices), GL_UNSIGNED_BYTE_IND, @indices[0]);
    finally
      FLibOGL.glDisableVertexAttribArray(FShapeShader.PositionHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True;
end;

function TSDLCustomRenderer.DrawRectangle3f(const Vertex: TOpenGLVertex3f;
  const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
const
  indices: array [0..5] of Byte = (0,1,2,0,2,3);
begin
  with FColorsShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(PositionHandle);
    FLibOGL.glEnableVertexAttribArray(ColorHandle);
    try
      FLibOGL.glVertexAttribPointer(PositionHandle, 3, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex3f), @Vertex[0]);
      FLibOGL.glVertexAttribPointer(ColorHandle, 4, GL_FLOAT_DATA, Ord(false), SizeOf(GL_Color), @Color[0]);
      FLibOGL.glUniformMatrix4fv(MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawElements(GL_TRIANGLES, Length(indices), GL_UNSIGNED_BYTE_IND, @indices[0]);
    finally
      FLibOGL.glDisableVertexAttribArray(PositionHandle);
      FLibOGL.glDisableVertexAttribArray(ColorHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True;
end;

function TSDLCustomRenderer.DrawTransparentRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawRectangle2f(Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawTransparentRectangle2f(const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawRectangle2f(Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawTransparentRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawRectangle3f(Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawTransparentRectangle3f(const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawRectangle3f(Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
begin
  with FShapeShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(PositionHandle);
    try
      FLibOGL.glVertexAttribPointer(PositionHandle, 2, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex2f), @Vertex[0]);
      FLibOGL.glUniform4fv(ColorHandle, 1, @Color);
      FLibOGL.glUniformMatrix4fv(MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawArrays(mode, 0, Length(Vertex));
    finally
      FLibOGL.glDisableVertexAttribArray(PositionHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True
end;

function TSDLCustomRenderer.DrawArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
begin
  with FColorsShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(PositionHandle);
    FLibOGL.glEnableVertexAttribArray(ColorHandle);
    try
      FLibOGL.glVertexAttribPointer(PositionHandle, 2, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex2f), @Vertex[0]);
      FLibOGL.glVertexAttribPointer(ColorHandle, 4, GL_FLOAT_DATA, Ord(false), SizeOf(GL_Color), @Color[0]);
      FLibOGL.glUniformMatrix4fv(MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawArrays(mode, 0, Length(Vertex));
    finally
      FLibOGL.glDisableVertexAttribArray(PositionHandle);
      FLibOGL.glDisableVertexAttribArray(ColorHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True
end;

function TSDLCustomRenderer.DrawArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
begin
  with FShapeShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(PositionHandle);
    try
      FLibOGL.glVertexAttribPointer(PositionHandle, 3, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex3f), @Vertex[0]);
      FLibOGL.glUniform4fv(ColorHandle, 1, @Color);
      FLibOGL.glUniformMatrix4fv(MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawArrays(mode, 0, Length(Vertex));
    finally
      FLibOGL.glDisableVertexAttribArray(PositionHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True
end;

function TSDLCustomRenderer.DrawArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
begin
  with FColorsShader do
  begin
    FLibOGL.glUseProgram(Shader.ShaderProgram);
    FLibOGL.glEnableVertexAttribArray(PositionHandle);
    FLibOGL.glEnableVertexAttribArray(ColorHandle);
    try
      FLibOGL.glVertexAttribPointer(PositionHandle, 3, GL_FLOAT_DATA, Ord(false), SizeOf(Vertex3f), @Vertex[0]);
      FLibOGL.glVertexAttribPointer(ColorHandle, 4, GL_FLOAT_DATA, Ord(false), SizeOf(GL_Color), @Color[0]);
      FLibOGL.glUniformMatrix4fv(MVPMatrixHandle, 1, Ord(GL_FALSE), Matrix);
      FLibOGL.glDrawArrays(mode, 0, Length(Vertex));
    finally
      FLibOGL.glDisableVertexAttribArray(PositionHandle);
      FLibOGL.glDisableVertexAttribArray(ColorHandle);
      FLibOGL.glUseProgram(0);
    end
  end;

  Result := True
end;

function TSDLCustomRenderer.DrawTransparentArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawArrays3f(mode, Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawTransparentArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Color; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawArrays2f(mode, Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawTransparentArrays3f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex3f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawArrays3f(mode, Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

function TSDLCustomRenderer.DrawTransparentArrays2f(const mode: GL_DrawMode; const Vertex: TOpenGLVertex2f; const Color: GL_Colors; Matrix: POpenGLMatrix): Boolean;
begin
  InternalSetOnTransparentColorOptions;
  Result := DrawArrays2f(mode, Vertex, Color, Matrix);
  InternalSetOffTransparentColorOptions
end;

procedure TSDLCustomRenderer.MakeVertex3f(X, Y, W, H: GLfloat; out
  Vertex: TOpenGLVertex3f);
begin
  SetLength(Vertex, 4);
  Vertex3f.Make(Vertex[0], X, Y);
  Vertex3f.Make(Vertex[1], X + W, Y);
  Vertex3f.Make(Vertex[2], X + W, Y + H);
  Vertex3f.Make(Vertex[3], X, Y + H);
end;

procedure TSDLCustomRenderer.MakeVertex2f(X, Y, W, H: GLfloat; out
  Vertex: TOpenGLVertex2f);
begin
  SetLength(Vertex, 4);
  Vertex2f.Make(Vertex[0], X, Y);
  Vertex2f.Make(Vertex[1], X + W, Y);
  Vertex2f.Make(Vertex[2], X + W, Y + H);
  Vertex2f.Make(Vertex[3], X, Y + H);
end;

procedure TSDLCustomRenderer.ApplyCamera(const Matrix: POpenGLMatrix);
type
  PSDLCameraData = ^TSDLCameraData;
  TSDLCameraData = record
    eyeX: Double;
    eyeY: Double;
    eyeZ: Double;
    centerX: Double;
    centerY: Double;
    centerZ: Double;
    upX: Double;
    upY: Double;
    upZ: Double;
  end;
var
  CameraData: PSDLCameraData;
  Asspect: GLfloat;
begin
  if Assigned(Camera) and Camera.Active then
  begin
    CameraData := Camera.Data;

    Matrix^.glLoadIdentity;
    Asspect := Window.Size.Height / Window.Size.Width;
    Matrix^.glFrustum(-1, 1, -Asspect, Asspect, 2, Max(1024, abs(CameraData^.eyeZ) + 1));
    //Matrix^.gluPerspective(45, Window.Size.Width / Window.Size.Height, 2, 1024);
    Matrix^.gluLookAt(
      CameraData^.eyeX,
      CameraData^.eyeY,
      CameraData^.eyeZ,
      CameraData^.centerX,
      CameraData^.centerY,
      CameraData^.centerZ,
      CameraData^.upX,
      CameraData^.upY,
      CameraData^.upZ
    )
  end;
end;

procedure TSDLCustomRenderer.SetActive(AValue: Boolean);
begin
  if FActive=AValue then
    Exit;

  FActive:=AValue;
end;

procedure TSDLCustomRenderer.SetCamera(AValue: TSDLCustomCamera);
begin
  if FCamera=AValue then Exit;
  FCamera:=AValue;
end;

procedure TSDLCustomRenderer.SetOnInit(AValue: TNotifyEvent);
begin
  FOnInit:=AValue;
end;

procedure TSDLCustomRenderer.SetOnRender(AValue: TSDLOnRenderEvent);
begin
  FOnRender:=AValue;
end;

procedure TSDLCustomRenderer.InternalSetOnTransparentColorOptions;
begin
  FLibOGL.glEnable(GL_BLEND);
  //FLibOGL.glEnable(GL_ALPHA_TEST)
end;

procedure TSDLCustomRenderer.InternalSetOffTransparentColorOptions;
begin
  FLibOGL.glDisable(GL_BLEND);
  //FLibOGL.glDisable(GL_ALPHA_TEST)
end;

end.

