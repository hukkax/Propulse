unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SDL.Extended.Window, SDL.Extended.Renderer,
  SDL.Extended.Camera, SDL.Extended.WorkerControl, SDL.Api.libSDL2, Controls,
  SDL.Api.Types, SDL.Api.Video, Forms, ExtCtrls, SDL.Extended.Context,
  SDL.Extended.Interfaces, SDL.Extended.OpenGLExtensions, SDL.Api.OpenGL.Types,
  SDl.Extended.Types;

type

  { TSDLWindow1 }

  TSDLWindow1 = class(TSDLWindow)
    SDLCamera1: TSDLCamera;
    SDLOpenGLExtensions1: TSDLOpenGLExtensions;
    SDLRenderer1: TSDLRenderer;
    SDLRenderer2: TSDLRenderer;
    SDLWorkerControl1: TSDLWorkerControl;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleInitOpenGL(const Sender: TObject);
    procedure DataModulePaint(const Sender: TObject);
    procedure DataModuleProcess(Sender: TObject);
    procedure SDLOpenGLExtensions1NotLoaded(const Sender: TObject; const Extension: String; Option: TSDLExtensionOption);
    procedure SDLRenderer1Render(const Renderer: IRenderer);
    procedure SDLRenderer2Render(const Renderer: IRenderer);
    procedure SDLWorkerControl1WorkFinish(const Sender: TObject; const JobName: String; const Worker: IWorker; const Data: Pointer);
  private
    FCurFPS, FFixedFPS: Integer;
    FNextFPS: QWord;
    FCurAPS, FFixedAPS: Integer;
    FNextAPS: QWord;
    GLTextures: array [0..5] of PSDLTextureContext;
  public
  end;

implementation

uses
  SDL.Extended.Application, typinfo, Graphics, SDL.Extended.Loaders, SDL.Extended.Utils,
  LCLProc;

{$R *.lfm}

{ TSDLWindow1 }

procedure TSDLWindow1.DataModuleCreate(Sender: TObject);
begin
  FNextFPS := GetTickCount64;
  FNextAPS := GetTickCount64;
end;

procedure TSDLWindow1.DataModuleDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(GLTextures) to High(GLTextures) do
    Dispose(GLTextures[i]);
end;

procedure TSDLWindow1.DataModuleInitOpenGL(const Sender: TObject);
begin
  OpenGL.glViewport(0, 0, Round(Size.Width), Round(Size.Height));
  OpenGL.glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  SDLWorkerControl1.AddTextureLoading(
    'LoadPNG',
    TFileStream.Create('../../sdl_image/bin/data/sdl.png', fmOpenRead)
  );

  SDLWorkerControl1.AddTextureLoading(
    'LoadJPG',
    TFileStream.Create('../../sdl_image/bin/data/sdl.jpg', fmOpenRead)
  );

  SDLWorkerControl1.AddTextureLoading(
    'LoadXCF',
    TFileStream.Create('../../sdl_image/bin/data/sdl.xcf', fmOpenRead)
  );

  SDLWorkerControl1.AddTextureLoading(
    'LoadGIF',
    TFileStream.Create('../../sdl_image/bin/data/sdl.gif', fmOpenRead)
  );

  SDLWorkerControl1.AddTextureLoading(
    'LoadBMP',
    TFileStream.Create('../../sdl_image/bin/data/sdl.bmp', fmOpenRead)
  );

  SDLWorkerControl1.AddTextureLoading(
    'LoadTIF',
    TFileStream.Create('../../sdl_image/bin/data/sdl.tif', fmOpenRead)
  );

  SDLWorkerControl1.Active := True;
end;

procedure TSDLWindow1.DataModulePaint(const Sender: TObject);
begin
  Inc(FCurFPS);
  if FNextFPS <= GetTickCount64 then
  begin
    FFixedFPS := FCurFPS;
    SDL2.Video.SDL_SetWindowTitle(Wnd, SDL_String('FPS: ' + IntToStr(FFixedFPS) + ' APS: ' + IntToStr(FFixedAPS)));
    FNextFPS := GetTickCount64 + 1000;
    FCurFPS := 0;
  end;
end;

procedure TSDLWindow1.DataModuleProcess(Sender: TObject);
begin
  Inc(FCurAPS);
  if FNextAPS <= GetTickCount64 then
  begin
    FFixedAPS := FCurAPS;
    SDL2.Video.SDL_SetWindowTitle(Wnd, SDL_String('FPS: ' + IntToStr(FFixedFPS) + ' APS: ' + IntToStr(FFixedAPS)));
    FNextAPS := GetTickCount64 + 1000;
    FCurAPS := 0;
  end;
end;

procedure TSDLWindow1.SDLOpenGLExtensions1NotLoaded(const Sender: TObject;
  const Extension: String; Option: TSDLExtensionOption);
begin
  if Option = extRequired then
  begin
    raise Exception.CreateFmt('Required extension "%s" not supported!', [Extension]);
    Application.Terminate;
  end
  else
  if Option = extOptional then
    DebugLn(Format('Extension "%s" not loaded!', [Extension]));
end;

procedure TSDLWindow1.SDLRenderer1Render(const Renderer: IRenderer);
var
  i: Integer;
  Vertex1: TOpenGLVertex2f;
  Vertex2: array [0..7] of GLfloat;
  Cords: TOpenGLVertex2f;
  Textures: array [0..0] of PSDLTextureContext;
  Matrix: TOpenGLMatrix;
begin
  Renderer.ApplyCamera(@Matrix);
  Application.Lock;
  try
  for i := Low(GLTextures) to High(GLTextures) do
  begin
      if Assigned(GLTextures[i]) then
      begin
        Renderer.MakeVertex2f(-4.5 + i, -0.5, 1, 1, Vertex1);
        Renderer.MakeVertex2f(0, 0, 1, 1, Cords);

        Vertex2[0] := -4.5 + i;
        Vertex2[1] := 0.6;
        Vertex2[2] := -4.5 + i;
        Vertex2[3] := 1.6;
        Vertex2[4] := -3.5 + i;
        Vertex2[5] := 1.6;
        Vertex2[6] := -3.5 + i;
        Vertex2[7] := 0.6;

        Textures[0] := GLTextures[i];

        Renderer.DrawTexture2f(Vertex1, Cords, Textures[0]^.ID, @Matrix);
        //Renderer.DrawTexture(Vertex2, Cords, GLTextures[i])
      end
    end
  finally
    Application.Unlock;
  end
end;

procedure TSDLWindow1.SDLRenderer2Render(const Renderer: IRenderer);
var
  Vertex1: TOpenGLVertex2f;
  Vertex2: TOpenGLVertex2f;
  Matrix: TOpenGLMatrix;
begin
  Renderer.ApplyCamera(@Matrix);
  SetLength(Vertex1, 4);
  Vertex1[0] := Vertex2f.Make(2.0, 2.0);
  Vertex1[1] := Vertex2f.Make(2.0, 3.0);
  Vertex1[2] := Vertex2f.Make(3.0, 3.0);
  Vertex1[3] := Vertex2f.Make(3.0, 2.0);

  SetLength(Vertex2, 4);
  Vertex2[0] := Vertex2f.Make(0.0, 2.0);
  Vertex2[1] := Vertex2f.Make(0.0, 3.0);
  Vertex2[2] := Vertex2f.Make(1.0, 3.0);
  Vertex2[3] := Vertex2f.Make(1.0, 2.0);

  Renderer.DrawRectangle2f(Vertex1, GL_Color.Make(128, 0, 255, 255), @Matrix);
  Renderer.DrawRectangle2f(Vertex2, GL_Color.Make(128, 0, 255, 255), @Matrix);
end;

procedure TSDLWindow1.SDLWorkerControl1WorkFinish(const Sender: TObject;
  const JobName: String; const Worker: IWorker; const Data: Pointer);
var
  TextureData: PSDLTextureData absolute Data;
begin
  Application.Lock;
  try
    if JobName = 'LoadPNG' then
      GLTextures[0] := TextureData^.ResultTexture
    else
    if JobName = 'LoadJPG' then
      GLTextures[1] := TextureData^.ResultTexture
    else
    if JobName = 'LoadXCF' then
      GLTextures[2] := TextureData^.ResultTexture
    else
    if JobName = 'LoadGIF' then
      GLTextures[3] := TextureData^.ResultTexture
    else
    if JobName = 'LoadBMP' then
      GLTextures[4] := TextureData^.ResultTexture
    else
    if JobName = 'LoadTIF' then
      GLTextures[5] := TextureData^.ResultTexture;
  finally
    Application.Unlock;
  end;
end;

initialization
  Application.RegisterWindow(TSDLWindow1, True);
end.

