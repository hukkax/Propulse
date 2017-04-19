unit SDL.Extended.Loaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Extended.LibraryProvider, SDL.Extended.Interfaces, SDL.Extended.Context,
  SDL.Api.Types, SDL.Extended.Window, SDL.Api.OpenGL.Types;

type

  TSDLTextureLoadedvent = procedure(const Sender: TObject; const Texture: PSDLTextureContext) of object;

  PSDLTextureData = ^TSDLTextureData;
  TSDLTextureData = record
    InputStream: TStream;
    FreeStream: Boolean;
    ResultTexture: PSDLTextureContext;
  end;

  { TSDLTextureWorker }

  TSDLTextureWorker = class(TInterfacedObject, IWorker)
  private
    FOnTextureLoaded: TSDLTextureLoadedvent;
    procedure SetOnTextureLoaded(AValue: TSDLTextureLoadedvent);
    procedure InternalLoadImage(const Data: PSDLTextureData; const Provider: TSDLCustomLibraryProvider; out Image: PSDL_Surface);
    procedure InternalLoadTexture(const Image: PSDL_Surface; const Provider: TSDLCustomLibraryProvider; const GLTexture: PSDLTextureContext);
  public
    function Execute(const Control: IControl; var Data: Pointer): Boolean;

    function CreateData(const Stream: TStream; FreeStream: Boolean): Pointer;
    procedure AddRef;
    procedure Release;

    property OnTextureLoaded: TSDLTextureLoadedvent read FOnTextureLoaded write SetOnTextureLoaded;
  end;

  { TSDLTextureLoader }

  TSDLTextureLoader = class(TInterfacedPersistent, IControl)
  private
    FTexture: PSDLTextureContext;
    FWindow: TSDLWindow;
    FWorker: TSDLTextureWorker;
    procedure ExternalTextureLoaded(const Sender: TObject; const NewTexture: PSDLTextureContext);
    function GetSharedContext(out Context: SDL_GLContext): Boolean;
    function PutSharedContext(const Context: SDL_GLContext): Boolean;
    function GetSDLWindow: PSDL_Window;
    function GetLogger: ILogger;
    function GetProvider: TComponent;
  public
    constructor Create(constref AWindow: TSDLWindow); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function LoadTexture(const FileName: String; out Texture: PSDLTextureContext): Boolean;
    function LoadTexture(const Stream: TStream; out Texture: PSDLTextureContext; FreeStream: Boolean = False): Boolean;

    property Window: TSDLWindow read FWindow;
  end;

implementation

uses
  SDL.Api.libSDL_Image, SDL.Api.Video, SDL.Api.Surface, SDL.Extended.Stream, SDL.Extended.Strings,
  SDL.Api.OpenGL;

{ TSDLTextureLoader }

procedure TSDLTextureLoader.ExternalTextureLoaded(const Sender: TObject; const NewTexture: PSDLTextureContext);
begin
  FTexture := NewTexture;
end;

function TSDLTextureLoader.GetSharedContext(out Context: SDL_GLContext): Boolean;
begin
  Result := FWindow.GetSharedContext(Context)
end;

function TSDLTextureLoader.PutSharedContext(const Context: SDL_GLContext): Boolean;
begin
  Result := FWindow.PutSharedContext(Context)
end;

function TSDLTextureLoader.GetSDLWindow: PSDL_Window;
begin
  Result := FWindow.Wnd
end;

function TSDLTextureLoader.GetLogger: ILogger;
begin
  Result := nil
end;

function TSDLTextureLoader.GetProvider: TComponent;
var
  Provider: IInternalProviderContainer;
begin
  if FWindow.GetInterface(IInternalProviderContainer, Provider) then
    Result := Provider.GetInternalProvider
  else
    Result := nil
end;

constructor TSDLTextureLoader.Create(constref AWindow: TSDLWindow);
begin
  FWindow := AWindow
end;

procedure TSDLTextureLoader.AfterConstruction;
begin
  inherited AfterConstruction;

  FWorker := TSDLTextureWorker.Create;
  FWorker.OnTextureLoaded := @ExternalTextureLoaded
end;

procedure TSDLTextureLoader.BeforeDestruction;
begin
  FWorker.Free;

  inherited BeforeDestruction;
end;

function TSDLTextureLoader.LoadTexture(const FileName: String; out
  Texture: PSDLTextureContext): Boolean;
begin
  Result := LoadTexture(TFileStream.Create(FileName, fmOpenRead), Texture, True)
end;

function TSDLTextureLoader.LoadTexture(const Stream: TStream; out
  Texture: PSDLTextureContext; FreeStream: Boolean): Boolean;
var
  Data: Pointer;
begin
  FTexture := nil;
  Data := FWorker.CreateData(Stream, FreeStream);
  Result := FWorker.Execute(Self, Data);
  Texture := FTexture;
end;

{ TSDLTextureWorker }

procedure TSDLTextureWorker.SetOnTextureLoaded(AValue: TSDLTextureLoadedvent);
begin
  FOnTextureLoaded:=AValue;
end;

procedure TSDLTextureWorker.InternalLoadImage(const Data: PSDLTextureData;
  const Provider: TSDLCustomLibraryProvider; out Image: PSDL_Surface);
var
  NewImage: PSDL_Surface;
  SDLImageApi: TSDL2_ImageLibrary;
  SDLSurfaceApi: TSDLSurfaceApi;
  SDLStream: TSDLStream;
  Bpp: SDL_UInt8;
  Buff, i, j: Integer;
  Start, Finish: Pointer;
begin
  SDLImageApi := Provider.Libs['SDL_Image'] as TSDL2_ImageLibrary;

  if not Assigned(SDLImageApi) then
    raise Exception.CreateFmt(rsLibraryNotFound, ['SDL_Image']);

  SDLSurfaceApi := TSDLSurfaceApi.Create(Provider.Libs['SDL2']);
  try
    SDLSurfaceApi.Init;
    if not SDLSurfaceApi.Valid then
      raise SDL_ImageException.Create(SDLSurfaceApi.LastError);

    SDLStream := TSDLStream.Create(Data^.InputStream, Data^.FreeStream);
    try
      Image := SDLImageApi.IMG_Load_RW(SDLStream.SDL_RWops, SDL_FALSE);
    finally
      SDLStream.Free
    end;

    if not Assigned(Image) then
      raise SDL_ImageException.Create(SDLImageApi.LastError);

    if Image^.format^.format <> SDL_PIXELFORMAT_ABGR8888 then
    begin
      NewImage := SDLSurfaceApi.SDL_ConvertSurfaceFormat(Image, SDL_PIXELFORMAT_ABGR8888, SDL_SWSURFACE);
      if Assigned(NewImage) then
      begin
        SDLSurfaceApi.SDL_FreeSurface(Image);
        Image := NewImage;
      end
    end;
  finally
    SDLSurfaceApi.Free
  end;

  Buff := 0;
  Bpp := Image^.format^.BytesPerPixel;

  for i := 0 to (Image^.h - 1) div 2 do
  begin
    Start := (Image^.pixels + i * Image^.w * Bpp);
    Finish := (Image^.pixels + (Image^.h - i - 1) * Image^.w * Bpp);
    for j := 0 to Image^.w - 1 do
    begin
      Move(Start^, Buff, Bpp);
      Move(Finish^, Start^, Bpp);
      Move(Buff, Finish^, Bpp);
      Inc(Start, Bpp);
      Inc(Finish, Bpp);
    end;
  end;
end;

procedure TSDLTextureWorker.InternalLoadTexture(const Image: PSDL_Surface;
  const Provider: TSDLCustomLibraryProvider; const GLTexture: PSDLTextureContext
  );
var
  libGL: TOpenGLLibrary;
begin
  libGL := Provider.Libs['OpenGL'] as TOpenGLLibrary;

  if not Assigned(libGL) then
    raise Exception.CreateFmt(rsLibraryNotFound, ['OpenGL']);

  libGL.glActiveTexture(GL_TEXTURE0);
  libGL.glGenTextures(1, @GLTexture^.ID);

  libGL.glBindTexture(GL_TEXTURE_2D, GLTexture^.ID);

  libGL.glTexParameteri(GL_TEXTURE_2D_TEX, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  libGL.glTexParameteri(GL_TEXTURE_2D_TEX, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  libGL.glTexParameteri(GL_TEXTURE_2D_TEX, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  libGL.glTexParameteri(GL_TEXTURE_2D_TEX, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  libGL.glTexImage2D(
    GL_TEXTURE_2D_TXIMG,
    0,
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
    Image^.w,
    Image^.h,
    0,
    GL_RGBA,
    GL_UNSIGNED_BYTE,
    Image^.pixels
  );
  libGL.glGenerateMipmap(GL_TEXTURE_2D_MIN);

  {glGetTexLevelParameteriv(
    GL_TEXTURE_2D, 0,
    GL_TEXTURE_COMPRESSED_IMAGE_SIZE,
    @GLTexture^.CompressedSize
  );   }
end;

function TSDLTextureWorker.Execute(const Control: IControl; var Data: Pointer): Boolean;
var
  TextureData: PSDLTextureData absolute Data;
  NewTexture: PSDLTextureContext;
  LibProvider: TSDLCustomLibraryProvider;
  SDLVideoApi: TSDLVideoApi;
  SDLSurfaceApi: TSDLSurfaceApi;
  SDLTexture: PSDL_Surface;
  Context: SDL_GLContext;
begin
  TextureData^.ResultTexture := nil;

  New(NewTexture);
  try
    NewTexture^.ID := 0;

    LibProvider := Control.GetProvider as TSDLCustomLibraryProvider;
    SDLVideoApi := TSDLVideoApi.Create(LibProvider.Libs['SDL2']);
    SDLSurfaceApi := TSDLSurfaceApi.Create(LibProvider.Libs['SDL2']);
    try
      SDLVideoApi.Init;
      if not SDLVideoApi.Valid then
        raise SDL_ImageException.Create(SDLVideoApi.LastError);

      SDLSurfaceApi.Init;
      if not SDLSurfaceApi.Valid then
        raise SDL_ImageException.Create(SDLSurfaceApi.LastError);

      InternalLoadImage(TextureData, Control.GetProvider as TSDLCustomLibraryProvider, SDLTexture);
      try
        NewTexture^.Width := SDLTexture^.w;
        NewTexture^.Height := SDLTexture^.h;

        if SDLVideoApi.SDL_GL_GetCurrentContext() = nil then
        begin
          Control.GetSharedContext(Context);
          try
            SDLVideoApi.SDL_GL_MakeCurrent(Control.GetSDLWindow, Context);
            try
              InternalLoadTexture(SDLTexture, LibProvider, NewTexture)
            finally
              SDLVideoApi.SDL_GL_MakeCurrent(Control.GetSDLWindow, nil);
            end
          finally
            Control.PutSharedContext(Context)
          end
        end
        else
          InternalLoadTexture(SDLTexture, LibProvider, NewTexture)
      finally
        SDLSurfaceApi.SDL_FreeSurface(SDLTexture)
      end;
    finally
      SDLVideoApi.Free;
      SDLSurfaceApi.Free;
    end;

    Result := True;
    TextureData^.ResultTexture := NewTexture;

    if Assigned(FOnTextureLoaded) then
      FOnTextureLoaded(Self, NewTexture);
  except
    on Exception do
    begin
      TextureData^.ResultTexture := nil;
      Dispose(NewTexture);
      Result := False
    end
  end
end;

function TSDLTextureWorker.CreateData(const Stream: TStream; FreeStream: Boolean
  ): Pointer;
var
  NewData: PSDLTextureData;
begin
  New(NewData);
  NewData^.InputStream := Stream;
  NewData^.FreeStream := FreeStream;
  Result := NewData;
end;

procedure TSDLTextureWorker.AddRef;
begin
  _AddRef
end;

procedure TSDLTextureWorker.Release;
begin
  _Release
end;

end.

