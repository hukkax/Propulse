unit SDL.Extended.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Extended.WorkerControl, SDL.Extended.Loaders, GraphType,
  SDL.Api.libSDL_Image, Graphics;

type

  { TSDLCustomWorkerControlHelper }

  TSDLCustomWorkerControlHelper = class helper for TSDLCustomWorkerControl
  public
    (**
      *
      * \brief Adds texture loading job to queue
      *
      * \param JobName Used for user to identifie job on loading events
      * \param Stream Stream containing texture data
      * \param OnLoaded Event witch will be fired after texture is loaded
      *
    **)
    (* @{ *)
    procedure AddTextureLoading(const JobName: String; const Stream: TStream);
    procedure AddTextureLoading(const JobName: String; const Stream: TStream; OnLoaded: TSDLTextureLoadedvent);
    (* @} *)
  end;

  function LoadSDLImage(const Path: TFilename; const SDL_ImageApi: TSDL2_ImageLibrary; out Image: TRawImage): Boolean;
  function LoadSDLImage(const Path: TFilename; const SDL_ImageApi: TSDL2_ImageLibrary; const Image: TRasterImage): Boolean;

implementation

uses
  SDL.Api.Types;

var
  InternalTextureWorker: TSDLTextureWorker;

function LoadSDLImage(const Path: TFilename; const SDL_ImageApi: TSDL2_ImageLibrary; out Image: TRawImage): Boolean;
var
  SDLImage, NewImage: PSDL_Surface;
begin
  Image.Init;

  if not Assigned(SDL_ImageApi) or not SDL_ImageApi.Valid then
    Exit(False);

  SDLImage := SDL_ImageApi.IMG_Load(SDL_String(Path));
  try
    if not Assigned(SDLImage) then
      Exit(False);

    if SDLImage^.format^.format <> SDL_PIXELFORMAT_ABGR8888 then
    begin
      NewImage := SDL_ImageApi.Surface.SDL_ConvertSurfaceFormat(SDLImage, SDL_PIXELFORMAT_ABGR8888, SDL_SWSURFACE);
      if Assigned(NewImage) then
      begin
        SDL_ImageApi.Surface.SDL_FreeSurface(SDLImage);
        SDLImage := NewImage
      end
    end;

    Image.Description.Init_BPP32_R8G8B8A8_BIO_TTB(SDLImage^.w, SDLImage^.h);
    Image.CreateData(False);

    Move(SDLImage^.pixels^, Image.Data^, Image.DataSize)
  finally
    SDL_ImageApi.Surface.SDL_FreeSurface(SDLImage)
  end;

  Result := True;
end;

function LoadSDLImage(const Path: TFilename; const SDL_ImageApi: TSDL2_ImageLibrary; const Image: TRasterImage): Boolean;
var
  RawImage: TRawImage;
begin
  if LoadSDLImage(Path, SDL_ImageApi, RawImage) then
  begin
    Image.LoadFromRawImage(RawImage, True);
    Result := True
  end
  else
    Result := False
end;

{ TSDLCustomWorkerControlHelper }

procedure TSDLCustomWorkerControlHelper.AddTextureLoading(const JobName: String; const Stream: TStream);
var
  Worker: TSDLTextureWorker;
begin
  Worker := TSDLTextureWorker.Create;
  Worker.OnTextureLoaded := InternalTextureWorker.OnTextureLoaded;
  AddWorker(JobName, Worker, Worker.CreateData(Stream, True))
end;

procedure TSDLCustomWorkerControlHelper.AddTextureLoading(const JobName: String; const Stream: TStream; OnLoaded: TSDLTextureLoadedvent);
var
  Worker: TSDLTextureWorker;
begin
  Worker := TSDLTextureWorker.Create;
  Worker.OnTextureLoaded := OnLoaded;
  AddWorker(JobName, Worker, Worker.CreateData(Stream, True));
end;

initialization
  InternalTextureWorker := TSDLTextureWorker.Create;
  InternalTextureWorker.AddRef;

finalization
  InternalTextureWorker.Release;

end.

