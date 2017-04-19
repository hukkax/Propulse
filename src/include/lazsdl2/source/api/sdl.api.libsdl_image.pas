(*
  SDL_image:  An example image loading library for use with SDL
  Copyright (C) 1997-2016 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*)
unit SDL.Api.libSDL_Image;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DLibrary, SDL.Api.Types, SDL.Api.libSDL2, SDL.Api.Surface;

type

  { TSDL2_ImageLibrary }

  TSDL2_ImageLibrary = class(TLibrary)
  public const
    SDL_IMAGE_MAJOR_VERSION = 2;
    SDL_IMAGE_MINOR_VERSION = 0;
    SDL_IMAGE_PATCHLEVEL = 1;
  private type
    TIMG_Linked_Version = function(): PSDL_Version cdecl;
    TIMG_Init = function(): SDL_ResultCode cdecl;
    TIMG_Quit = procedure() cdecl;
    TIMG_LoadTyped_RW = function(src: PSDL_RWops; freesrc: SDL_Bool; _type: SDL_String): PSDL_Surface cdecl;
    TIMG_Load = function(_file: SDL_String): PSDL_Surface cdecl;
    TIMG_Load_RW = function(src: PSDL_RWops; freesrc: SDL_Bool): PSDL_Surface cdecl;
    TIMG_LoadTexture = function(renderer: PSDL_Renderer; _file: SDL_String): PSDL_Texture cdecl;
    TIMG_LoadTexture_RW = function(renderer: PSDL_Renderer; src: PSDL_RWops; freesrc: SDL_Bool): PSDL_Texture cdecl;
    TIMG_LoadTextureTyped_RW = function(renderer: PSDL_Renderer; src: PSDL_RWops; freesrc: SDL_Bool; _type: SDL_String): PSDL_Texture cdecl;
    TIMG_isICO = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isCUR = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isBMP = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isGIF = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isJPG = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isLBM = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isPCX = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isPNG = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isPNM = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isTIF = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isXCF = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isXPM = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isXV = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_isWEBP = function(src: PSDL_RWops): SDL_Bool cdecl;
    TIMG_LoadICO_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadCUR_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadBMP_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadGIF_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadJPG_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadLBM_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadPCX_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadPNG_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadPNM_RW= function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadTGA_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadTIF_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadXCF_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadXPM_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadXV_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_LoadWEBP_RW = function(src: PSDL_RWops): PSDL_Surface cdecl;
    TIMG_ReadXPMFromArray = function(xpm: PPChar): PSDL_Surface cdecl;
    TIMG_SavePNG = function(surface: PSDL_Surface; filename: SDL_String): SDL_ResultCode cdecl;
    TIMG_SavePNG_RW = function(surface: PSDL_Surface; dst: PSDL_RWops; freedst: Boolean): SDL_ResultCode cdecl;
  public
    (** This function gets the version of the dynamically linked SDL_image library.
       it should NOT be used to fill a version structure, instead you should
       use the SDL_IMAGE_VERSION() macro.
     *)
    IMG_Linked_Version: TIMG_Linked_Version;
    (** Loads dynamic libraries and prepares them for use.  Flags should be
       one or more flags from IMG_InitFlags OR'd together.
       It returns the flags successfully initialized, or 0 on failure.
     *)
    IMG_Init: TIMG_Init;
    (** Unloads libraries loaded with IMG_Init *)
    IMG_Quit: TIMG_Quit;
    (** Load an image from an SDL data source.
       The 'type' may be one of: "BMP", "GIF", "PNG", etc.

       Here is a list of the currently recognized strings (case is not important):

      "BMP", "CUR", "GIF", "ICO", "JPG", "LBM"
      "PCX", "PNG", "PNM", "TGA", "TIF", "XCF"
      "XPM", "XV"

       If the image format supports a transparent pixel, SDL will set the
       colorkey for the surface.  You can enable RLE acceleration on the
       surface afterwards by calling:
        SDL_SetColorKey(image, SDL_RLEACCEL, image->format->colorkey);
     *)
    (* @{ *)
    IMG_LoadTyped_RW: TIMG_LoadTyped_RW;
    IMG_Load: TIMG_Load;
    IMG_Load_RW: TIMG_Load_RW;
    (* @} *)
    (** Load an image directly into a render texture.
     *)
    (* @{ *)
    IMG_LoadTexture: TIMG_LoadTexture;
    IMG_LoadTexture_RW: TIMG_LoadTexture_RW;
    IMG_LoadTextureTyped_RW: TIMG_LoadTextureTyped_RW;
    (* @} *)
    (** Functions to detect a file type, given a seekable source *)
    (* @{ *)
    IMG_isICO: TIMG_isICO;
    IMG_isCUR: TIMG_isCUR;
    IMG_isBMP: TIMG_isBMP;
    IMG_isGIF: TIMG_isGIF;
    IMG_isJPG: TIMG_isJPG;
    IMG_isLBM: TIMG_isLBM;
    IMG_isPCX: TIMG_isPCX;
    IMG_isPNG: TIMG_isPNG;
    IMG_isPNM: TIMG_isPNM;
    IMG_isTIF: TIMG_isTIF;
    IMG_isXCF: TIMG_isXCF;
    IMG_isXPM: TIMG_isXPM;
    IMG_isXV: TIMG_isXV;
    IMG_isWEBP: TIMG_isWEBP;
    (* @} *)
    (** Individual loading functions *)
    (* @{ *)
    IMG_LoadICO_RW: TIMG_LoadICO_RW;
    IMG_LoadCUR_RW: TIMG_LoadCUR_RW;
    IMG_LoadBMP_RW: TIMG_LoadBMP_RW;
    IMG_LoadGIF_RW: TIMG_LoadGIF_RW;
    IMG_LoadJPG_RW: TIMG_LoadJPG_RW;
    IMG_LoadLBM_RW: TIMG_LoadLBM_RW;
    IMG_LoadPCX_RW: TIMG_LoadPCX_RW;
    IMG_LoadPNG_RW: TIMG_LoadPNG_RW;
    IMG_LoadPNM_RW: TIMG_LoadPNM_RW;
    IMG_LoadTGA_RW: TIMG_LoadTGA_RW;
    IMG_LoadTIF_RW: TIMG_LoadTIF_RW;
    IMG_LoadXCF_RW: TIMG_LoadXCF_RW;
    IMG_LoadXPM_RW: TIMG_LoadXPM_RW;
    IMG_LoadXV_RW: TIMG_LoadXV_RW;
    IMG_LoadWEBP_RW: TIMG_LoadWEBP_RW;
    IMG_ReadXPMFromArray: TIMG_ReadXPMFromArray;
    (* @} *)
    (** Individual saving functions *)
    (* @{ *)
    IMG_SavePNG: TIMG_SavePNG;
    IMG_SavePNG_RW: TIMG_SavePNG_RW;
    (* @} *)
  private
    FSDL2: TSDL2Library;
    FSurface: TSDLSurfaceApi;
  protected
    procedure DoGetFileVersion(out VersionStr: String); override;
    procedure DoGetLastError(out ErrorStr: String); override;
    procedure DoSetLastError(const ErrorStr: String); override;
    procedure GetRequiredMethods(const List: TMethodList); override;
    procedure DoInit; override;
  public
    constructor Create(const LibraryFile: String; SDL2: TSDL2Library); reintroduce;

    (* This macro can be used to fill a version structure with the compile-time
     * version of the SDL_image library.
     *)
    procedure SDL_IMAGE_VERSION(out Ver: SDL_Version);
    (** We'll use SDL for reporting errors *)
    (* @{ *)
    procedure IMG_SetError(const fmt: SDL_String; params: array of const);
    function IMG_GetError: SDL_String;
    (* @} *)

    property Surface: TSDLSurfaceApi read FSurface;
  end;

implementation

{ TSDL2_ImageLibrary }

procedure TSDL2_ImageLibrary.DoGetFileVersion(out VersionStr: String);
var
  Ver: SDL_Version;
begin
  if not Assigned(IMG_Linked_Version) then
  begin
    Ver.major := 0;
    Ver.minor := 0;
    Ver.patch := 0;
  end
  else
    Ver := IMG_Linked_Version()^;

  VersionStr := Format('%d.%d.%d.%d', [Ver.Major, Ver.Minor, Ver.Patch, 0]);
end;

procedure TSDL2_ImageLibrary.DoGetLastError(out ErrorStr: String);
begin
  if FSDL2.Valid then
    ErrorStr := FSDL2.LastError
  else
    inherited DoGetLastError(ErrorStr)
end;

procedure TSDL2_ImageLibrary.DoSetLastError(const ErrorStr: String);
begin
  if FSDL2.Valid then
    FSDL2.LastError := ErrorStr
  else
    inherited DoSetLastError(ErrorStr)
end;

procedure TSDL2_ImageLibrary.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('IMG_Linked_Version', @IMG_Linked_Version);
  List.Add('IMG_Init', @IMG_Init);
  List.Add('IMG_Quit', @IMG_Quit);
  List.Add('IMG_LoadTyped_RW', @IMG_LoadTyped_RW);
  List.Add('IMG_Load', @IMG_Load);
  List.Add('IMG_Load_RW', @IMG_Load_RW);
  List.Add('IMG_LoadTexture', @IMG_LoadTexture);
  List.Add('IMG_LoadTexture_RW', @IMG_LoadTexture_RW);
  List.Add('IMG_LoadTextureTyped_RW', @IMG_LoadTextureTyped_RW);
  List.Add('IMG_isICO', @IMG_isICO);
  List.Add('IMG_isCUR', @IMG_isCUR);
  List.Add('IMG_isBMP', @IMG_isBMP);
  List.Add('IMG_isGIF', @IMG_isGIF);
  List.Add('IMG_isJPG', @IMG_isJPG);
  List.Add('IMG_isLBM', @IMG_isLBM);
  List.Add('IMG_isPCX', @IMG_isPCX);
  List.Add('IMG_isPNG', @IMG_isPNG);
  List.Add('IMG_isPNM', @IMG_isPNM);
  List.Add('IMG_isTIF', @IMG_isTIF);
  List.Add('IMG_isXCF', @IMG_isXCF);
  List.Add('IMG_isXPM', @IMG_isXPM);
  List.Add('IMG_isXV', @IMG_isXV);
  List.Add('IMG_isWEBP', @IMG_isWEBP);
  List.Add('IMG_LoadICO_RW', @IMG_LoadICO_RW);
  List.Add('IMG_LoadBMP_RW', @IMG_LoadBMP_RW);
  List.Add('IMG_LoadGIF_RW', @IMG_LoadGIF_RW);
  List.Add('IMG_LoadJPG_RW', @IMG_LoadJPG_RW);
  List.Add('IMG_LoadLBM_RW', @IMG_LoadLBM_RW);
  List.Add('IMG_LoadPCX_RW', @IMG_LoadPCX_RW);
  List.Add('IMG_LoadPNG_RW', @IMG_LoadPNG_RW);
  List.Add('IMG_LoadPNM_RW', @IMG_LoadPNM_RW);
  List.Add('IMG_LoadTGA_RW', @IMG_LoadTGA_RW);
  List.Add('IMG_LoadTIF_RW', @IMG_LoadTIF_RW);
  List.Add('IMG_LoadXCF_RW', @IMG_LoadXCF_RW);
  List.Add('IMG_LoadXPM_RW', @IMG_LoadXPM_RW);
  List.Add('IMG_LoadXV_RW', @IMG_LoadXV_RW);
  List.Add('IMG_LoadWEBP_RW', @IMG_LoadWEBP_RW);
  List.Add('IMG_ReadXPMFromArray', @IMG_ReadXPMFromArray);
  List.Add('IMG_SavePNG', @IMG_SavePNG);
  List.Add('IMG_SavePNG_RW', @IMG_SavePNG_RW)
end;

procedure TSDL2_ImageLibrary.DoInit;
begin
  inherited DoInit;

  FSurface := TSDLSurfaceApi.Create(FSDL2);
  FSurface.Init;

  if not FSurface.Valid then
    LastError := LastError + FSurface.LastError + LineEnding;
end;

constructor TSDL2_ImageLibrary.Create(const LibraryFile: String;
  SDL2: TSDL2Library);
var
  RezFileName: String;
begin
  if LibraryFile = '' then
  begin
    {$IFDEF WINDOWS}
      RezFileName := 'SDL2_image.dll';
    {$ENDIF}

    {$IFDEF UNIX}
      {$IFDEF DARWIN}
        RezFileName := 'libSDL2_image.dylib';
      {$ELSE}
        {$IFDEF ANDROID}
        RezFileName := 'libSDL2_image.so';
        {$ELSE}
        RezFileName := 'libSDL2_image-2.0.so.0';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}

    {$IFDEF MACOS}
      RezFileName := 'SDL2_image';
      {$linklib libSDL2_image}
    {$ENDIF}
  end
  else
    RezFileName := LibraryFile;

  inherited Create(RezFileName);
  FSDL2 := SDL2;
end;

procedure TSDL2_ImageLibrary.SDL_IMAGE_VERSION(out Ver: SDL_Version);
begin
  Ver.major := SDL_IMAGE_MAJOR_VERSION;
  Ver.minor := SDL_IMAGE_MINOR_VERSION;
  Ver.patch := SDL_IMAGE_PATCHLEVEL
end;

procedure TSDL2_ImageLibrary.IMG_SetError(const fmt: SDL_String;
  params: array of const);
begin
  FSDL2.Error.SDL_SetError(SDL_String(Format(fmt, params)))
end;

function TSDL2_ImageLibrary.IMG_GetError: SDL_String;
begin
  Result := FSDL2.Error.SDL_GetError()
end;

end.

