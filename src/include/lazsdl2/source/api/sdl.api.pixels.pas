unit SDL.Api.Pixels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLPixelsApi }

  TSDLPixelsApi = class(TSubLibrary)
  private type
    TSDL_GetPixelFormatName = function(format: SDL_PixelFormatEnum): SDL_String cdecl;
    TSDL_PixelFormatEnumToMasks = function(format: SDL_PixelFormatEnum; out bpp: SDL_SInt32; out Rmask: SDL_UInt32; out GSmask: SDL_UInt32; out Bmask: SDL_UInt32; out Amask: SDL_UInt32): SDL_Bool cdecl;
    TSDL_MasksToPixelFormatEnum = function(bpp: SDL_SInt32; Rmask: SDL_UInt32; Gmask: SDL_UInt32; Bmask: SDL_UInt32; Amask: SDL_UInt32): SDL_PixelFormatEnum cdecl;
    TSDL_AllocFormat = function(pixel_format: SDL_PixelFormatEnum): PSDL_PixelFormat cdecl;
    TSDL_FreeFormat = procedure(format: PSDL_PixelFormat) cdecl;
    TSDL_AllocPalette = function(ncolors: SDL_SInt32): PSDL_Palette cdecl;
    TSDL_SetPixelFormatPalette = function(format: PSDL_PixelFormat; palette: PSDL_Palette): SDL_SInt32 cdecl;
    TSDL_SetPaletteColors = function(palette: PSDL_Palette; const colors: PSDL_Color; firstcolor: SDL_SInt32; ncolors: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_FreePalette = procedure(palette: PSDL_Palette) cdecl;
    TSDL_MapRGB = function(const format: PSDL_PixelFormat; r: SDL_UInt8; g: SDL_UInt8; b: SDL_UInt8): SDL_UInt32 cdecl;
    TSDL_MapRGBA = function(const format: PSDL_PixelFormat; r: SDL_UInt8; g: SDL_UInt8; b: SDL_UInt8; a: SDL_UInt8): SDL_UInt32 cdecl;
    TSDL_GetRGB = procedure(pixel: SDL_UInt32; const format: PSDL_PixelFormat; out r: SDL_UInt8; out g: SDL_UInt8; out b: SDL_UInt8) cdecl;
    TSDL_GetRGBA = procedure (pixel: SDL_UInt32; const format: PSDL_PixelFormat; out r: SDL_UInt8; out g: SDL_UInt8; out b: SDL_UInt8; out a: SDL_UInt8) cdecl;
    TSDL_CalculateGammaRamp = procedure(gamma: SDL_Float; ramp: PSDL_UInt16) cdecl;
  public
    (**
     * \brief Get the human readable name of a pixel format
     *)
    SDL_GetPixelFormatName: TSDL_GetPixelFormatName;
    (**
     *  \brief Convert one of the enumerated pixel formats to a bpp and RGBA masks.
     *
     *  \return SDL_TRUE, or SDL_FALSE if the conversion wasn't possible.
     *
     *  \sa SDL_MasksToPixelFormatEnum()
     *)
    SDL_PixelFormatEnumToMasks: TSDL_PixelFormatEnumToMasks;
    (**
     *  \brief Convert a bpp and RGBA masks to an enumerated pixel format.
     *
     *  \return The pixel format, or ::SDL_PIXELFORMAT_UNKNOWN if the conversion
     *          wasn't possible.
     *
     *  \sa SDL_PixelFormatEnumToMasks()
     *)
    SDL_MasksToPixelFormatEnum: TSDL_MasksToPixelFormatEnum;
    (**
     *  \brief Create an SDL_PixelFormat structure from a pixel format enum.
     *)
    SDL_AllocFormat: TSDL_AllocFormat;
    (**
     *  \brief Free an SDL_PixelFormat structure.
     *)
    SDL_FreeFormat: TSDL_FreeFormat;
    (**
     *  \brief Create a palette structure with the specified number of color
     *         entries.
     *
     *  \return A new palette, or NULL if there wasn't enough memory.
     *
     *  \note The palette entries are initialized to white.
     *
     *  \sa SDL_FreePalette()
     *)
     SDL_AllocPalette: TSDL_AllocPalette;
     (**
     *  \brief Set the palette for a pixel format structure.
     *)
     SDL_SetPixelFormatPalette: TSDL_SetPixelFormatPalette;
     (**
      *  \brief Set a range of colors in a palette.
      *
      *  \param palette    The palette to modify.
      *  \param colors     An array of colors to copy into the palette.
      *  \param firstcolor The index of the first palette entry to modify.
      *  \param ncolors    The number of entries to modify.
      *
      *  \return 0 on success, or -1 if not all of the colors could be set.
      *)
     SDL_SetPaletteColors: TSDL_SetPaletteColors;
     (**
      *  \brief Free a palette created with SDL_AllocPalette().
      *
      *  \sa SDL_AllocPalette()
      *)
     SDL_FreePalette: TSDL_FreePalette;
     (**
      *  \brief Maps an RGB triple to an opaque pixel value for a given pixel format.
      *
      *  \sa SDL_MapRGBA
      *)
     SDL_MapRGB: TSDL_MapRGB;
     (**
      *  \brief Get the RGB components from a pixel of the specified format.
      *
      *  \sa SDL_GetRGBA
      *)
     SDL_MapRGBA: TSDL_MapRGBA;
     (**
      *  \brief Get the RGB components from a pixel of the specified format.
      *
      *  \sa SDL_GetRGBA
      *)
     SDL_GetRGB: TSDL_GetRGB;
     (**
      *  \brief Get the RGBA components from a pixel of the specified format.
      *
      *  \sa SDL_GetRGB
      *)
     SDL_GetRGBA: TSDL_GetRGBA;
     (**
      *  \brief Calculate a 256 entry gamma ramp for a gamma value.
      *)
     SDL_CalculateGammaRamp: TSDL_CalculateGammaRamp;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    function SDL_DEFINE_PIXELFOURCC(const A, B, C, D: SDL_UInt32): SDL_SInt32;
    function SDL_DEFINE_PIXELFORMAT(const _type, order, layout, bits, bytes: SDL_UInt32): SDL_SInt32;
    function SDL_ISPIXELFORMAT_FOURCC(const X: SDL_UInt32): Boolean;
    function SDL_PIXELFLAG(const X: SDL_UInt32): Boolean;
    function SDL_PIXELTYPE(const X: SDL_UInt32): Boolean;
    function SDL_PIXELORDER(const X: SDL_UInt32): Boolean;
    function SDL_PIXELLAYOUT(const X: SDL_UInt32): Boolean;
    function SDL_BITSPERPIXEL(const X: SDL_UInt32): Boolean;
  end;

implementation

{ TSDLPixelsApi }

procedure TSDLPixelsApi.DoInit;
begin

end;

procedure TSDLPixelsApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetPixelFormatName', @SDL_GetPixelFormatName);
  List.Add('SDL_PixelFormatEnumToMasks', @SDL_PixelFormatEnumToMasks);
  List.Add('SDL_MasksToPixelFormatEnum', @SDL_MasksToPixelFormatEnum);
  List.Add('SDL_AllocFormat', @SDL_AllocFormat);
  List.Add('SDL_FreeFormat', @SDL_FreeFormat);
  List.Add('SDL_AllocPalette', @SDL_AllocPalette);
  List.Add('SDL_SetPixelFormatPalette', @SDL_SetPixelFormatPalette);
  List.Add('SDL_SetPaletteColors', @SDL_SetPaletteColors);
  List.Add('SDL_FreePalette', @SDL_FreePalette);
  List.Add('SDL_MapRGB', @SDL_MapRGB);
  List.Add('SDL_MapRGBA', @SDL_MapRGBA);
  List.Add('SDL_GetRGB', @SDL_GetRGB);
  List.Add('SDL_GetRGBA', @SDL_GetRGBA);
  List.Add('SDL_CalculateGammaRamp', @SDL_CalculateGammaRamp);
end;

function TSDLPixelsApi.SDL_DEFINE_PIXELFOURCC(const A, B, C, D: SDL_UInt32
  ): SDL_SInt32;
begin
  Result :=
     A or
    (B shl  8) or
    (C shl 16) or
    (D shl 24)
end;

function TSDLPixelsApi.SDL_DEFINE_PIXELFORMAT(const _type, order, layout, bits,
  bytes: SDL_UInt32): SDL_SInt32;
begin
  Result :=
    (1 shl 28) or
    (_type shl 24) or
    (order shl 20) or
    (layout shl 16) or
    (bits shl 8) or
    (bytes shl 0)

end;

function TSDLPixelsApi.SDL_ISPIXELFORMAT_FOURCC(const X: SDL_UInt32): Boolean;
begin
  Result := (X and Integer(SDL_PIXELFLAG(X) <> True)) = X;
end;

function TSDLPixelsApi.SDL_PIXELFLAG(const X: SDL_UInt32): Boolean;
begin
  Result := (X shr 28) = $0F
end;

function TSDLPixelsApi.SDL_PIXELTYPE(const X: SDL_UInt32): Boolean;
begin
  Result := (X shr 24) = $0F
end;

function TSDLPixelsApi.SDL_PIXELORDER(const X: SDL_UInt32): Boolean;
begin
  Result := (X shr 20) = $0F
end;

function TSDLPixelsApi.SDL_PIXELLAYOUT(const X: SDL_UInt32): Boolean;
begin
  Result := (X shr 16) = $0F
end;

function TSDLPixelsApi.SDL_BITSPERPIXEL(const X: SDL_UInt32): Boolean;
begin
  Result := (X shr 8) = $FF
end;

end.

