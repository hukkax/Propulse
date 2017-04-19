(**
 *  \file SDL_surface.h
 *
 *  Header file for ::SDL_Surface definition and management functions.
 *)
unit SDL.Api.Surface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary, SDL.Api.Rwops;

type

  { TSDLSurfaceApi }

  TSDLSurfaceApi = class(TSubLibrary)
  public type
    TSDL_Blit = function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32;
  private type
    TSDL_CreateRGBSurface = function(flags: SDL_UInt32; Width: SDL_SInt32; Height: SDL_SInt32; depth: SDL_SInt32; Rmask: SDL_UInt32; Gmask: SDL_UInt32; Bmask: SDL_UInt32; Amask: SDL_UInt32): PSDL_Surface cdecl;
    TSDL_CreateRGBSurfaceFrom = function(pixels: Pointer; Width: SDL_SInt32; Height: SDL_SInt32; depth: SDL_SInt32; pitch: SDL_SInt32; Rmask: SDL_UInt32; Gmask: SDL_UInt32; Bmask: SDL_UInt32; Amask: SDL_UInt32): PSDL_Surface cdecl;
    TSDL_FreeSurface = procedure(surface: PSDL_Surface) cdecl;
    TSDL_SetSurfacePalette = function(surface: PSDL_Surface; palette: PSDL_Palette): SDL_SInt32 cdecl;
    TSDL_LockSurface = function(surface: PSDL_Surface): SDL_SInt32 cdecl;
    TSDL_UnlockSurface = procedure(surface: PSDL_Surface) cdecl;
    TSDL_LoadBMP_RW = function(src: PSDL_RWops; freesrc: SDL_SInt32): PSDL_Surface cdecl;
    TSDL_SaveBMP_RW = function(surface: PSDL_Surface; dst: PSDL_RWops; freedst: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_SetSurfaceRLE = function(surface: PSDL_Surface; flag: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_SetColorKey = function(surface: PSDL_Surface; flag: SDL_SInt32; key: SDL_UInt32): SDL_SInt32 cdecl;
    TSDL_GetColorKey = function(surface: PSDL_Surface; key: PSDL_UInt32): SDL_SInt32 cdecl;
    TSDL_SetSurfaceColorMod = function(surface: PSDL_Surface; r: SDL_UInt8; g: SDL_UInt8; b: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_GetSurfaceColorMod = function(surface: PSDL_Surface; r: PSDL_UInt8; g: PSDL_UInt8; b: PSDL_UInt8): SDL_SInt32 cdecl;
    TSDL_SetSurfaceAlphaMod = function(surface: PSDL_Surface; alpha: SDL_UInt8): SDL_SInt32 cdecl;
    TSDL_GetSurfaceAlphaMod = function(surface: PSDL_Surface; alpha: PSDL_UInt8): SDL_SInt32 cdecl;
    TSDL_SetSurfaceBlendMode = function(surface: PSDL_Surface; blendMode: SDL_BlendMode): SDL_SInt32 cdecl;
    TSDL_GetSurfaceBlendMode = function(surface: PSDL_Surface; blendMode: PSDL_BlendMode): SDL_SInt32 cdecl;
    TSDL_SetClipRect = function(surface: PSDL_Surface; const rect: PSDL_Rect): SDL_Bool cdecl;
    TSDL_GetClipRect = procedure(surface: PSDL_Surface; rect: PSDL_Rect) cdecl;
    TSDL_ConvertSurface = function(src: PSDL_Surface; fmt: PSDL_PixelFormat; flags: SDL_Surface_Flags): PSDL_Surface cdecl;
    TSDL_ConvertSurfaceFormat = function(src: PSDL_Surface; pixel_format: SDL_PixelFormatEnum; flags: SDL_Surface_Flags): PSDL_Surface cdecl;
    TSDL_ConvertPixels = function(Width: SDL_SInt32; Height: SDL_SInt32; src_format: SDL_UInt32; const src: Pointer; src_pitch: SDL_SInt32; dst_format: SDL_UInt32; dst: Pointer; dst_pitch: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_FillRect = function(dst: PSDL_Surface; const rect: PSDL_Rect; color: SDL_UInt32): SDL_SInt32 cdecl;
    TSDL_FillRects = function(dst: PSDL_Surface; const rects: PSDL_Rect; Count: SDL_SInt32; color: SDL_UInt32): SDL_SInt32 cdecl;
    TSDL_UpperBlit = function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_LowerBlit = function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_SoftStretch = function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; const dstrect: PSDL_Surface): SDL_SInt32 cdecl;
    TSDL_BlitSurfaceScaled = function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_UpperBlitScaled = function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32 cdecl;
    TSDL_LowerBlitScaled = function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32 cdecl;
  public
    (**
     *  Allocate and free an RGB surface.
     *
     *  If the depth is 4 or 8 bits, an empty palette is allocated for the surface.
     *  If the depth is greater than 8 bits, the pixel format is set using the
     *  flags '[RGB]mask'.
     *
     *  If the function runs out of memory, it will return NULL.
     *
     *  \param flags The \c flags are obsolete and should be set to 0.
     *  \param width The width in pixels of the surface to create.
     *  \param height The height in pixels of the surface to create.
     *  \param depth The depth in bits of the surface to create.
     *  \param Rmask The red mask of the surface to create.
     *  \param Gmask The green mask of the surface to create.
     *  \param Bmask The blue mask of the surface to create.
     *  \param Amask The alpha mask of the surface to create.
     *)
    SDL_CreateRGBSurface: TSDL_CreateRGBSurface;
    SDL_CreateRGBSurfaceFrom: TSDL_CreateRGBSurfaceFrom;
    SDL_FreeSurface: TSDL_FreeSurface;
    (**
     *  \brief Set the palette used by a surface.
     *
     *  \return 0, or -1 if the surface format doesn't use a palette.
     *
     *  \note A single palette can be shared with many surfaces.
     *)
    SDL_SetSurfacePalette: TSDL_SetSurfacePalette;
    (**
     *  \brief Sets up a surface for directly accessing the pixels.
     *
     *  Between calls to SDL_LockSurface() / SDL_UnlockSurface(), you can write
     *  to and read from \c surface->pixels, using the pixel format stored in
     *  \c surface->format.  Once you are done accessing the surface, you should
     *  use SDL_UnlockSurface() to release it.
     *
     *  Not all surfaces require locking.  If SDL_MUSTLOCK(surface) evaluates
     *  to 0, then you can read and write to the surface at any time, and the
     *  pixel format of the surface will not change.
     *
     *  No operating system or library calls should be made between lock/unlock
     *  pairs, as critical system locks may be held during this time.
     *
     *  SDL_LockSurface() returns 0, or -1 if the surface couldn't be locked.
     *
     *  \sa SDL_UnlockSurface()
     *)
    SDL_LockSurface: TSDL_LockSurface;
    SDL_UnlockSurface: TSDL_UnlockSurface;
    (**
     *  Load a surface from a seekable SDL data stream (memory or file).
     *
     *  If \c freesrc is non-zero, the stream will be closed after being read.
     *
     *  The new surface should be freed with SDL_FreeSurface().
     *
     *  \return the new surface, or NULL if there was an error.
     *)
    SDL_LoadBMP_RW: TSDL_LoadBMP_RW;
    (**
     *  Save a surface to a seekable SDL data stream (memory or file).
     *
     *  If \c freedst is non-zero, the stream will be closed after being written.
     *
     *  \return 0 if successful or -1 if there was an error.
     *)
    SDL_SaveBMP_RW: TSDL_SaveBMP_RW;
    (**
     *  \brief Sets the RLE acceleration hint for a surface.
     *
     *  \return 0 on success, or -1 if the surface is not valid
     *
     *  \note If RLE is enabled, colorkey and alpha blending blits are much faster,
     *        but the surface must be locked before directly accessing the pixels.
     *)
    SDL_SetSurfaceRLE: TSDL_SetSurfaceRLE;
    (**
     *  \brief Sets the color key (transparent pixel) in a blittable surface.
     *
     *  \param surface The surface to update
     *  \param flag Non-zero to enable colorkey and 0 to disable colorkey
     *  \param key The transparent pixel in the native surface format
     *
     *  \return 0 on success, or -1 if the surface is not valid
     *
     *  You can pass SDL_RLEACCEL to enable RLE accelerated blits.
     *)
    SDL_SetColorKey: TSDL_SetColorKey;
    (**
     *  \brief Gets the color key (transparent pixel) in a blittable surface.
     *
     *  \param surface The surface to update
     *  \param key A pointer filled in with the transparent pixel in the native
     *             surface format
     *
     *  \return 0 on success, or -1 if the surface is not valid or colorkey is not
     *          enabled.
     *)
    SDL_GetColorKey: TSDL_GetColorKey;
    (**
     *  \brief Set an additional color value used in blit operations.
     *
     *  \param surface The surface to update.
     *  \param r The red color value multiplied into blit operations.
     *  \param g The green color value multiplied into blit operations.
     *  \param b The blue color value multiplied into blit operations.
     *
     *  \return 0 on success, or -1 if the surface is not valid.
     *
     *  \sa SDL_GetSurfaceColorMod()
     *)
    SDL_SetSurfaceColorMod: TSDL_SetSurfaceColorMod;
    (**
     *  \brief Get the additional color value used in blit operations.
     *
     *  \param surface The surface to query.
     *  \param r A pointer filled in with the current red color value.
     *  \param g A pointer filled in with the current green color value.
     *  \param b A pointer filled in with the current blue color value.
     *
     *  \return 0 on success, or -1 if the surface is not valid.
     *
     *  \sa SDL_SetSurfaceColorMod()
     *)
    SDL_GetSurfaceColorMod: TSDL_GetSurfaceColorMod;
    (**
     *  \brief Set an additional alpha value used in blit operations.
     *
     *  \param surface The surface to update.
     *  \param alpha The alpha value multiplied into blit operations.
     *
     *  \return 0 on success, or -1 if the surface is not valid.
     *
     *  \sa SDL_GetSurfaceAlphaMod()
     *)
    SDL_SetSurfaceAlphaMod: TSDL_SetSurfaceAlphaMod;
    (**
     *  \brief Get the additional alpha value used in blit operations.
     *
     *  \param surface The surface to query.
     *  \param alpha A pointer filled in with the current alpha value.
     *
     *  \return 0 on success, or -1 if the surface is not valid.
     *
     *  \sa SDL_SetSurfaceAlphaMod()
     *)
    SDL_GetSurfaceAlphaMod: TSDL_GetSurfaceAlphaMod;
    (**
     *  \brief Set the blend mode used for blit operations.
     *
     *  \param surface The surface to update.
     *  \param blendMode ::SDL_BlendMode to use for blit blending.
     *
     *  \return 0 on success, or -1 if the parameters are not valid.
     *
     *  \sa SDL_GetSurfaceBlendMode()
     *)
    SDL_SetSurfaceBlendMode: TSDL_SetSurfaceBlendMode;
    (**
     *  \brief Get the blend mode used for blit operations.
     *
     *  \param surface   The surface to query.
     *  \param blendMode A pointer filled in with the current blend mode.
     *
     *  \return 0 on success, or -1 if the surface is not valid.
     *
     *  \sa SDL_SetSurfaceBlendMode()
     *)
    SDL_GetSurfaceBlendMode: TSDL_GetSurfaceBlendMode;
    (**
     *  Sets the clipping rectangle for the destination surface in a blit.
     *
     *  If the clip rectangle is NULL, clipping will be disabled.
     *
     *  If the clip rectangle doesn't intersect the surface, the function will
     *  return SDL_FALSE and blits will be completely clipped.  Otherwise the
     *  function returns SDL_TRUE and blits to the surface will be clipped to
     *  the intersection of the surface area and the clipping rectangle.
     *
     *  Note that blits are automatically clipped to the edges of the source
     *  and destination surfaces.
     *)
    SDL_SetClipRect: TSDL_SetClipRect;
    (**
     *  Gets the clipping rectangle for the destination surface in a blit.
     *
     *  \c rect must be a pointer to a valid rectangle which will be filled
     *  with the correct values.
     *)
    SDL_GetClipRect: TSDL_GetClipRect;
    (**
     *  Creates a new surface of the specified format, and then copies and maps
     *  the given surface to it so the blit of the converted surface will be as
     *  fast as possible.  If this function fails, it returns NULL.
     *
     *  The \c flags parameter is passed to SDL_CreateRGBSurface() and has those
     *  semantics.  You can also pass ::SDL_RLEACCEL in the flags parameter and
     *  SDL will try to RLE accelerate colorkey and alpha blits in the resulting
     *  surface.
     *)
    (* @{ *)
    SDL_ConvertSurface: TSDL_ConvertSurface;
    SDL_ConvertSurfaceFormat: TSDL_ConvertSurfaceFormat;
    (* @} *)
    (**
     * \brief Copy a block of pixels of one format to another format
     *
     *  \return 0 on success, or -1 if there was an error
     *)
    SDL_ConvertPixels: TSDL_ConvertPixels;
    (**
     *  Performs a fast fill of the given rectangle with \c color.
     *
     *  If \c rect is NULL, the whole surface will be filled with \c color.
     *
     *  The color should be a pixel of the format used by the surface, and
     *  can be generated by the SDL_MapRGB() function.
     *
     *  \return 0 on success, or -1 on error.
     *)
    SDL_FillRect: TSDL_FillRect;
    SDL_FillRects: TSDL_FillRects;
    (**
     *  This is the public blit function, SDL_BlitSurface(), and it performs
     *  rectangle validation and clipping before passing it to SDL_LowerBlit()
     *)
    SDL_UpperBlit: TSDL_UpperBlit;
    (**
     *  This is a semi-private blit function and it performs low-level surface
     *  blitting only.
     *)
    SDL_LowerBlit: TSDL_LowerBlit;
    (**
     *  \brief Perform a fast, low quality, stretch blit between two surfaces of the
     *         same pixel format.
     *
     *  \note This function uses a static buffer, and is not thread-safe.
     *)
    SDL_SoftStretch: TSDL_SoftStretch;
    (**
     *  This is the public scaled blit function, SDL_BlitScaled(), and it performs
     *  rectangle validation and clipping before passing it to SDL_LowerBlitScaled()
     *)
    SDL_BlitSurfaceScaled: TSDL_BlitSurfaceScaled;
    (**
     *  This is the public scaled blit function, SDL_BlitScaled(), and it performs
     *  rectangle validation and clipping before passing it to SDL_LowerBlitScaled()
     *)
    SDL_UpperBlitScaled: TSDL_UpperBlitScaled;
    (**
     *  This is a semi-private blit function and it performs low-level surface
     *  scaled blitting only.
     *)
    SDL_LowerBlitScaled: TSDL_LowerBlitScaled;
  strict private
    FRWops: TSDLRwopsApi;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     *  Evaluates to true if the surface needs to be locked before access.
     *)
    function SDL_MUSTLOCK(const S: PSDL_Surface): boolean;
    (**
     *  Load a surface from a file.
     *
     *  Convenience macro.
     *)
    function SDL_LoadBMP(_file: SDL_String): PSDL_Surface;
    (**
     *  Save a surface to a file.
     *
     *  Convenience macro.
     *)
    function SDL_SaveBMP(Const surface:PSDL_Surface; Const filename:AnsiString):SDL_SInt32;
    (**
     *  Performs a fast blit from the source surface to the destination surface.
     *
     *  This assumes that the source and destination rectangles are
     *  the same size.  If either \c srcrect or \c dstrect are NULL, the entire
     *  surface (\c src or \c dst) is copied.  The final blit rectangles are saved
     *  in \c srcrect and \c dstrect after all clipping is performed.
     *
     *  \return If the blit is successful, it returns 0, otherwise it returns -1.
     *
     *  The blit function should not be called on a locked surface.
     *
     *  The blit semantics for surfaces with and without blending and colorkey
     *  are defined as follows:
     *  \verbatim
        RGBA->RGB:
          Source surface blend mode set to SDL_BLENDMODE_BLEND:
            alpha-blend (using the source alpha-channel and per-surface alpha)
            SDL_SRCCOLORKEY ignored.
          Source surface blend mode set to SDL_BLENDMODE_NONE:
            copy RGB.
            if SDL_SRCCOLORKEY set, only copy the pixels matching the
            RGB values of the source color key, ignoring alpha in the
            comparison.

        RGB->RGBA:
          Source surface blend mode set to SDL_BLENDMODE_BLEND:
            alpha-blend (using the source per-surface alpha)
          Source surface blend mode set to SDL_BLENDMODE_NONE:
            copy RGB, set destination alpha to source per-surface alpha value.
          both:
            if SDL_SRCCOLORKEY set, only copy the pixels matching the
            source color key.

        RGBA->RGBA:
          Source surface blend mode set to SDL_BLENDMODE_BLEND:
            alpha-blend (using the source alpha-channel and per-surface alpha)
            SDL_SRCCOLORKEY ignored.
          Source surface blend mode set to SDL_BLENDMODE_NONE:
            copy all of RGBA to the destination.
            if SDL_SRCCOLORKEY set, only copy the pixels matching the
            RGB values of the source color key, ignoring alpha in the
            comparison.

        RGB->RGB:
          Source surface blend mode set to SDL_BLENDMODE_BLEND:
            alpha-blend (using the source per-surface alpha)
          Source surface blend mode set to SDL_BLENDMODE_NONE:
            copy RGB.
          both:
            if SDL_SRCCOLORKEY set, only copy the pixels matching the
            source color key.
        \endverbatim
     *
     *  You should call SDL_BlitSurface() unless you know exactly how SDL
     *  blitting works internally and how to use the other blit functions.
     *)
    function SDL_BlitSurface(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32;

    function SDL_GetPixet(const Surface: PSDL_Surface; X, Y: Integer; out Pixel: SDL_SInt32): Boolean;
  end;

implementation

{ TSDLSurfaceApi }

procedure TSDLSurfaceApi.DoInit;
begin
  inherited DoInit;

  FRWops :=  TSDLRwopsApi.Create(Owner);
  FRWops.Init;

  if not FRWops.Valid then
    LastError := LastError + FRWops.LastError + LineEnding;
end;

procedure TSDLSurfaceApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_CreateRGBSurface', @SDL_CreateRGBSurface);
  List.Add('SDL_CreateRGBSurfaceFrom', @SDL_CreateRGBSurfaceFrom);
  List.Add('SDL_FreeSurface', @SDL_FreeSurface);
  List.Add('SDL_SetSurfacePalette', @SDL_SetSurfacePalette);
  List.Add('SDL_LockSurface', @SDL_LockSurface);
  List.Add('SDL_UnlockSurface', @SDL_UnlockSurface);
  List.Add('SDL_LoadBMP_RW', @SDL_LoadBMP_RW);
  List.Add('SDL_SaveBMP_RW', @SDL_SaveBMP_RW);
  List.Add('SDL_SetSurfaceRLE', @SDL_SetSurfaceRLE);
  List.Add('SDL_SetColorKey', @SDL_SetColorKey);
  List.Add('SDL_GetColorKey', @SDL_GetColorKey);
  List.Add('SDL_SetSurfaceColorMod', @SDL_SetSurfaceColorMod);
  List.Add('SDL_GetSurfaceColorMod', @SDL_GetSurfaceColorMod);
  List.Add('SDL_SetSurfaceAlphaMod', @SDL_SetSurfaceAlphaMod);
  List.Add('SDL_GetSurfaceAlphaMod', @SDL_GetSurfaceAlphaMod);
  List.Add('SDL_SetSurfaceBlendMode', @SDL_SetSurfaceBlendMode);
  List.Add('SDL_GetSurfaceBlendMode', @SDL_GetSurfaceBlendMode);
  List.Add('SDL_SetClipRect', @SDL_SetClipRect);
  List.Add('SDL_GetClipRect', @SDL_GetClipRect);
  List.Add('SDL_ConvertSurface', @SDL_ConvertSurface);
  List.Add('SDL_ConvertSurfaceFormat', @SDL_ConvertSurfaceFormat);
  List.Add('SDL_ConvertPixels', @SDL_ConvertPixels);
  List.Add('SDL_FillRect', @SDL_FillRect);
  List.Add('SDL_FillRects', @SDL_FillRects);
  List.Add('SDL_UpperBlit', @SDL_UpperBlit);
  List.Add('SDL_LowerBlit', @SDL_LowerBlit);
  List.Add('SDL_SoftStretch', @SDL_SoftStretch);
  List.Add('SDL_UpperBlitScaled', @SDL_BlitSurfaceScaled);
  List.Add('SDL_UpperBlitScaled', @SDL_UpperBlitScaled);
  List.Add('SDL_LowerBlitScaled', @SDL_LowerBlitScaled);
end;

function TSDLSurfaceApi.SDL_MUSTLOCK(const S: PSDL_Surface): boolean;
begin
  Result := ((S^.flags.Value and Ord(SDL_RLEACCEL)) <> 0)
end;

function TSDLSurfaceApi.SDL_LoadBMP(_file: SDL_String): PSDL_Surface;
begin
  Result := SDL_LoadBMP_RW(FRWops.SDL_RWFromFile(_file, 'rb'), 1);
end;

function TSDLSurfaceApi.SDL_SaveBMP(const surface: PSDL_Surface;
  const filename: AnsiString): SDL_SInt32;
begin
   Result := SDL_SaveBMP_RW(surface, FRWops.SDL_RWFromFile(SDL_String(filename), 'wb'), 1)
end;

function TSDLSurfaceApi.SDL_BlitSurface(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): SDL_SInt32;
begin
  Result := SDL_UpperBlit(src, srcrect, dst, dstrect)
end;

function TSDLSurfaceApi.SDL_GetPixet(const Surface: PSDL_Surface; X, Y: Integer; out Pixel: SDL_SInt32): Boolean;
var
  Pos: Pointer;
begin
  Result := True;
  Pos := Pointer(Surface^.pixels + (y * Surface^.w + x) * Surface^.format^.BytesPerPixel);

  case Surface^.format^.BytesPerPixel of
    1: Pixel := PByte(Pos)^;
    2: Pixel := PWord(Pos)^;
    3: Pixel := (PByte(Pos)^ shl 16) or (PByte(Pos + 1)^ shl 8) or PByte(Pos + 2)^;
    4: Pixel := PInteger(Pos)^
  else
    begin
      Result := False;
      Pixel := 0;
    end;
  end;
end;

end.
