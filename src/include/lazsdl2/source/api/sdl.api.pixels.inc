

type

  PSDL_Color = ^SDL_Color;
  PSDL_Palette = ^SDL_Palette;
  PSDL_PixelFormat = ^SDL_PixelFormat;

  (** Pixel type. *)
  SDL_PixelType = (
    SDL_PIXELTYPE_UNKNOWN,
    SDL_PIXELTYPE_INDEX1,
    SDL_PIXELTYPE_INDEX4,
    SDL_PIXELTYPE_INDEX8,
    SDL_PIXELTYPE_PACKED8,
    SDL_PIXELTYPE_PACKED16,
    SDL_PIXELTYPE_PACKED32,
    SDL_PIXELTYPE_ARRAYU8,
    SDL_PIXELTYPE_ARRAYU16,
    SDL_PIXELTYPE_ARRAYU32,
    SDL_PIXELTYPE_ARRAYF16,
    SDL_PIXELTYPE_ARRAYF32
  );

  (** Bitmap pixel order, high bit -> low bit. *)
  SDL_BitmapPixelOrder = (
    SDL_BITMAPORDER_NONE,
    SDL_BITMAPORDER_4321,
    SDL_BITMAPORDER_1234
  );

  (** Packed component order, high bit -> low bit. *)
  SDL_PackedComponentOrder = (
    SDL_PACKEDORDER_NONE,
    SDL_PACKEDORDER_XRGB,
    SDL_PACKEDORDER_RGBX,
    SDL_PACKEDORDER_ARGB,
    SDL_PACKEDORDER_RGBA,
    SDL_PACKEDORDER_XBGR,
    SDL_PACKEDORDER_BGRX,
    SDL_PACKEDORDER_ABGR,
    SDL_PACKEDORDER_BGRA
  );

  (** Array component order, low byte -> high byte. *)
  (* !!! FIXME: in 2.1, make these not overlap differently with
     !!! FIXME:  SDL_PACKEDORDER_*, so we can simplify SDL_ISPIXELFORMAT_ALPHA *)
  SDL_ArrayComponentOrder = (
    SDL_ARRAYORDER_NONE,
    SDL_ARRAYORDER_RGB,
    SDL_ARRAYORDER_RGBA,
    SDL_ARRAYORDER_ARGB,
    SDL_ARRAYORDER_BGR,
    SDL_ARRAYORDER_BGRA,
    SDL_ARRAYORDER_ABGR
  );

  (** Packed component layout. *)
  SDL_PackedComponentLayout = (
      SDL_PACKEDLAYOUT_NONE,
      SDL_PACKEDLAYOUT_332,
      SDL_PACKEDLAYOUT_4444,
      SDL_PACKEDLAYOUT_1555,
      SDL_PACKEDLAYOUT_5551,
      SDL_PACKEDLAYOUT_565,
      SDL_PACKEDLAYOUT_8888,
      SDL_PACKEDLAYOUT_2101010,
      SDL_PACKEDLAYOUT_1010102
  );

  SDL_PixelFormatEnum = (
    SDL_PIXELFORMAT_UNKNOWN = 0,
    SDL_PIXELFORMAT_INDEX1LSB = (1 shl 28)                    or
                                (Integer(SDL_PIXELTYPE_INDEX1) shl 24) or
                                (Integer(SDL_BITMAPORDER_4321) shl 20) or
                                (0 shl 16)                    or
                                (1 shl 8)                     or
                                (0 shl 0),
    SDL_PIXELFORMAT_INDEX1MSB = (1 shl 28)                    or
                                (Integer(SDL_PIXELTYPE_INDEX1) shl 24) or
                                (Integer(SDL_BITMAPORDER_1234) shl 20) or
                                (0 shl 16)                    or
                                (1 shl 8)                     or
                                (0 shl 0),
    SDL_PIXELFORMAT_INDEX4LSB = (1 shl 28)                    or
                                (Integer(SDL_PIXELTYPE_INDEX4) shl 24) or
                                (Integer(SDL_BITMAPORDER_4321) shl 20) or
                                (0 shl 16)                    or
                                (4 shl 8)                     or
                                (0 shl 0),
    SDL_PIXELFORMAT_INDEX4MSB = (1 shl 28)                    or
                                (Integer(SDL_PIXELTYPE_INDEX4) shl 24) or
                                (Integer(SDL_BITMAPORDER_1234) shl 20) or
                                (0 shl 16)                    or
                                (4 shl 8)                     or
                                (0 shl 0),
    SDL_PIXELFORMAT_INDEX8 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED8) shl 24)  or
                                (0 shl 20)                      or
                                (0 shl 16)                      or
                                (8 shl 8)                       or
                                (1 shl 0),
    SDL_PIXELFORMAT_RGB332 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED8) shl 24)  or
                                (Integer(SDL_PACKEDORDER_XRGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_332) shl 16)   or
                                (8 shl 8)                       or
                                (1 shl 0),
    SDL_PIXELFORMAT_RGB444 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_XRGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_4444) shl 16)  or
                                (12 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_RGB555 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_XRGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_1555) shl 16)  or
                                (15 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_BGR555 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_XBGR) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_1555) shl 16)  or
                                (15 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_ARGB4444 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_ARGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_4444) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0){%H-},
    SDL_PIXELFORMAT_RGBA4444 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_RGBA) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_4444) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_ABGR4444 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_ABGR) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_4444) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_BGRA4444 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_BGRA) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_4444) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_ARGB1555 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_ARGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_1555) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_RGBA5551 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_RGBA) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_5551) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_ABGR1555 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_ABGR) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_1555) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_BGRA5551 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_BGRA) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_5551) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_RGB565 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_XRGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_565) shl 16)   or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_BGR565 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED16) shl 24) or
                                (Integer(SDL_PACKEDORDER_XBGR) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_1555) shl 16)  or
                                (16 shl 8)                      or
                                (2 shl 0),
    SDL_PIXELFORMAT_RGB24 =     (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_ARRAYU8) shl 24)  or
                                (Integer(SDL_ARRAYORDER_RGB) shl 20)     or
                                (0 shl 16)                      or
                                (24 shl 8)                      or
                                (3 shl 0),
    SDL_PIXELFORMAT_BGR24 =     (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_ARRAYU8) shl 24)  or
                                (Integer(SDL_ARRAYORDER_BGR) shl 20)     or
                                (0 shl 16)                      or
                                (24 shl 8)                      or
                                (3 shl 0),
    SDL_PIXELFORMAT_RGB888 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_XRGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_RGBX8888 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_RGBX) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_BGR888 =    (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_XBGR) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_BGRX8888 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_BGRX) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (24 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_ARGB8888 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_ARGB) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_RGBA8888 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_RGBA) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_ABGR8888 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_ABGR) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_BGRA8888 =  (1 shl 28)                      or
                                (Integer(SDL_PIXELTYPE_PACKED32) shl 24) or
                                (Integer(SDL_PACKEDORDER_RGBX) shl 20)   or
                                (Integer(SDL_PACKEDLAYOUT_8888) shl 16)  or
                                (32 shl 8)                      or
                                (4 shl 0),
    SDL_PIXELFORMAT_ARGB2101010 = (1 shl 28)                       or
                                  (Integer(SDL_PIXELTYPE_PACKED32) shl 24)  or
                                  (Integer(SDL_PACKEDORDER_ARGB) shl 20)    or
                                  (Integer(SDL_PACKEDLAYOUT_2101010) shl 16)or
                                  (32 shl 8)                       or
                                  (4 shl 0),
    SDL_PIXELFORMAT_YV12 = (Integer('Y')       ) or
                           (Integer('V') shl  8) or
                           (Integer('1') shl 16) or
                           (Integer('2') shl 24), (** Planar mode: Y + V + U  (3 planes) *)
    SDL_PIXELFORMAT_IYUV = (Integer('I')       ) or
                           (Integer('Y') shl  8) or
                           (Integer('U') shl 16) or
                           (Integer('V') shl 24), (** Planar mode: Y + U + V  (3 planes) *)
    SDL_PIXELFORMAT_YUY2 = (Integer('Y')       ) or
                           (Integer('U') shl  8) or
                           (Integer('Y') shl 16) or
                           (Integer('2') shl 24), (** Packed mode: Y0+U0+Y1+V0 (1 plane) *)
    SDL_PIXELFORMAT_UYVY = (Integer('U')       ) or
                           (Integer('Y') shl  8) or
                           (Integer('V') shl 16) or
                           (Integer('Y') shl 24), (** Packed mode: U0+Y0+V0+Y1 (1 plane) *)
    SDL_PIXELFORMAT_YVYU = (Integer('Y')       ) or
                           (Integer('V') shl  8) or
                           (Integer('Y') shl 16) or
                           (Integer('U') shl 24), (** Packed mode: Y0+V0+Y1+U0 (1 plane) *)
    SDL_PIXELFORMAT_NV12 = (Integer('N')       ) or
                           (Integer('V') shl  8) or
                           (Integer('1') shl 16) or
                           (Integer('2') shl 24), (** Planar mode: Y + U/V interleaved  (2 planes) *)
    SDL_PIXELFORMAT_NV21 = (Integer('N')       ) or
                           (Integer('V') shl  8) or
                           (Integer('2') shl 16) or
                           (Integer('1') shl 24)  (** Planar mode: Y + V/U interleaved  (2 planes) *)
  );

  { SDL_Color }

  SDL_Color = record
    r: SDL_UInt8;
    g: SDL_UInt8;
    b: SDL_UInt8;
    a: SDL_UInt8;
    class function Make(const Rv, Gv, Bv, Av: Byte): SDL_Color; static;
  end;

  SDL_Palette = record
    ncolors: SDL_SInt32;
    colors: PSDL_Color;
    version: SDL_UInt32;
    refcount: SDL_SInt32;
  end;

  (**
   *  \note Everything in the pixel format structure is read-only.
   *)
  SDL_PixelFormat = record
    format: SDL_PixelFormatEnum;
    palette: PSDL_Palette;
    BitsPerPixel: SDL_UInt8;
    BytesPerPixel: SDL_UInt8;
    padding: array[0..1] of SDL_UInt8;
    Rmask: SDL_UInt32;
    Gmask: SDL_UInt32;
    Bmask: SDL_UInt32;
    Amask: SDL_UInt32;
    Rloss: SDL_UInt8;
    Gloss: SDL_UInt8;
    Bloss: SDL_UInt8;
    Aloss: SDL_UInt8;
    Rshift: SDL_UInt8;
    Gshift: SDL_UInt8;
    Bshift: SDL_UInt8;
    Ashift: SDL_UInt8;
    refcount: SDL_SInt32;
    next: PSDL_PixelFormat;
  end;

