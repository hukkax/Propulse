

type

  PSDL_Renderer = ^SDL_Renderer;
  PSDL_Texture = ^SDL_Texture;
  PSDL_RendererInfo = ^SDL_RendererInfo;

  (**
   *  \brief Flags used when creating a rendering context
   *)
  SDL_RendererFlag = (
    SDL_RENDERER_SOFTWARE = $00000001,         (** The renderer is a software fallback *)
    SDL_RENDERER_ACCELERATED = $00000002,      (** The renderer uses hardware acceleration *)
    SDL_RENDERER_PRESENTVSYNC = $00000004,     (** Present is synchronized with the refresh rate *)
    SDL_RENDERER_TARGETTEXTURE = $00000008     (** The renderer supports rendering to texture *)
  );

  (**
   *  \brief The access pattern allowed for a texture.
   *)
  SDL_TextureAccess = (
    SDL_TEXTUREACCESS_STATIC,    (** Changes rarely, not lockable *)
    SDL_TEXTUREACCESS_STREAMING, (** Changes frequently, lockable *)
    SDL_TEXTUREACCESS_TARGET     (** Texture can be used as a render target *)
  );

  (**
   *  \brief The texture channel modulation used in SDL_RenderCopy().
   *)
  SDL_TextureModulate = (
    SDL_TEXTUREMODULATE_NONE = $00000000,     (** No modulation *)
    SDL_TEXTUREMODULATE_COLOR = $00000001,    (** srcC = srcC * color *)
    SDL_TEXTUREMODULATE_ALPHA = $00000002     (** srcA = srcA * alpha *)
  );

  (**
   *  \brief Flip constants for SDL_RenderCopyEx
   *)
  SDL_RendererFlip = (
    SDL_FLIP_NONE = $00000000,     (** Do not flip *)
    SDL_FLIP_HORIZONTAL = $00000001,    (** flip horizontally *)
    SDL_FLIP_VERTICAL = $00000002     (** flip vertically *)
  );

  SDL_RendererFlags = record Value: SDL_UInt32 end;

  (**
   *  \brief Information on the capabilities of a render driver or context.
   *)
  SDL_RendererInfo = record
    name: SDL_String;                         (** The name of the renderer *)
    flags: SDL_RendererFlags;                 (** Supported ::SDL_RendererFlags *)
    num_texture_formats: SDL_UInt32;          (** The number of available texture formats *)
    texture_formats: array [0..15] of Uint32; (** The available texture formats *)
    max_texture_width: SDL_SInt32;            (** The maximum texture width *)
    max_texture_height: SDL_SInt32;           (** The maximum texture height *)
  end;

  (**
   *  \brief A structure representing rendering state
   *)
  SDL_Renderer = record end;

  (**
   *  \brief An efficient driver-specific representation of pixel data
   *)
  SDL_Texture = record end;

