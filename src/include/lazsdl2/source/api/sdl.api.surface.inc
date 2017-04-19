
type

  PSDL_Surface = ^SDL_Surface;

  (**
   *  \name Surface flags
   *
   *  These are the currently supported flags for the ::SDL_Surface.
   *
   *  \internal
   *  Used internally (read-only).
   *)
  SDL_Surface_Flag = (
    SDL_SWSURFACE = 0,          (** Just here for compatibility *)
    SDL_PREALLOC  = $00000001,  (** Surface uses preallocated memory *)
    SDL_RLEACCEL  = $00000002,  (** Surface is RLE encoded *)
    SDL_DONTFREE  = $00000004  (** Surface is referenced internally *)
  );

  SDL_Surface_Flags = record Value: SDL_UInt32; end;

  (**
   * \brief A collection of pixels used in software blitting.
   *
   * \note  This structure should be treated as read-only, except for \c pixels,
   *        which, if not NULL, contains the raw pixel data for the surface.
   *)
  SDL_Surface = record
    flags: SDL_Surface_Flags;   {**< Read-only *}
    format: PSDL_PixelFormat;   {**< Read-only *}
    w, h: SDL_SInt32;           {**< Read-only *}
    pitch: SDL_SInt32;          {**< Read-only *}
    pixels: Pointer;            {**< Read-write *}

    {** Application data associated with the surface *}
    userdata: Pointer;          {**< Read-write *}

    {** information needed for surfaces requiring locks *}
    locked: SDL_SInt32;         {**< Read-only *}
    lock_data: Pointer;         {**< Read-only *}

    {** clipping information *}
    clip_rect: PSDL_Rect;       {**< Read-only *}

    {** info for fast blit mapping to other surfaces *}
    map: Pointer;               {**< Private *} //SDL_BlitMap

    {** Reference count -- used when freeing surface *}
    refcount: SDL_SInt32;       {**< Read-mostly *}
  end;
