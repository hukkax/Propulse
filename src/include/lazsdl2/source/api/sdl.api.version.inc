

type

  PSDL_Version = ^SDL_Version;

  (**
   *  \brief Information the version of SDL in use.
   *
   *  Represents the library's version as three levels: major revision
   *  (increments with massive changes, additions, and enhancements),
   *  minor revision (increments with backwards-compatible changes to the
   *  major revision), and patchlevel (increments with fixes to the minor
   *  revision).
   *
   *  \sa SDL_VERSION
   *  \sa SDL_GetVersion
   *)
  SDL_Version = record
    major: SDL_UInt8;  (** major version *)
    minor: SDL_UInt8;  (** minor version *)
    patch: SDL_UInt8;  (** update version *)
  end;

