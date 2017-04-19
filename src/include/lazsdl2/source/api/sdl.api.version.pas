unit SDL.Api.Version;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary,
  SDL.Api.Options;

type

  { TSDLVersionApi }

  TSDLVersionApi = class(TSubLibrary)
  private type
    TSDL_GetVersion = procedure(out ver: SDL_Version) cdecl;
    TSDL_GetRevision = function: SDL_String cdecl;
    TSDL_GetRevisionNumber = function: SDL_SInt32 cdecl;
  public
    (**
     *  \brief Get the version of SDL that is linked against your program.
     *
     *  If you are linking to SDL dynamically, then it is possible that the
     *  current version will be different than the version you compiled against.
     *  This function returns the current version, while SDL_VERSION() is a
     *  macro that tells you what version you compiled with.
     *
     *  \code
     *  SDL_version compiled;
     *  SDL_version linked;
     *
     *  SDL_VERSION(&compiled);
     *  SDL_GetVersion(&linked);
     *  printf("We compiled against SDL version %d.%d.%d ...\n",
     *         compiled.major, compiled.minor, compiled.patch);
     *  printf("But we linked against SDL version %d.%d.%d.\n",
     *         linked.major, linked.minor, linked.patch);
     *  \endcode
     *
     *  This function may be called safely at any time, even before SDL_Init().
     *
     *  \sa SDL_VERSION
     *)
    SDL_GetVersion: TSDL_GetVersion;
    (**
     *  \brief Get the code revision of SDL that is linked against your program.
     *
     *  Returns an arbitrary string (a hash value) uniquely identifying the
     *  exact revision of the SDL library in use, and is only useful in comparing
     *  against other revisions. It is NOT an incrementing number.
     *)
    SDL_GetRevision: TSDL_GetRevision;
    (**
     *  \brief Get the revision number of SDL that is linked against your program.
     *
     *  Returns a number uniquely identifying the exact revision of the SDL
     *  library in use. It is an incrementing number based on commits to
     *  hg.libsdl.org.
     *)
    SDL_GetRevisionNumber: TSDL_GetRevisionNumber;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     *  \brief Macro to determine SDL version program was compiled against.
     *
     *  This macro fills in a SDL_version structure with the version of the
     *  library you compiled against. This is determined by what header the
     *  compiler uses. Note that if you dynamically linked the library, you might
     *  have a slightly newer or older version at runtime. That version can be
     *  determined with SDL_GetVersion(), which, unlike SDL_VERSION(),
     *  is not a macro.
     *
     *  \param Ver A pointer to a SDL_version struct to initialize.
     *
     *  \sa SDL_version
     *  \sa SDL_GetVersion
     *)
    procedure SDL_VERSION(Ver: PSDL_Version);
    (**
     *  This macro turns the version numbers into a numeric value:
     *  \verbatim
        (1,2,3) -> (1203)
        \endverbatim
     *
     *  This assumes that there will never be more than 100 patchlevels.
     *)
    function SDL_VERSIONNUM(const Major, Minor, Patch: UInt8): SDL_UInt32;
    (**
     *  This is the version number macro for the current SDL version.
     *)
    function SDL_COMPILEDVERSION: SDL_UInt32;
    (**
     *  This macro will evaluate to true if compiled with SDL at least X.Y.Z.
     *)
    function SDL_VERSION_ATLEAST(const Major, Minor, Patch: UInt8): Boolean;
  end;

implementation

{ TSDLVersionApi }

procedure TSDLVersionApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetVersion', @SDL_GetVersion);
  List.Add('SDL_GetRevision', @SDL_GetRevision);
  List.Add('SDL_GetRevisionNumber', @SDL_GetRevisionNumber);
end;

procedure TSDLVersionApi.SDL_VERSION(Ver: PSDL_Version);
begin
  Ver^.major := SDLOptions.SDL_MAJOR_VERSION;
  Ver^.minor := SDLOptions.SDL_MINOR_VERSION;
  Ver^.patch := SDLOptions.SDL_PATCHLEVEL
end;

function TSDLVersionApi.SDL_VERSIONNUM(const Major, Minor, Patch: UInt8
  ): SDL_UInt32;
begin
  Result := Major * 1000 + Minor * 100 + Patch
end;

function TSDLVersionApi.SDL_COMPILEDVERSION: SDL_UInt32;
begin
  Result := SDL_VERSIONNUM(
    SDLOptions.SDL_MAJOR_VERSION,
    SDLOptions.SDL_MINOR_VERSION,
    SDLOptions.SDL_PATCHLEVEL
  )
end;

function TSDLVersionApi.SDL_VERSION_ATLEAST(const Major, Minor, Patch: UInt8
  ): Boolean;
begin
  Result := SDL_COMPILEDVERSION >= SDL_VERSIONNUM(Major, Minor, Patch);
end;

end.

