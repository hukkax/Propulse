unit SDL.Extended.LibraryProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DLibrary,
  SDL.Api.Types;

type

  TLibraryPathList = class(TStringList)
  end;

  { TSDLCustomLibraryProvider }

  TSDLCustomLibraryProvider = class(TComponent)
  private type

    { TLibEntry }

    TLibEntry = class
    private
      FShared: Boolean;
      FLib: TAbstractLibrary;
    public
      constructor Create(const ALib: TAbstractLibrary; AShared: Boolean); reintroduce;
      procedure BeforeDestruction; override;

      property Lib: TAbstractLibrary read FLib write FLib;
      property Shared: Boolean read FShared write FShared;
    end;

  private
    FActive: Boolean;
    FLibs: TStringList;
    FPaths: TLibraryPathList;
    function GetLibs(LibName: String): TAbstractLibrary;
    procedure SetActive(AValue: Boolean);
    procedure SetLibs(LibName: String; AValue: TAbstractLibrary);
  protected
    procedure LoadObject; virtual; abstract;
    procedure UnLoadObject; virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Close;
    procedure Open;

    property Active: Boolean read FActive write SetActive; (** Load/Unload Lib object if used external object this is always true *)
    property Paths: TLibraryPathList read FPaths write FPaths; (** All paths to libraries if empty the default will be used *)
    property Libs[LibName: String]: TAbstractLibrary read GetLibs write SetLibs; (** All libraries avaible to provider by name *)
  end;

  TSDLInitOption = (sdlTimer, sdlAudio, sdlVideo, sdlJoystick, sdlHaptic, sdlGameControler, sdlEvents);
  TSDLInitOptions = set of TSDLInitOption;

  { TSDLLibraryProvider }

  TSDLLibraryProvider = class(TSDLCustomLibraryProvider)
  private
    FInitOptions: TSDLInitOptions;
    procedure SetInitOptions(AValue: TSDLInitOptions);
  protected
    procedure LoadObject; override;
    procedure UnLoadObject; override;
  published
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;

    property InitOptions: TSDLInitOptions read FInitOptions write SetInitOptions;
    property Active;
    property Paths;
  end;

implementation

uses
  SDL.Api.libSDL2, SDL.Api.libSDL_Image;

{ TSDLCustomLibraryProvider.TLibEntry }

constructor TSDLCustomLibraryProvider.TLibEntry.Create(
  const ALib: TAbstractLibrary; AShared: Boolean);
begin
  FLib := ALib;
  FShared := AShared;
end;

procedure TSDLCustomLibraryProvider.TLibEntry.BeforeDestruction;
begin
  if not FShared then
    FLib.Free;

  inherited BeforeDestruction;
end;

{ TSDLLibraryProvider }

procedure TSDLLibraryProvider.SetInitOptions(AValue: TSDLInitOptions);
begin
  if FInitOptions=AValue then Exit;
  FInitOptions:=AValue;
end;

procedure TSDLLibraryProvider.LoadObject;
var
  SDL2: TSDL2Library;
  SDL_Image: TSDL2_ImageLibrary;
  NewLib: TLibrary;
  LibName, LibPath: String;
  InitFlags: SDL_InitFlags;
  i: Integer;
begin
  for i := 0 to FLibs.Count - 1 do
  begin
    FLibs.GetNameValue(i, LibName, LibPath);

    //Do not load library if user have alredy loaded it
    if Assigned(TLibEntry(FLibs.Objects[i]).Lib) then
      Continue;

    NewLib := nil;
    try
      if LibName = 'SDL2' then
      begin
        SDL2 := TSDL2Library.Create(LibPath);
        NewLib := SDL2;
        SDL2.Init;

        if not SDL2.Valid then
          raise SDLCriticalException.Create(SDL2.LastError);

        InitFlags := SDL_INIT_NOTHING;

        if sdlTimer in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_TIMER;

        if sdlAudio in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_AUDIO;

        if sdlVideo in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_VIDEO;

        if sdlJoystick in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_JOYSTICK;

        if sdlHaptic in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_HAPTIC;

        if sdlGameControler in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_GAMECONTROLLER;

        if sdlEvents in FInitOptions then
          InitFlags := InitFlags or SDL_INIT_EVENTS;

        if not SDL2.SDL_Init(InitFlags) then
          raise SDLCriticalException.Create(SDL2.LastError);
      end
      else
      if LibName = 'SDL_Image' then
      begin
        SDL_Image := TSDL2_ImageLibrary.Create(LibPath, Libs['SDL2'] as TSDL2Library);
        NewLib := SDL_Image;
        SDL_Image.Init;

        {if not SDL_Image.Valid then
          raise SDLCriticalException.Create(SDL_Image.LastError);

        if not SDL_Image.IMG_Init() then
          raise SDLCriticalException.Create(SDL_Image.LastError); }
      end
      else
      begin
        NewLib := TLibrary.Create(LibPath);
        NewLib.Init;
        if not NewLib.Valid then
          raise SDLException.Create(SDL2.LastError);
      end
    except
      on Exception do
      begin
        if Assigned(NewLib) then
          NewLib.Free;
        raise
      end;
    end;

    TLibEntry(FLibs.Objects[i]).Lib := NewLib;
    TLibEntry(FLibs.Objects[i]).Shared := False
  end;
end;

procedure TSDLLibraryProvider.UnLoadObject;
begin
  FLibs.Clear;
end;

constructor TSDLLibraryProvider.Create(AOwner: TComponent);
begin
  FInitOptions := [sdlTimer, sdlAudio, sdlVideo, sdlJoystick, sdlHaptic, sdlGameControler, sdlEvents];
  inherited Create(AOwner);
end;

procedure TSDLLibraryProvider.AfterConstruction;
begin
  inherited AfterConstruction;
  Libs['SDL2'] := nil;
  Libs['SDL_Image'] := nil;
end;

{ TSDLCustomLibraryProvider }

procedure TSDLCustomLibraryProvider.SetActive(AValue: Boolean);
begin
  if (FActive=AValue) then
    Exit;

  FActive:=AValue;

  if not (csDesigning in ComponentState) then
  begin
    //External libraries must be initialized and deinitialized externaly
    if FActive then
      LoadObject
    else
      UnLoadObject
  end;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure TSDLCustomLibraryProvider.SetLibs(LibName: String;
  AValue: TAbstractLibrary);
var
  Index: Integer;
  OldLib: TAbstractLibrary;
begin
  Index := FLibs.IndexOfName(LibName);

  if Index = -1 then
  begin
    FLibs.AddObject(LibName + '=', TLibEntry.Create(AValue, True));
    Exit;
  end;

  OldLib := Libs[LibName];
  if Index > -1 then
    FLibs.Delete(Index);

  FLibs.InsertObject(Index, LibName + '=', TLibEntry.Create(AValue, True));

  OldLib.Free;

  FPONotifyObservers(Self, ooChange, nil);
end;

function TSDLCustomLibraryProvider.GetLibs(LibName: String): TAbstractLibrary;
var
  Index: Integer;
begin
  Index := FLibs.IndexOfName(LibName);

  if Index > -1 then
    Result := (FLibs.Objects[Index] as TLibEntry).Lib
  else
    Result := nil
end;

procedure TSDLCustomLibraryProvider.AfterConstruction;
begin
  FPaths := TLibraryPathList.Create;
  FLibs := TStringList.Create;
  FLibs.OwnsObjects := True;

  inherited AfterConstruction;
end;

procedure TSDLCustomLibraryProvider.BeforeDestruction;
begin
  Active := False;
  FPaths.Free;
  FLibs.Free;

  inherited BeforeDestruction;
end;

procedure TSDLCustomLibraryProvider.Open;
begin
  Active := True
end;

procedure TSDLCustomLibraryProvider.Close;
begin
  Active := False
end;

end.
