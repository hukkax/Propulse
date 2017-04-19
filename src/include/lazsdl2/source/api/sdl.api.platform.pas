unit SDL.Api.Platform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLPlatformApi }

  TSDLPlatformApi = class(TSubLibrary)
  private type
    TSDL_GetPlatform = function(): SDL_String cdecl;
  public
    (**
     *  \brief Gets the name of the platform.
     *
     *  \note Here are the names returned for some supported platforms:
     *    -Windows
     *    - Mac OS X
     *    - Linux
     *    - iOS
     *    - Android
     *)
    SDL_GetPlatform: TSDL_GetPlatform;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLPlatformApi }

procedure TSDLPlatformApi.DoInit;
begin

end;

procedure TSDLPlatformApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetPlatform', @SDL_GetPlatform);
end;

end.

