unit SDL.Api.Touch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLTouchApi }

  TSDLTouchApi = class(TSubLibrary)
  private type
    TSDL_GetNumTouchDevices = function: SDL_SInt32 cdecl;
    TSDL_GetTouchDevice = function(index: SDL_SInt32): SDL_TouchID cdecl;
    TSDL_GetNumTouchFingers = function(touchID: SDL_TouchID): SDL_SInt32 cdecl;
    TSDL_GetTouchFinger = function(touchID: SDL_TouchID; index: SDL_SInt32): PSDL_Finger cdecl;
  public
    (**
     *  \brief Get the number of registered touch devices.
     *)
    SDL_GetNumTouchDevices: TSDL_GetNumTouchDevices;
    (**
     *  \brief Get the touch ID with the given index, or 0 if the index is invalid.
     *)
    SDL_GetTouchDevice: TSDL_GetTouchDevice;
    (**
     *  \brief Get the number of active fingers for a given touch device.
     *)
    SDL_GetNumTouchFingers: TSDL_GetNumTouchFingers;
    (**
     *  \brief Get the finger object of the given touch, with the given index.
     *)
    SDL_GetTouchFinger: TSDL_GetTouchFinger;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLTouchApi }

procedure TSDLTouchApi.DoInit;
begin

end;

procedure TSDLTouchApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetNumTouchDevices', @SDL_GetNumTouchDevices);
  List.Add('SDL_GetTouchDevice', @SDL_GetTouchDevice);
  List.Add('SDL_GetNumTouchFingers', @SDL_GetNumTouchFingers);
  List.Add('SDL_GetTouchFinger', @SDL_GetTouchFinger);
end;

end.

