(**
 *  \file SDL_power.h
 *
 *  Header for the SDL power management routines.
 *)
unit SDL.Api.Power;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLPowerApi }

  TSDLPowerApi = class(TSubLibrary)
  private type
    TSDL_GetPowerInfo = function(secs: PSDL_SInt32; pct: PSDL_SInt32): SDL_PowerState cdecl;
  public
    (**
     *  \brief Get the current power supply details.
     *
     *  \param secs Seconds of battery life left. You can pass a NULL here if
     *              you don't care. Will return -1 if we can't determine a
     *              value, or we're not running on a battery.
     *
     *  \param pct Percentage of battery life left, between 0 and 100. You can
     *             pass a NULL here if you don't care. Will return -1 if we
     *             can't determine a value, or we're not running on a battery.
     *
     *  \return The state of the battery (if any).
     *)
    SDL_GetPowerInfo: TSDL_GetPowerInfo;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    function SDL_GetPowerInfoSecs: SDL_SInt32;
    function SDL_GetPowerInfoPct: SDL_SInt32;
    function SDL_GetPowerInfoState: SDL_PowerState;
  end;

implementation

{ TSDLPowerApi }

procedure TSDLPowerApi.DoInit;
begin

end;

procedure TSDLPowerApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetPowerInfo', @SDL_GetPowerInfo);
end;

function TSDLPowerApi.SDL_GetPowerInfoSecs: SDL_SInt32;
begin
  SDL_GetPowerInfo(@Result, nil)
end;

function TSDLPowerApi.SDL_GetPowerInfoPct: SDL_SInt32;
begin
  SDL_GetPowerInfo(nil, @Result)
end;

function TSDLPowerApi.SDL_GetPowerInfoState: SDL_PowerState;
begin
  Result := SDL_GetPowerInfo(nil, nil)
end;

end.

