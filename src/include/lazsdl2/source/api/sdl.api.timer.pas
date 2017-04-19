unit SDL.Api.Timer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLTimerApi }

  TSDLTimerApi = class(TSubLibrary)
  public type
    TSDL_TimerCallback = function(interval: SDL_UInt32; param: SDL_Data): SDL_UInt32 cdecl;
  protected type
    TSDL_GetTicks = function: SDL_UInt32 cdecl;
    TSDL_GetPerformanceCounter = function: SDL_UInt64 cdecl;
    TSDL_GetPerformanceFrequency = function: SDL_UInt64 cdecl;
    TSDL_Delay = procedure(ms: SDL_UInt32) cdecl;
    TSDL_AddTimer = function(interval: UInt32; callback: TSDL_TimerCallback; param: Pointer): SDL_TimerID cdecl;
    TSDL_RemoveTimer = function(id: SDL_TimerID): SDL_Bool cdecl;
  public
    (**
     * \brief Get the number of milliseconds since the SDL library initialization.
     *
     * \note This value wraps if the program runs for more than ~49 days.
     *)
    SDL_GetTicks: TSDL_GetTicks;
    (**
     * \brief Get the current value of the high resolution counter
     *)
    SDL_GetPerformanceCounter: TSDL_GetPerformanceCounter;
    (**
     * \brief Get the count per second of the high resolution counter
     *)
    SDL_GetPerformanceFrequency: TSDL_GetPerformanceFrequency;
    (**
     * \brief Wait a specified number of milliseconds before returning.
     *)
    SDL_Delay: TSDL_Delay;
    (**
     * \brief Add a new timer to the pool of timers already running.
     *
     * \return A timer ID, or 0 when an error occurs.
     *)
    SDL_AddTimer: TSDL_AddTimer;
    (**
     * \brief Remove a timer knowing its ID.
     *
     * \return A boolean value indicating success or failure.
     *
     * \warning It is not safe to remove a timer multiple times.
     *)
    SDL_RemoveTimer: TSDL_RemoveTimer;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     * \brief Compare SDL ticks values, and return true if A has passed B
     *
     * e.g. if you want to wait 100 ms, you could do this:
     *  Uint32 timeout = SDL_GetTicks() + 100;
     *  while (!SDL_TICKS_PASSED(SDL_GetTicks(), timeout)) {
     *      ... do work until timeout has elapsed
     *  }
     *)
    function SDL_TICKS_PASSED(Const A, B: SDL_UInt32):Boolean;
  end;

implementation

{ TSDLTimerApi }

procedure TSDLTimerApi.DoInit;
begin

end;

procedure TSDLTimerApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetTicks', @SDL_GetTicks);
  List.Add('SDL_GetPerformanceCounter', @SDL_GetPerformanceCounter);
  List.Add('SDL_GetPerformanceFrequency', @SDL_GetPerformanceFrequency);
  List.Add('SDL_Delay', @SDL_Delay);
  List.Add('SDL_AddTimer', @SDL_AddTimer);
  List.Add('SDL_RemoveTimer', @SDL_RemoveTimer)
end;

function TSDLTimerApi.SDL_TICKS_PASSED(const A, B: SDL_UInt32): Boolean;
begin
  Result := A > B
end;

end.

