(**
 *  \file SDL_cpuinfo.h
 *
 *  CPU feature detection for SDL.
 *)
unit SDL.Api.CPUInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLCPUInfoApi }

  TSDLCPUInfoApi = class(TSubLibrary)
  private type
    TSDL_GetCPUCount = function(): SDL_SInt32; cdecl;
    TSDL_GetCPUCacheLineSize = function(): SDL_SInt32 cdecl;
    TSDL_HasRDTSC = function(): SDL_Bool cdecl;
    TSDL_HasAltiVec = function(): SDL_Bool cdecl;
    TSDL_HasMMX = function(): SDL_Bool cdecl;
    TSDL_Has3DNow = function(): SDL_Bool cdecl;
    TSDL_HasSSE = function(): SDL_Bool cdecl;
    TSDL_HasSSE2 = function(): SDL_Bool cdecl;
    TSDL_HasSSE3 = function(): SDL_Bool cdecl;
    TSDL_HasSSE41 = function(): SDL_Bool cdecl;
    TSDL_HasSSE42 = function(): SDL_Bool cdecl;
    TSDL_HasAVX = function(): SDL_Bool cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_HasAVX2 = function(): SDL_Bool cdecl;
    {$ENDIF}
    TSDL_GetSystemRAM = function(): SDL_SInt32 cdecl;
  public
    (**
     *  This function returns the number of CPU cores available.
     *)
    SDL_GetCPUCount: TSDL_GetCPUCount;
    (**
     *  This function returns the L1 cache line size of the CPU
     *
     *  This is useful for determining multi-threaded structure padding
     *  or SIMD prefetch sizes.
     *)
    SDL_GetCPUCacheLineSize: TSDL_GetCPUCacheLineSize;
    (**
     *  This function returns true if the CPU has the RDTSC instruction.
     *)
    SDL_HasRDTSC: TSDL_HasRDTSC;
    (**
     *  This function returns true if the CPU has AltiVec features.
     *)
    SDL_HasAltiVec: TSDL_HasAltiVec;
    (**
     *  This function returns true if the CPU has MMX features.
     *)
    SDL_HasMMX: TSDL_HasMMX;
    (**
     *  This function returns true if the CPU has 3DNow! features.
     *)
    SDL_Has3DNow: TSDL_Has3DNow;
    (**
     *  This function returns true if the CPU has SSE features.
     *)
    SDL_HasSSE: TSDL_HasSSE;
    (**
     *  This function returns true if the CPU has SSE2 features.
     *)
    SDL_HasSSE2: TSDL_HasSSE2;
    (**
     *  This function returns true if the CPU has SSE3 features.
     *)
    SDL_HasSSE3: TSDL_HasSSE3;
    (**
     *  This function returns true if the CPU has SSE4.1 features.
     *)
    SDL_HasSSE41: TSDL_HasSSE41;
    (**
     *  This function returns true if the CPU has SSE4.2 features.
     *)
    SDL_HasSSE42: TSDL_HasSSE42;
    (**
     *  This function returns true if the CPU has AVX features.
     *)
    SDL_HasAVX: TSDL_HasAVX;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  This function returns true if the CPU has AVX2 features.
     *)
    SDL_HasAVX2: TSDL_HasAVX2;
    {$ENDIF}
    (**
     *  This function returns the amount of RAM configured in the system, in MB.
     *)
    SDL_GetSystemRAM: TSDL_GetSystemRAM;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLCPUInfoApi }

procedure TSDLCPUInfoApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetCPUCount', @SDL_GetCPUCount);
  List.Add('SDL_GetCPUCacheLineSize', @SDL_GetCPUCacheLineSize);
  List.Add('SDL_HasRDTSC', @SDL_HasRDTSC);
  List.Add('SDL_HasAltiVec', @SDL_HasAltiVec);
  List.Add('SDL_Has3DNow', @SDL_Has3DNow);
  List.Add('SDL_HasMMX', @SDL_HasMMX);
  List.Add('SDL_HasSSE', @SDL_HasSSE);
  List.Add('SDL_HasSSE2', @SDL_HasSSE2);
  List.Add('SDL_HasSSE3', @SDL_HasSSE3);
  List.Add('SDL_HasSSE41', @SDL_HasSSE41);
  List.Add('SDL_HasSSE42', @SDL_HasSSE42);
  List.Add('SDL_HasAVX', @SDL_HasAVX);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_HasAVX2', @SDL_HasAVX2, False);
  {$ENDIF}
  List.Add('SDL_GetSystemRAM', @SDL_GetSystemRAM);
end;

end.

