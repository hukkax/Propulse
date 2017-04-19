unit SDL.Api.Error;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLErrorApi }

  TSDLErrorApi = class(TSubLibrary)
  private type
    TSDL_SetError = function(const fmt: SDL_String): SDL_SInt32 cdecl; varargs;
    TSDL_GetError = function: SDL_String cdecl;
    TSDL_ClearError = procedure cdecl;
    TSDL_Error = function(code: SDL_ErrorCode): SDL_SInt32 cdecl;
  public
    (** SDL_SetError() unconditionally returns -1. *)
    SDL_SetError: TSDL_SetError;
    SDL_GetError: TSDL_GetError;
    SDL_ClearError: TSDL_ClearError;
    (** SDL_Error() unconditionally returns -1. *)
    SDL_Error: TSDL_Error;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    procedure SDL_OutOfMemory;
    procedure SDL_Err_Unsupported;
    procedure SDL_InvalidParamError(Param: SDL_String);
  end;

implementation

{ TSDLErrorApi }

procedure TSDLErrorApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_SetError', @SDL_SetError);
  List.Add('SDL_GetError', @SDL_GetError);
  List.Add('SDL_ClearError', @SDL_ClearError);
  List.Add('SDL_Error', @SDL_Error);
end;

procedure TSDLErrorApi.SDL_OutOfMemory;
begin
  SDL_Error(SDL_ENOMEM)
end;

procedure TSDLErrorApi.SDL_Err_Unsupported;
begin
  SDL_Error(SDL_UNSUPPORTED)
end;

procedure TSDLErrorApi.SDL_InvalidParamError(Param: SDL_String);
begin
  SDL_SetError('Parameter "%s" is invalid', Param)
end;

end.

