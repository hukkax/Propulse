unit SDL.Api.LoadSO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLLoadSOApi }

  TSDLLoadSOApi = class(TSubLibrary)
  private type
    TSDL_LoadObject = function(Const sofile: SDL_String): SDL_LibHandle;
    TSDL_LoadFunction = function(handle: SDL_LibHandle; Const name: SDL_String): SDL_Pointer;
    TSDL_UnloadObject = procedure(handle: SDL_LibHandle);
  public
    (**
     *  This function dynamically loads a shared object and returns a pointer
     *  to the object handle (or NULL if there was an error).
     *  The 'sofile' parameter is a system dependent name of the object file.
     *)
    SDL_LoadObject: TSDL_LoadObject;
    (**
     *  Given an object handle, this function looks up the address of the
     *  named function in the shared object and returns it.  This address
     *  is no longer valid after calling SDL_UnloadObject().
     *)
    SDL_LoadFunction: TSDL_LoadFunction;
    (**
     *  Unload a shared object from memory.
     *)
    SDL_UnloadObject: tSDL_UnloadObject;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLLoadSOApi }

procedure TSDLLoadSOApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_LoadObject', @SDL_LoadObject);
  List.Add('SDL_LoadFunction', @SDL_LoadFunction);
  List.Add('SDL_UnloadObject', @SDL_UnloadObject);
end;

end.

