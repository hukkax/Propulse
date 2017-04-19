(*
 * \author Edgar Simo Serra
 *)
unit SDL.Api.Hints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

{$I sdlhints.inc}

type
  { TSDLHintsApi }

  TSDLHintsApi = class(TSubLibrary)
  public type
    TSDL_HintCallback = procedure(userdata: SDL_Data; const name: SDL_String; const oldValue: SDL_String; const newValue: SDL_String);
  private type
    TSDL_SetHintWithPriority = function(const name: SDL_String; const value: SDL_String; priority: SDL_HintPriority): SDL_Bool; cdecl;
    TSDL_SetHint =  function(const name: SDL_String; const value: SDL_String): SDL_Bool; cdecl;
    TSDL_GetHint = function( const name: SDL_String): SDL_String; cdecl;
    TSDL_AddHintCallback = procedure(const name: SDL_String; callback: TSDL_HintCallback; userdata: SDL_Data); cdecl;
    TSDL_DelHintCallback = procedure(const name: SDL_String; callback: TSDL_HintCallback; userdata: SDL_Data); cdecl;
    TSDL_ClearHints = procedure(); cdecl;
  public
    (**
     *  \brief Set a hint with a specific priority
     *
     *  The priority controls the behavior when setting a hint that already
     *  has a value.  Hints will replace existing hints of their priority and
     *  lower.  Environment variables are considered to have override priority.
     *
     *  \return SDL_TRUE if the hint was set, SDL_FALSE otherwise
     *)
    SDL_SetHintWithPriority: TSDL_SetHintWithPriority;
    (**
     *  \brief Set a hint with normal priority
     *
     *  \return SDL_TRUE if the hint was set, SDL_FALSE otherwise
     *)
    SDL_SetHint: TSDL_SetHint;
    (**
     *  \brief Get a hint
     *
     *  \return The string value of a hint variable.
     *)
    SDL_GetHint: TSDL_GetHint;
    (**
     *  \brief Add a function to watch a particular hint
     *
     *  \param name The hint to watch
     *  \param callback The function to call when the hint value changes
     *  \param userdata A pointer to pass to the callback function
     *)
    SDL_AddHintCallback: TSDL_AddHintCallback;
    (**
     *  \brief Remove a function watching a particular hint
     *
     *  \param name The hint being watched
     *  \param callback The function being called when the hint value changes
     *  \param userdata A pointer being passed to the callback function
     *)
    SDL_DelHintCallback: TSDL_DelHintCallback;
    (**
     *  \brief  Clear all hints
     *
     *  This function is called during SDL_Quit() to free stored hints.
     *)
    SDL_ClearHints: TSDL_ClearHints;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLHintsApi }

procedure TSDLHintsApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_SetHintWithPriority', @SDL_SetHintWithPriority);
  List.Add('SDL_SetHint', @SDL_SetHint);
  List.Add('SDL_GetHint', @SDL_GetHint);
  List.Add('SDL_AddHintCallback', @SDL_AddHintCallback);
  List.Add('SDL_DelHintCallback', @SDL_DelHintCallback);
  List.Add('SDL_ClearHints', @SDL_ClearHints);
end;

end.

