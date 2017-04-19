(**
 *  \file SDL_gesture.h
 *
 *  Include file for SDL gesture event handling.
 *)
unit SDL.Api.Gesture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLGestureApi }

  TSDLGestureApi = class(TSubLibrary)
  private type
    TSDL_RecordGesture = function(touchId: SDL_TouchID): SDL_SInt32 cdecl;
    TSDL_SaveAllDollarTemplates = function(src: PSDL_RWops): SDL_SInt32 cdecl;
    TSDL_SaveDollarTemplate = function(gestureId: SDL_GestureID; src: PSDL_RWops): SDL_SInt32 cdecl;
    TSDL_LoadDollarTemplates = function(touchId: SDL_TouchID; src: PSDL_RWops): SDL_SInt32 cdecl;
  public
    (**
     *  \brief Begin Recording a gesture on the specified touch, or all touches (-1)
     *
     *
     *)
    SDL_RecordGesture: TSDL_RecordGesture;
    (**
     *  \brief Save all currently loaded Dollar Gesture templates
     *
     *
     *)
    SDL_SaveAllDollarTemplates: TSDL_SaveAllDollarTemplates;
    (**
     *  \brief Save a currently loaded Dollar Gesture template
     *
     *
     *)
    SDL_SaveDollarTemplate: TSDL_SaveDollarTemplate;
    (**
     *
     *  \brief Load Dollar Gesture templates from a file
     *
     *)
    SDL_LoadDollarTemplates: TSDL_LoadDollarTemplates;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLGestureApi }

procedure TSDLGestureApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_RecordGesture', @SDL_RecordGesture);
  List.Add('SDL_SaveAllDollarTemplates', @SDL_SaveAllDollarTemplates);
  List.Add('SDL_SaveDollarTemplate', @SDL_SaveDollarTemplate);
  List.Add('SDL_LoadDollarTemplates', @SDL_LoadDollarTemplates);
end;

end.

