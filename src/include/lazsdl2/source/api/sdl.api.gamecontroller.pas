(**
 *  \file SDL_gamecontroller.h
 *
 *  In order to use these functions, SDL_Init() must have been called
 *  with the ::SDL_INIT_GAMECONTROLLER flag.  This causes SDL to scan the system
 *  for game controllers, and load appropriate drivers.
 *
 *  If you would like to receive controller updates while the application
 *  is in the background, you should set the following hint before calling
 *  SDL_Init(): SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
 *)
unit SDL.Api.GameController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  (**
   *  To count the number of game controllers in the system for the following:
   *  int nJoysticks = SDL_NumJoysticks();
   *  int nGameControllers = 0;
   *  for ( int i = 0; i < nJoysticks; i++ ) {
   *      if ( SDL_IsGameController(i) ) {
   *          nGameControllers++;
   *      }
   *  }
   *
   *  Using the SDL_HINT_GAMECONTROLLERCONFIG hint or the SDL_GameControllerAddMapping you can add support for controllers SDL is unaware of or cause an existing controller to have a different binding. The format is:
   *  guid,name,mappings
   *
   *  Where GUID is the string value from SDL_JoystickGetGUIDString(), name is the human readable string for the device and mappings are controller mappings to joystick ones.
   *  Under Windows there is a reserved GUID of "xinput" that covers any XInput devices.
   *  The mapping format for joystick is:
   *      bX - a joystick button, index X
   *      hX.Y - hat X with value Y
   *      aX - axis X of the joystick
   *  Buttons can be used as a controller axis and vice versa.
   *
   *  This string shows an example of a valid mapping for a controller
   *  "341a3608000000000000504944564944,Afterglow PS3 Controller,a:b1,b:b2,y:b3,x:b0,start:b9,guide:b12,back:b8,dpup:h0.1,dpleft:h0.8,dpdown:h0.4,dpright:h0.2,leftshoulder:b4,rightshoulder:b5,leftstick:b10,rightstick:b11,leftx:a0,lefty:a1,rightx:a2,righty:a3,lefttrigger:b6,righttrigger:b7",
   *
   *)

  { TSDLGameControllerApi }

  TSDLGameControllerApi = class(TSubLibrary)
  private type
    TSDL_GameControllerAddMappingsFromRW = function(rw: PSDL_RWops; freerw: SDL_SInt32): SDL_SInt32; cdecl;
    TSDL_GameControllerAddMapping = function( mappingString: SDL_String ): SDL_SInt32 cdecl;
    TSDL_GameControllerMappingForGUID = function( guid: SDL_JoystickGUID ): SDL_String cdecl;
    TSDL_GameControllerMapping = function( gamecontroller: PSDL_GameController ): SDL_String cdecl;
    TSDL_IsGameController = function(joystick_index: SDL_SInt32): SDL_Bool cdecl;
    TSDL_GameControllerNameForIndex = function(joystick_index: SDL_SInt32): SDL_String cdecl;
    TSDL_GameControllerOpen = function(joystick_index: SDL_SInt32): PSDL_GameController cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_GameControllerFromInstanceID = function(joyid: SDL_JoystickID): PSDL_GameController;
    {$ENDIF}
    TSDL_GameControllerName = function(gamecontroller: PSDL_GameController): SDL_String cdecl;
    TSDL_GameControllerGetAttached = function(gamecontroller: PSDL_GameController): SDL_Bool cdecl;
    TSDL_GameControllerGetJoystick = function(gamecontroller: PSDL_GameController): PSDL_Joystick cdecl;
    TSDL_GameControllerEventState = function(state: SDL_EventState): SDL_SInt32 cdecl;
    TSDL_GameControllerUpdate = procedure() cdecl;
    TSDL_GameControllerGetAxisFromString = function(pchString: SDL_String): SDL_GameControllerAxis cdecl;
    TSDL_GameControllerGetStringForAxis = function(axis: SDL_GameControllerAxis): SDL_String cdecl;
    TSDL_GameControllerGetBindForAxis = function(gamecontroller: PSDL_GameController; axis: SDL_GameControllerAxis): SDL_GameControllerButtonBind cdecl;
    TSDL_GameControllerGetAxis = function(gamecontroller: PSDL_GameController; axis: SDL_GameControllerAxis): SDL_SInt16 cdecl;
    TSDL_GameControllerGetButtonFromString = function(pchString: SDL_String): SDL_GameControllerButton cdecl;
    TSDL_GameControllerGetStringForButton = function(button: SDL_GameControllerButton): SDL_String cdecl;
    TSDL_GameControllerGetBindForButton = function(gamecontroller: PSDL_GameController; button: SDL_GameControllerButton): SDL_GameControllerButtonBind cdecl;
    TSDL_GameControllerGetButton = function(gamecontroller: PSDL_GameController; button: SDL_GameControllerButton): SDL_UInt8 cdecl;
    TSDL_GameControllerClose = procedure(gamecontroller: PSDL_GameController) cdecl;
  public
    (**
     *  Load a set of mappings from a seekable SDL data stream (memory or file), filtered by the current SDL_GetPlatform()
     *  A community sourced database of controllers is available at https://raw.github.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt
     *
     *  If \c freerw is non-zero, the stream will be closed after being read.
     *
     * \return number of mappings added, -1 on error
     *)
    SDL_GameControllerAddMappingsFromRW: TSDL_GameControllerAddMappingsFromRW;
    (**
     *  Add or update an existing mapping configuration
     *
     * \return 1 if mapping is added, 0 if updated, -1 on error
     *)
    SDL_GameControllerAddMapping: TSDL_GameControllerAddMapping;
    (**
     *  Get a mapping string for a GUID
     *
     *  \return the mapping string.  Must be freed with SDL_free.  Returns NULL if no mapping is available
     *)
    SDL_GameControllerMappingForGUID: TSDL_GameControllerMappingForGUID;
    (**
     *  Get a mapping string for an open GameController
     *
     *  \return the mapping string.  Must be freed with SDL_free.  Returns NULL if no mapping is available
     *)
    SDL_GameControllerMapping: tSDL_GameControllerMapping;
    (**
     *  Is the joystick on this index supported by the game controller interface?
     *)
    SDL_IsGameController: TSDL_IsGameController;
    (**
     *  Get the implementation dependent name of a game controller.
     *  This can be called before any controllers are opened.
     *  If no name can be found, this function returns NULL.
     *)
    SDL_GameControllerNameForIndex: TSDL_GameControllerNameForIndex;
    (**
     *  Open a game controller for use.
     *  The index passed as an argument refers to the N'th game controller on the system.
     *  This index is not the value which will identify this controller in future
     *  controller events.  The joystick's instance id (::SDL_JoystickID) will be
     *  used there instead.
     *
     *  \return A controller identifier, or NULL if an error occurred.
     *)
    SDL_GameControllerOpen: TSDL_GameControllerOpen;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     * Return the SDL_GameController associated with an instance id.
     *)
    SDL_GameControllerFromInstanceID: TSDL_GameControllerFromInstanceID;
    {$ENDIF}
    (**
     *  Return the name for this currently opened controller
     *)
    SDL_GameControllerName: TSDL_GameControllerName;
    (**
     *  Returns SDL_TRUE if the controller has been opened and currently connected,
     *  or SDL_FALSE if it has not.
     *)
    SDL_GameControllerGetAttached: TSDL_GameControllerGetAttached;
    (**
     *  Get the underlying joystick object used by a controller
     *)
    SDL_GameControllerGetJoystick: TSDL_GameControllerGetJoystick;
    (**
     *  Enable/disable controller event polling.
     *
     *  If controller events are disabled, you must call SDL_GameControllerUpdate()
     *  yourself and check the state of the controller when you want controller
     *  information.
     *
     *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
     *)
    SDL_GameControllerEventState: TSDL_GameControllerEventState;
    (**
     *  Update the current state of the open game controllers.
     *
     *  This is called automatically by the event loop if any game controller
     *  events are enabled.
     *)
    SDL_GameControllerUpdate: TSDL_GameControllerUpdate;
    (**
     *  turn this string into a axis mapping
     *)
    SDL_GameControllerGetAxisFromString: TSDL_GameControllerGetAxisFromString;
    (**
     *  turn this axis enum into a string mapping
     *)
    SDL_GameControllerGetStringForAxis: TSDL_GameControllerGetStringForAxis;
    (**
     *  Get the SDL joystick layer binding for this controller button mapping
     *)
    SDL_GameControllerGetBindForAxis: TSDL_GameControllerGetBindForAxis;
    (**
     *  Get the current state of an axis control on a game controller.
     *
     *  The state is a value ranging from -32768 to 32767 (except for the triggers,
     *  which range from 0 to 32767).
     *
     *  The axis indices start at index 0.
     *)
    SDL_GameControllerGetAxis: TSDL_GameControllerGetAxis;
    (**
     *  turn this string into a button mapping
     *)
    SDL_GameControllerGetButtonFromString: TSDL_GameControllerGetButtonFromString;
    (**
     *  turn this button enum into a string mapping
     *)
    SDL_GameControllerGetStringForButton: TSDL_GameControllerGetStringForButton;
    (**
     *  Get the SDL joystick layer binding for this controller button mapping
     *)
    SDL_GameControllerGetBindForButton: TSDL_GameControllerGetBindForButton;
    (**
     *  Get the current state of a button on a game controller.
     *
     *  The button indices start at index 0.
     *)
    SDL_GameControllerGetButton: TSDL_GameControllerGetButton;
    (**
     *  Close a controller previously opened with SDL_GameControllerOpen().
     *)
    SDL_GameControllerClose: TSDL_GameControllerClose;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;

    (**
     *  Load a set of mappings from a file, filtered by the current SDL_GetPlatform()
     *
     *  Convenience macro.
     *)
    function SDL_GameControllerAddMappingsFromFile(const FileName: String): SDL_SInt32;
  end;

implementation

{ TSDLGameControllerApi }

procedure TSDLGameControllerApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GameControllerAddMappingsFromRW', @SDL_GameControllerAddMappingsFromRW);
  List.Add('SDL_GameControllerAddMapping', @SDL_GameControllerAddMapping);
  List.Add('SDL_GameControllerMappingForGUID', @SDL_GameControllerMappingForGUID);
  List.Add('SDL_GameControllerMapping', @SDL_GameControllerMapping);
  List.Add('SDL_IsGameController', @SDL_IsGameController);
  List.Add('SDL_GameControllerNameForIndex', @SDL_GameControllerNameForIndex);
  List.Add('SDL_GameControllerOpen', @SDL_GameControllerOpen);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_GameControllerFromInstanceID', @SDL_GameControllerFromInstanceID, False);
  {$ENDIF}
  List.Add('SDL_GameControllerName', @SDL_GameControllerName);
  List.Add('SDL_GameControllerGetAttached', @SDL_GameControllerGetAttached);
  List.Add('SDL_GameControllerGetJoystick', @SDL_GameControllerGetJoystick);
  List.Add('SDL_GameControllerEventState', @SDL_GameControllerEventState);
  List.Add('SDL_GameControllerUpdate', @SDL_GameControllerUpdate);
  List.Add('SDL_GameControllerGetAxisFromString', @SDL_GameControllerGetAxisFromString);
  List.Add('SDL_GameControllerGetStringForAxis', @SDL_GameControllerGetStringForAxis);
  List.Add('SDL_GameControllerGetBindForAxis', @SDL_GameControllerGetBindForAxis);
  List.Add('SDL_GameControllerGetAxis', @SDL_GameControllerGetAxis);
  List.Add('SDL_GameControllerGetButtonFromString', @SDL_GameControllerGetButtonFromString);
  List.Add('SDL_GameControllerGetBindForButton', @SDL_GameControllerGetBindForButton);
  List.Add('SDL_GameControllerGetButton', @SDL_GameControllerGetButton);
  List.Add('SDL_GameControllerClose', @SDL_GameControllerClose);
end;

function TSDLGameControllerApi.SDL_GameControllerAddMappingsFromFile(
  const FileName: String): SDL_SInt32;
begin
  Result := 0;
end;

end.

