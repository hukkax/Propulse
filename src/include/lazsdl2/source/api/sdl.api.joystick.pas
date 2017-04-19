unit SDL.Api.JoyStick;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLJoyStickApi }

  TSDLJoyStickApi = class(TSubLibrary)
  private type
    TSDL_NumJoysticks = function: SDL_SInt32 cdecl;
    TSDL_JoystickNameForIndex = function(device_index: SDL_SInt32): SDL_String cdecl;
    TSDL_JoystickOpen = function(device_index: SDL_SInt32): PSDL_Joystick cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_JoystickFromInstanceID = function(joyid: SDL_JoystickID): PSDL_Joystick cdecl;
    {$ENDIF}
    TSDL_JoystickName = function(joystick: PSDL_Joystick): SDL_String cdecl;
    TSDL_JoystickGetDeviceGUID = function(device_index: SDL_SInt32): SDL_JoystickGUID cdecl;
    TSDL_JoystickGetGUID = function(joystick: PSDL_Joystick): SDL_JoystickGUID cdecl;
    TSDL_JoystickGetGUIDString = procedure(guid: SDL_JoystickGUId; pszGUID: SDL_String; cbGUID: SDL_SInt32) cdecl;
    TSDL_JoystickGetGUIDFromString = function(const pchGUID: SDL_String): SDL_JoystickGUID cdecl;
    TSDL_JoystickGetAttached = function(joystick: PSDL_Joystick): SDL_Bool cdecl;
    TSDL_JoystickInstanceID = function(joystick: PSDL_Joystick): SDL_JoystickID cdecl;
    TSDL_JoystickNumAxes = function(joystick: PSDL_Joystick): SDL_SInt32 cdecl;
    TSDL_JoystickNumBalls = function(joystick: PSDL_Joystick): SDL_SInt32 cdecl;
    TSDL_JoystickNumHats = function(joystick: PSDL_Joystick): SDL_SInt32 cdecl;
    TSDL_JoystickNumButtons = function(joystick: PSDL_Joystick): SDL_SInt32 cdecl;
    TSDL_JoystickUpdate = procedure cdecl;
    TSDL_JoystickEventState = function(state: SDL_EventState): SDL_SInt32 cdecl;
    TSDL_JoystickGetAxis = function(joystick: PSDL_Joystick; axis: SDL_SInt32): SDL_SInt16 cdecl;
    TSDL_JoystickGetHat = function(joystick: PSDL_Joystick; hat: SDL_SInt32): SDL_HatState cdecl;
    TSDL_JoystickGetBall = function(joystick: PSDL_Joystick; ball: SDL_SInt32; out dx: SDL_SInt32; out dy: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_JoystickGetButton = function(joystick: PSDL_Joystick; button: SDL_SInt32): ByteBool cdecl;
    TSDL_JoystickClose = procedure(joystick: PSDL_Joystick) cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_JoystickPowerLevel = function(joystick: PSDL_Joystick): SDL_JoystickPowerLevel cdecl;
    {$ENDIF}
  public
    (**
     *  Count the number of joysticks attached to the system right now
     *)
    SDL_NumJoysticks: TSDL_NumJoysticks;
    (**
     *  Get the implementation dependent name of a joystick.
     *  This can be called before any joysticks are opened.
     *  If no name can be found, this function returns NULL.
     *)
    SDL_JoystickNameForIndex: TSDL_JoystickNameForIndex;
    (**
     *  Open a joystick for use.
     *  The index passed as an argument refers to the N'th joystick on the system.
     *  This index is not the value which will identify this joystick in future
     *  joystick events.  The joystick's instance id (::SDL_JoystickID) will be used
     *  there instead.
     *
     *  \return A joystick identifier, or NULL if an error occurred.
     *)
    SDL_JoystickOpen: TSDL_JoystickOpen;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     * Return the SDL_Joystick associated with an instance id.
     *)
    SDL_JoystickFromInstanceID: TSDL_JoystickFromInstanceID;
    {$ENDIF}
    (**
     *  Return the name for this currently opened joystick.
     *  If no name can be found, this function returns NULL.
     *)
    SDL_JoystickName: TSDL_JoystickName;
    (**
     *  Return the GUID for the joystick at this index
     *)
    SDL_JoystickGetDeviceGUID: TSDL_JoystickGetDeviceGUID;
    (**
     *  Return the GUID for this opened joystick
     *)
    SDL_JoystickGetGUID: TSDL_JoystickGetGUID;
    (**
     *  Return a string representation for this guid. pszGUID must point to at least 33 bytes
     *  (32 for the string plus a NULL terminator).
     *)
    SDL_JoystickGetGUIDString: TSDL_JoystickGetGUIDString;
    (**
     *  convert a string into a joystick formatted guid
     *)
    SDL_JoystickGetGUIDFromString: TSDL_JoystickGetGUIDFromString;
    (**
     *  Returns SDL_TRUE if the joystick has been opened and currently connected, or SDL_FALSE if it has not.
     *)
    SDL_JoystickGetAttached: TSDL_JoystickGetAttached;
    (**
     *  Get the instance ID of an opened joystick or -1 if the joystick is invalid.
     *)
    SDL_JoystickInstanceID: TSDL_JoystickInstanceID;
    (**
     *  Get the number of general axis controls on a joystick.
     *)
    SDL_JoystickNumAxes: TSDL_JoystickNumAxes;
    (**
     *  Get the number of trackballs on a joystick.
     *
     *  Joystick trackballs have only relative motion events associated
     *  with them and their state cannot be polled.
     *)
    SDL_JoystickNumBalls: TSDL_JoystickNumBalls;
    (**
     *  Get the number of POV hats on a joystick.
     *)
    SDL_JoystickNumHats: TSDL_JoystickNumHats;
    (**
     *  Get the number of buttons on a joystick.
     *)
    SDL_JoystickNumButtons: TSDL_JoystickNumButtons;
    (**
     *  Update the current state of the open joysticks.
     *
     *  This is called automatically by the event loop if any joystick
     *  events are enabled.
     *)
    SDL_JoystickUpdate: TSDL_JoystickUpdate;
    (**
     *  Enable/disable joystick event polling.
     *
     *  If joystick events are disabled, you must call SDL_JoystickUpdate()
     *  yourself and check the state of the joystick when you want joystick
     *  information.
     *
     *  The state can be one of ::SDL_QUERY, ::SDL_ENABLE or ::SDL_IGNORE.
     *)
    SDL_JoystickEventState: TSDL_JoystickEventState;
    (**
     *  Get the current state of an axis control on a joystick.
     *
     *  The state is a value ranging from -32768 to 32767.
     *
     *  The axis indices start at index 0.
     *)
    SDL_JoystickGetAxis: TSDL_JoystickGetAxis;
    (**
     *  Get the current state of a POV hat on a joystick.
     *
     *  The hat indices start at index 0.
     *
     *  \return The return value is one of the following positions:
     *           - ::SDL_HAT_CENTERED
     *           - ::SDL_HAT_UP
     *           - ::SDL_HAT_RIGHT
     *           - ::SDL_HAT_DOWN
     *           - ::SDL_HAT_LEFT
     *           - ::SDL_HAT_RIGHTUP
     *           - ::SDL_HAT_RIGHTDOWN
     *           - ::SDL_HAT_LEFTUP
     *           - ::SDL_HAT_LEFTDOWN
     *)
    SDL_JoystickGetHat: TSDL_JoystickGetHat;
    (**
     *  Get the ball axis change since the last poll.
     *
     *  \return 0, or -1 if you passed it invalid parameters.
     *
     *  The ball indices start at index 0.
     *)
    SDL_JoystickGetBall: tSDL_JoystickGetBall;
    (**
     *  Get the current state of a button on a joystick.
     *
     *  The button indices start at index 0.
     *)
    SDL_JoystickGetButton: TSDL_JoystickGetButton;
    (**
     *  Close a joystick previously opened with SDL_JoystickOpen().
     *)
    SDL_JoystickClose: TSDL_JoystickClose;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  Return the battery level of this joystick
     *)
    SDL_JoystickPowerLevel: TSDL_JoystickPowerLevel;
    {$ENDIF}
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLJoyStickApi }

procedure TSDLJoyStickApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_NumJoysticks', @SDL_NumJoysticks);
  List.Add('SDL_JoystickNameForIndex', @SDL_JoystickNameForIndex);
  List.Add('SDL_JoystickOpen', @SDL_JoystickOpen);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_JoystickFromInstanceID', @SDL_JoystickFromInstanceID, False);
  {$ENDIF}
  List.Add('SDL_JoystickName', @SDL_JoystickName);
  List.Add('SDL_JoystickGetDeviceGUID', @SDL_JoystickGetDeviceGUID);
  List.Add('SDL_JoystickGetGUID', @SDL_JoystickGetGUID);
  List.Add('SDL_JoystickGetGUIDString', @SDL_JoystickGetGUIDString);
  List.Add('SDL_JoystickGetGUIDFromString', @SDL_JoystickGetGUIDFromString);
  List.Add('SDL_JoystickGetAttached', @SDL_JoystickGetAttached);
  List.Add('SDL_JoystickInstanceID', @SDL_JoystickInstanceID);
  List.Add('SDL_JoystickNumAxes', @SDL_JoystickNumAxes);
  List.Add('SDL_JoystickNumBalls', @SDL_JoystickNumBalls);
  List.Add('SDL_JoystickNumHats', @SDL_JoystickNumHats);
  List.Add('SDL_JoystickNumButtons', @SDL_JoystickNumButtons);
  List.Add('SDL_JoystickUpdate', @SDL_JoystickUpdate);
  List.Add('SDL_JoystickEventState', @SDL_JoystickEventState);
  List.Add('SDL_JoystickGetAxis', @SDL_JoystickGetAxis);
  List.Add('SDL_JoystickGetHat', @SDL_JoystickGetHat);
  List.Add('SDL_JoystickGetBall', @SDL_JoystickGetBall);
  List.Add('SDL_JoystickGetButton', @SDL_JoystickGetButton);
  List.Add('SDL_JoystickClose', @SDL_JoystickClose);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_JoystickPowerLevel', @SDL_JoystickPowerLevel, False);
  {$ENDIF}
end;

end.

