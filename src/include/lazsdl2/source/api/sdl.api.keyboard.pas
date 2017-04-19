(**
 *  \file SDL_keyboard.h
 *
 *  Include file for SDL keyboard event handling
 *)
unit SDL.Api.Keyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLKeyboardApi }

  TSDLKeyboardApi = class(TSubLibrary)
  private type
    TSDL_GetKeyboardFocus = function: PSDL_Window cdecl;
    TSDL_GetKeyboardState = function(numkeys: PSDL_SInt32): PSDLKeyStatesArr cdecl;
    TSDL_GetModState = function: SDL_Keymods cdecl;
    TSDL_SetModState = procedure(modstate: SDL_Keymods) cdecl;
    TSDL_GetKeyFromScancode = function(scancode: SDL_ScanCode): SDL_KeyCode cdecl;
    TSDL_GetScancodeFromKey = function(key: SDL_KeyCode): SDL_ScanCode cdecl;
    TSDL_GetScancodeName = function(scancode: SDL_ScanCode): SDL_String cdecl;
    TSDL_GetScancodeFromName = function(const name: SDL_String): SDL_ScanCode cdecl;
    TSDL_GetKeyName = function(key: SDL_Keycode): SDL_String cdecl;
    TSDL_GetKeyFromName = function(const name: SDL_String): SDL_KeyCode cdecl;
    TSDL_StartTextInput = procedure cdecl;
    TSDL_IsTextInputActive = function: SDL_Bool cdecl;
    TSDL_StopTextInput = procedure cdecl;
    TSDL_SetTextInputRect = procedure(rect: PSDL_Rect) cdecl;
    TSDL_HasScreenKeyboardSupport = function: SDL_Bool cdecl;
    TSDL_IsScreenKeyboardShown = function(window: PSDL_Window): SDL_Bool cdecl;
  public
    (**
     *  \brief Get the window which currently has keyboard focus.
     *)
    SDL_GetKeyboardFocus: TSDL_GetKeyboardFocus;
    (**
     *  \brief Get a snapshot of the current state of the keyboard.
     *
     *  \param numkeys if non-NULL, receives the length of the returned array.
     *
     *  \return An array of key states. Indexes into this array are obtained by using ::SDL_Scancode values.
     *
     *  \b Example:
     *  \code
     *  const Uint8 *state = SDL_GetKeyboardState(NULL);
     *  if ( state[SDL_SCANCODE_RETURN] )   {
     *      printf("<RETURN> is pressed.\n");
     *  }
     *  \endcode
     *)
     SDL_GetKeyboardState: TSDL_GetKeyboardState;
     (**
      *  \brief Get the current key modifier state for the keyboard.
      *)
     SDL_GetModState: TSDL_GetModState;
     (**
      *  \brief Set the current key modifier state for the keyboard.
      *
      *  \note This does not change the keyboard state, only the key modifier flags.
      *)
     SDL_SetModState: TSDL_SetModState;
     (**
      *  \brief Get the key code corresponding to the given scancode according
      *         to the current keyboard layout.
      *
      *  See ::SDL_Keycode for details.
      *
      *  \sa SDL_GetKeyName()
      *)
     SDL_GetKeyFromScancode: TSDL_GetKeyFromScancode;
     (**
      *  \brief Get the scancode corresponding to the given key code according to the
      *         current keyboard layout.
      *
      *  See ::SDL_Scancode for details.
      *
      *  \sa SDL_GetScancodeName()
      *)
     SDL_GetScancodeFromKey: TSDL_GetScancodeFromKey;
     (**
      *  \brief Get a human-readable name for a scancode.
      *
      *  \return A pointer to the name for the scancode.
      *          If the scancode doesn't have a name, this function returns
      *          an empty string ("").
      *
      *  \sa SDL_Scancode
      *)
     SDL_GetScancodeName: TSDL_GetScancodeName;
     (**
      *  \brief Get a scancode from a human-readable name
      *
      *  \return scancode, or SDL_SCANCODE_UNKNOWN if the name wasn't recognized
      *
      *  \sa SDL_Scancode
      *)
     SDL_GetScancodeFromName: TSDL_GetScancodeFromName;
     (**
      *  \brief Get a human-readable name for a key.
      *
      *  \return A pointer to a UTF-8 string that stays valid at least until the next
      *          call to this function. If you need it around any longer, you must
      *          copy it.  If the key doesn't have a name, this function returns an
      *          empty string ("").
      *
      *  \sa SDL_Key
      *)
     SDL_GetKeyName: TSDL_GetKeyName;
     (**
      *  \brief Get a key code from a human-readable name
      *
      *  \return key code, or SDLK_UNKNOWN if the name wasn't recognized
      *
      *  \sa SDL_Keycode
      *)
     SDL_GetKeyFromName: TSDL_GetKeyFromName;
     (**
      *  \brief Start accepting Unicode text input events.
      *         This function will show the on-screen keyboard if supported.
      *
      *  \sa SDL_StopTextInput()
      *  \sa SDL_SetTextInputRect()
      *  \sa SDL_HasScreenKeyboardSupport()
      *)
     SDL_StartTextInput: TSDL_StartTextInput;
     (**
      *  \brief Return whether or not Unicode text input events are enabled.
      *
      *  \sa SDL_StartTextInput()
      *  \sa SDL_StopTextInput()
      *)
     SDL_IsTextInputActive: TSDL_IsTextInputActive;
     (**
      *  \brief Stop receiving any text input events.
      *         This function will hide the on-screen keyboard if supported.
      *
      *  \sa SDL_StartTextInput()
      *  \sa SDL_HasScreenKeyboardSupport()
      *)
     SDL_StopTextInput: TSDL_StopTextInput;
     (**
      *  \brief Set the rectangle used to type Unicode text inputs.
      *         This is used as a hint for IME and on-screen keyboard placement.
      *
      *  \sa SDL_StartTextInput()
      *)
     SDL_SetTextInputRect: TSDL_SetTextInputRect;
     (**
      *  \brief Returns whether the platform has some screen keyboard support.
      *
      *  \return SDL_TRUE if some keyboard support is available else SDL_FALSE.
      *
      *  \note Not all screen keyboard functions are supported on all platforms.
      *
      *  \sa SDL_IsScreenKeyboardShown()
      *)
     SDL_HasScreenKeyboardSupport: TSDL_HasScreenKeyboardSupport;
     (**
      *  \brief Returns whether the screen keyboard is shown for given window.
      *
      *  \param window The window for which screen keyboard should be queried.
      *
      *  \return SDL_TRUE if screen keyboard is shown else SDL_FALSE.
      *
      *  \sa SDL_HasScreenKeyboardSupport()
      *)
     SDL_IsScreenKeyboardShown: TSDL_IsScreenKeyboardShown;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLKeyboardApi }

procedure TSDLKeyboardApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetKeyboardFocus', @SDL_GetKeyboardFocus);
  List.Add('SDL_GetKeyboardState', @SDL_GetKeyboardState);
  List.Add('SDL_GetModState', @SDL_GetModState);
  List.Add('SDL_SetModState', @SDL_SetModState);
  List.Add('SDL_GetKeyFromScancode', @SDL_GetKeyFromScancode);
  List.Add('SDL_GetScancodeFromKey', @SDL_GetScancodeFromKey);
  List.Add('SDL_GetScancodeName', @SDL_GetScancodeName);
  List.Add('SDL_GetScancodeFromName', @SDL_GetScancodeFromName);
  List.Add('SDL_GetKeyName', @SDL_GetKeyName);
  List.Add('SDL_GetKeyFromName', @SDL_GetKeyFromName);
  List.Add('SDL_StartTextInput', @SDL_StartTextInput);
  List.Add('SDL_IsTextInputActive', @SDL_IsTextInputActive);
  List.Add('SDL_StopTextInput', @SDL_StopTextInput);
  List.Add('SDL_SetTextInputRect', @SDL_SetTextInputRect);
  List.Add('SDL_HasScreenKeyboardSupport', @SDL_HasScreenKeyboardSupport);
  List.Add('SDL_IsScreenKeyboardShown', @SDL_IsScreenKeyboardShown);
end;

end.

