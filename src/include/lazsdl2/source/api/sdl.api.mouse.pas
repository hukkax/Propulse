(**
 *  \file SDL_mouse.h
 *
 *  Include file for SDL mouse event handling.
 *)
unit SDL.Api.Mouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLMouseApi }

  TSDLMouseApi = class(TSubLibrary)
  private type
    TSDL_GetMouseFocus = function: PSDL_Window cdecl;
    TSDL_GetMouseState = function(out x: SDL_SInt32; out y: SDL_SInt32): UInt32 cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_GetGlobalMouseState = function(out x: SDL_SInt32; out y: SDL_SInt32): SDL_UInt32 cdecl;
    {$ENDIF}
    TSDL_GetRelativeMouseState = function(out x: SDL_SInt32; out y: SDL_SInt32): UInt32 cdecl;
    TSDL_WarpMouseInWindow = procedure(window: PSDL_Window; x: SDL_SInt32; y: SDL_SInt32) cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_WarpMouseGlobal = function(x: SDL_SInt32; y: SDL_SInt32): SDL_SInt32 cdecl;
    {$ENDIF}
    TSDL_SetRelativeMouseMode = function(enabled: SDL_Bool): SDL_SInt32 cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_CaptureMouse = function(enabled: SDL_Bool): SDL_SInt32 cdecl;
    {$ENDIF}
    TSDL_GetRelativeMouseMode = function: SDL_Bool cdecl;
    TSDL_CreateCursor = function(const data: PSDL_UInt8; const mask: PSDL_UInt8; w: SDL_SInt32; h: SDL_SInt32; hot_x: SDL_SInt32; hot_y: SDL_SInt32): PSDL_Cursor cdecl;
    TSDL_CreateColorCursor = function(surface: PSDL_Surface; hot_x: SDL_SInt32; SDL_hot_y: SDL_SInt32): PSDL_Cursor cdecl;
    TSDL_CreateSystemCursor = function(id: SDL_SystemCursor): PSDL_Cursor cdecl;
    TSDL_SetCursor = procedure(cursor: PSDL_Cursor) cdecl;
    TSDL_GetCursor = function: PSDL_Cursor cdecl;
    TSDL_GetDefaultCursor = function: PSDL_Cursor cdecl;
    TSDL_FreeCursor = procedure(cursor: PSDL_Cursor) cdecl;
    TSDL_ShowCursor = function(toggle: SDL_CursorAction): SDL_CursorState cdecl;
  public
    (**
     *  \brief Get the window which currently has mouse focus.
     *)
    SDL_GetMouseFocus: TSDL_GetMouseFocus;
    (**
     *  \brief Retrieve the current state of the mouse.
     *
     *  The current button state is returned as a button bitmask, which can
     *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
     *  mouse cursor position relative to the focus window for the currently
     *  selected mouse.  You can pass NULL for either x or y.
     *)
    SDL_GetMouseState: TSDL_GetMouseState;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Get the current state of the mouse, in relation to the desktop
     *
     *  This works just like SDL_GetMouseState(), but the coordinates will be
     *  reported relative to the top-left of the desktop. This can be useful if
     *  you need to track the mouse outside of a specific window and
     *  SDL_CaptureMouse() doesn't fit your needs. For example, it could be
     *  useful if you need to track the mouse while dragging a window, where
     *  coordinates relative to a window might not be in sync at all times.
     *
     *  \note SDL_GetMouseState() returns the mouse position as SDL understands
     *        it from the last pump of the event queue. This function, however,
     *        queries the OS for the current mouse position, and as such, might
     *        be a slightly less efficient function. Unless you know what you're
     *        doing and have a good reason to use this function, you probably want
     *        SDL_GetMouseState() instead.
     *
     *  \param x Returns the current X coord, relative to the desktop. Can be NULL.
     *  \param y Returns the current Y coord, relative to the desktop. Can be NULL.
     *  \return The current button state as a bitmask, which can be tested using the SDL_BUTTON(X) macros.
     *
     *  \sa SDL_GetMouseState
     *)
    SDL_GetGlobalMouseState: TSDL_GetGlobalMouseState;
    {$ENDIF}
    (**
     *  \brief Retrieve the relative state of the mouse.
     *
     *  The current button state is returned as a button bitmask, which can
     *  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
     *  mouse deltas since the last call to SDL_GetRelativeMouseState().
     *)
    SDL_GetRelativeMouseState: TSDL_GetRelativeMouseState;
    (**
     *  \brief Moves the mouse to the given position within the window.
     *
     *  \param window The window to move the mouse into, or NULL for the current mouse focus
     *  \param x The x coordinate within the window
     *  \param y The y coordinate within the window
     *
     *  \note This function generates a mouse motion event
     *)
    SDL_WarpMouseInWindow: TSDL_WarpMouseInWindow;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Moves the mouse to the given position in global screen space.
     *
     *  \param x The x coordinate
     *  \param y The y coordinate
     *  \return 0 on success, -1 on error (usually: unsupported by a platform).
     *
     *  \note This function generates a mouse motion event
     *)
    SDL_WarpMouseGlobal: TSDL_WarpMouseGlobal;
    {$ENDIF}
    (**
     *  \brief Set relative mouse mode.
     *
     *  \param enabled Whether or not to enable relative mode
     *
     *  \return 0 on success, or -1 if relative mode is not supported.
     *
     *  While the mouse is in relative mode, the cursor is hidden, and the
     *  driver will try to report continuous motion in the current window.
     *  Only relative motion events will be delivered, the mouse position
     *  will not change.
     *
     *  \note This function will flush any pending mouse motion.
     *
     *  \sa SDL_GetRelativeMouseMode()
     *)
    SDL_SetRelativeMouseMode: TSDL_SetRelativeMouseMode;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Capture the mouse, to track input outside an SDL window.
     *
     *  \param enabled Whether or not to enable capturing
     *
     *  Capturing enables your app to obtain mouse events globally, instead of
     *  just within your window. Not all video targets support this function.
     *  When capturing is enabled, the current window will get all mouse events,
     *  but unlike relative mode, no change is made to the cursor and it is
     *  not restrained to your window.
     *
     *  This function may also deny mouse input to other windows--both those in
     *  your application and others on the system--so you should use this
     *  function sparingly, and in small bursts. For example, you might want to
     *  track the mouse while the user is dragging something, until the user
     *  releases a mouse button. It is not recommended that you capture the mouse
     *  for long periods of time, such as the entire time your app is running.
     *
     *  While captured, mouse events still report coordinates relative to the
     *  current (foreground) window, but those coordinates may be outside the
     *  bounds of the window (including negative values). Capturing is only
     *  allowed for the foreground window. If the window loses focus while
     *  capturing, the capture will be disabled automatically.
     *
     *  While capturing is enabled, the current window will have the
     *  SDL_WINDOW_MOUSE_CAPTURE flag set.
     *
     *  \return 0 on success, or -1 if not supported.
     *)
    SDL_CaptureMouse: TSDL_CaptureMouse;
    {$ENDIF}
    (**
     *  \brief Query whether relative mouse mode is enabled.
     *
     *  \sa SDL_SetRelativeMouseMode()
     *)
    SDL_GetRelativeMouseMode: TSDL_GetRelativeMouseMode;
    (**
     *  \brief Create a cursor, using the specified bitmap data and
     *         mask (in MSB format).
     *
     *  The cursor width must be a multiple of 8 bits.
     *
     *  The cursor is created in black and white according to the following:
     *  <table>
     *  <tr><td> data </td><td> mask </td><td> resulting pixel on screen </td></tr>
     *  <tr><td>  0   </td><td>  1   </td><td> White </td></tr>
     *  <tr><td>  1   </td><td>  1   </td><td> Black </td></tr>
     *  <tr><td>  0   </td><td>  0   </td><td> Transparent </td></tr>
     *  <tr><td>  1   </td><td>  0   </td><td> Inverted color if possible, black
     *                                         if not. </td></tr>
     *  </table>
     *
     *  \sa SDL_FreeCursor()
     *)
    SDL_CreateCursor: TSDL_CreateCursor;
    (**
     *  \brief Create a color cursor.
     *
     *  \sa SDL_FreeCursor()
     *)
    SDL_CreateColorCursor: TSDL_CreateColorCursor;
    (**
     *  \brief Create a system cursor.
     *
     *  \sa SDL_FreeCursor()
     *)
    SDL_CreateSystemCursor: TSDL_CreateSystemCursor;
    (**
     *  \brief Set the active cursor.
     *)
    SDL_SetCursor: TSDL_SetCursor;
    (**
     *  \brief Return the active cursor.
     *)
    SDL_GetCursor: TSDL_GetCursor;
    (**
     *  \brief Return the default cursor.
     *)
    SDL_GetDefaultCursor: TSDL_GetDefaultCursor;
    (**
     *  \brief Frees a cursor created with SDL_CreateCursor().
     *
     *  \sa SDL_CreateCursor()
     *)
    SDL_FreeCursor: TSDL_FreeCursor;
    (**
     *  \brief Toggle whether or not the cursor is shown.
     *
     *  \param toggle 1 to show the cursor, 0 to hide it, -1 to query the current
     *                state.
     *
     *  \return 1 if the cursor is shown, or 0 if the cursor is hidden.
     *)
    SDL_ShowCursor: TSDL_ShowCursor;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    function SDL_Button(button: SDL_ButtonEnum): SDL_ButtonMasks; inline;
  end;

implementation

{ TSDLMouseApi }

procedure TSDLMouseApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetMouseFocus', @SDL_GetMouseFocus);
  List.Add('SDL_GetMouseState', @SDL_GetMouseState);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_GetGlobalMouseState', @SDL_GetGlobalMouseState, False);
  {$ENDIF}
  List.Add('SDL_GetRelativeMouseState', @SDL_GetRelativeMouseState);
  List.Add('SDL_WarpMouseInWindow', @SDL_WarpMouseInWindow);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_WarpMouseGlobal', @SDL_WarpMouseGlobal, False);
  {$ENDIF}
  List.Add('SDL_SetRelativeMouseMode', @SDL_SetRelativeMouseMode);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_CaptureMouse', @SDL_CaptureMouse, False);
  {$ENDIF}
  List.Add('SDL_GetRelativeMouseMode', @SDL_GetRelativeMouseMode);
  List.Add('SDL_CreateCursor', @SDL_CreateCursor);
  List.Add('SDL_CreateColorCursor', @SDL_CreateColorCursor);
  List.Add('SDL_CreateSystemCursor', @SDL_CreateSystemCursor);
  List.Add('SDL_SetCursor', @SDL_SetCursor);
  List.Add('SDL_GetCursor', @SDL_GetCursor);
  List.Add('SDL_GetDefaultCursor', @SDL_GetDefaultCursor);
  List.Add('SDL_FreeCursor', @SDL_FreeCursor);
  List.Add('SDL_ShowCursor', @SDL_ShowCursor);
end;

function TSDLMouseApi.SDL_Button(button: SDL_ButtonEnum): SDL_ButtonMasks;
begin
  Result := SDL_ButtonMasks(1 shl (Integer(button) - 1));
end;

end.

