unit SDL.Api.Video;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLVideoApi }

  TSDLVideoApi = class(TSubLibrary)
  public type
    (**
     *  \brief Callback used for hit-testing.
     *
     *  \sa SDL_SetWindowHitTest
     *)
    TSDL_HitTest = function(win: PSDL_Window; area: PSDL_Point; data: SDL_Data): SDL_HitTestResult cdecl;
  private type
    TSDL_GetNumVideoDrivers = function: SDL_SInt32 cdecl;
    TSDL_GetVideoDriver = function(index: SDL_SInt32): SDL_String cdecl;
    TSDL_VideoInit = function(const driver_name: SDL_String): SDL_ResultCode cdecl;
    TSDL_VideoQuit = procedure cdecl;
    TSDL_GetCurrentVideoDriver = function: SDL_String cdecl;
    TSDL_GetNumVideoDisplays = function: SDL_SInt32 cdecl;
    TSDL_GetDisplayName = function(displayIndex: SDL_SInt32): SDL_String cdecl;
    TSDL_GetDisplayBounds = function(displayIndex: SDL_SInt32; out rect: SDL_Rect): SDL_ResultCode cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_5}
    TSDL_GetDisplayUsableBounds = function(displayIndex: SDL_SInt32; out rect: SDL_Rect): SDL_ResultCode cdecl;
    {$ENDIF}
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_GetDisplayDPI = function(displayIndex: SDL_SInt32; ddpi: PSDL_Float; hdpi: PSDL_Float; vdpi: PSDL_Float): SDL_ResultCode cdecl;
    TSDL_GetDisplayDPIEx = function(displayIndex: SDL_SInt32; out ddpi: SDL_Float; out hdpi: SDL_Float; out vdpi: SDL_Float): SDL_ResultCode cdecl;
    {$ENDIF}
    TSDL_GetNumDisplayModes = function(displayIndex: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_GetDisplayMode = function(displayIndex: SDL_SInt32; modeIndex: SDL_SInt32; out mode: SDL_DisplayMode): SDL_ResultCode cdecl;
    TSDL_GetDesktopDisplayMode = function(displayIndex: SDL_SInt32; out mode: SDL_DisplayMode): SDL_ResultCode cdecl;
    TSDL_GetCurrentDisplayMode = function(displayIndex: SDL_SInt32; out mode: SDL_DisplayMode): SDL_ResultCode cdecl;
    TSDL_GetClosestDisplayMode = function(displayIndex: SDL_SInt32; const mode: PSDL_DisplayMode; out closest: SDL_DisplayMode): PSDL_DisplayMode cdecl;
    TSDL_GetWindowDisplayIndex = function(window: PSDL_Window): SDL_SInt32 cdecl;
    TSDL_SetWindowDisplayMode = function(window: PSDL_Window; const mode: PSDL_DisplayMode): SDL_ResultCode cdecl;
    TSDL_GetWindowDisplayMode = function(window: PSDL_Window; out mode: SDL_DisplayMode): SDL_ResultCode cdecl;
    TSDL_GetWindowPixelFormat = function(window: PSDL_Window): SDL_PixelFormatEnum cdecl;
    TSDL_CreateWindow = function(const title: SDL_String; x: SDL_SInt32; y: SDL_SInt32; w: SDL_SInt32; h: SDL_SInt32; flags: SDL_WindowFlags): PSDL_Window cdecl;
    TSDL_CreateWindowFrom = function(const data: SDL_Data): PSDL_Window cdecl;
    TSDL_GetWindowID = function(window: PSDL_Window): SDL_UInt32 cdecl;
    TSDL_GetWindowFromID = function(id: SDL_UInt32): PSDL_Window cdecl;
    TSDL_GetWindowFlags = function(window: PSDL_Window): SDL_WindowFlags cdecl;
    TSDL_SetWindowTitle = procedure(window: PSDL_Window; const title: SDL_String) cdecl;
    TSDL_GetWindowTitle = function(window: PSDL_Window): SDL_String cdecl;
    TSDL_SetWindowIcon = procedure(window: PSDL_Window; icon: PSDL_Surface) cdecl;
    TSDL_SetWindowData = function(window: PSDL_Window; const name: SDL_String; userdata: SDL_Data): SDL_Data cdecl;
    TSDL_GetWindowData = function(window: PSDL_Window; const name: SDL_String): SDL_Data cdecl;
    TSDL_SetWindowPosition = procedure(window: PSDL_Window; x: SDL_SInt32; y: SDL_SInt32) cdecl;
    TSDL_GetWindowPosition = procedure(window: PSDL_Window; x: PSDL_SInt32; y: PSDL_SInt32) cdecl;
    TSDL_GetWindowPositionEx = procedure(window: PSDL_Window; out x: SDL_SInt32; out y: SDL_SInt32) cdecl;
    TSDL_SetWindowSize = procedure(window: PSDL_Window; w: SDL_SInt32; h: SDL_SInt32) cdecl;
    TSDL_GetWindowSize = procedure(window: PSDL_Window; w: PSDL_SInt32; h: PSDL_SInt32) cdecl;
    TSDL_GetWindowSizeEx = procedure(window: PSDL_Window; out w: SDL_SInt32; out h: SDL_SInt32) cdecl;
    TSDL_SetWindowMinimumSize = procedure(window: PSDL_Window; min_w: SDL_SInt32; min_h: SDL_SInt32) cdecl;
    TSDL_GetWindowMinimumSize = procedure(window: PSDL_Window; out w: SDL_SInt32; out h: SDL_SInt32) cdecl;
    TSDL_SetWindowMaximumSize = procedure(window: PSDL_Window; max_w: SDL_SInt32; max_h: SDL_SInt32) cdecl;
    TSDL_GetWindowMaximumSize = procedure(window: PSDL_Window; out w: SDL_SInt32; out h: SDL_SInt32) cdecl;
    TSDL_SetWindowBordered = procedure(window: PSDL_Window; bordered: SDL_Bool) cdecl;
    TSDL_ShowWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_HideWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_RaiseWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_MaximizeWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_MinimizeWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_RestoreWindow = procedure(window: PSDL_Window) cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_5}
	TSDL_SetWindowInputFocus = function(window: PSDL_Window): SDL_SInt32 cdecl;
    {$ENDIF}
    TSDL_SetWindowFullscreen = function(window: PSDL_Window; flags: SDL_WindowFlag): SDL_ResultCode cdecl;
    TSDL_GetWindowSurface = function(window: PSDL_Window): PSDL_Surface cdecl;
    TSDL_UpdateWindowSurface = function(window: PSDL_Window): SDL_SInt32 cdecl;
    TSDL_UpdateWindowSurfaceRects = function(window: PSDL_Window; rects: PSDL_Rect; numrects: SDL_SInt32): SDL_ResultCode cdecl;
    TSDL_SetWindowGrab = procedure(window: PSDL_Window; grabbed: SDL_Bool) cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_GetGrabbedWindow = function(): PSDL_Window cdecl;
    {$ENDIF}
    TSDL_GetWindowGrab = function(window: PSDL_Window): SDL_Bool cdecl;
    TSDL_SetWindowBrightness = function(window: PSDL_Window; brightness: SDL_Float): SDL_ResultCode cdecl;
    TSDL_GetWindowBrightness = function(window: PSDL_Window): SDL_Float cdecl;
    TSDL_SetWindowGammaRamp = function(window: PSDL_Window; const red: PSDL_UInt16; const green: PSDL_UInt16; const blue: PSDL_UInt16): SDL_ResultCode cdecl;
    TSDL_GetWindowGammaRamp = function(window: PSDL_Window; out red: SDL_UInt16; out green: SDL_UInt16; out blue: SDL_UInt16): SDL_ResultCode cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_SetWindowHitTest = function(window: PSDL_Window; callback: TSDL_HitTest; callback_data: SDL_Data): SDL_ResultCode cdecl;
    {$ENDIF}
    TSDL_DestroyWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_IsScreenSaverEnabled = function: SDL_Bool cdecl;
    TSDL_EnableScreenSaver = procedure cdecl;
    TSDL_DisableScreenSaver = procedure cdecl;
    TSDL_GL_LoadLibrary = function(const path: SDL_String): SDL_ResultCode cdecl;
    TSDL_GL_GetProcAddress = function(const proc: SDL_String): Pointer cdecl;
    TSDL_GL_UnloadLibrary = procedure cdecl;
    TSDL_GL_ExtensionSupported = function(const extension: SDL_String): SDL_Bool cdecl;
    TSDL_GL_ResetAttributes = procedure(); cdecl;
    TSDL_GL_SetAttribute = function(attr: SDL_GLattr; value: SDL_SInt32): SDL_ResultCode cdecl;
    TSDL_GL_GetAttribute = function(attr: SDL_GLattr; out value: SDL_SInt32): SDL_ResultCode cdecl;
    TSDL_GL_CreateContext = function(window: PSDL_Window): SDL_GLContext cdecl;
    TSDL_GL_MakeCurrent = function(window: PSDL_Window; context: SDL_GLContext): SDL_ResultCode cdecl;
    TSDL_GL_GetCurrentWindow = function: PSDL_Window cdecl;
    TSDL_GL_GetCurrentContext = function: SDL_GLContext cdecl;
    TSDL_GL_GetDrawableSize = procedure(window: PSDL_Window; out w: SDL_SInt32; out h: SDL_SInt32) cdecl;
    TSDL_GL_SetSwapInterval = function(interval: SDL_SwapInterval): SDL_ResultCode cdecl;
    TSDL_GL_GetSwapInterval = function: SDL_SwapInterval cdecl;
    TSDL_GL_SwapWindow = procedure(window: PSDL_Window) cdecl;
    TSDL_GL_DeleteContext = procedure(context: SDL_GLContext) cdecl;
  public
    (**
     *  \brief Get the number of video drivers compiled into SDL
     *
     *  \sa SDL_GetVideoDriver()
     *)
    SDL_GetNumVideoDrivers: TSDL_GetNumVideoDrivers;
    (**
     *   \brief Get the name of a built in video driver.
     *
     *   \note The video drivers are presented in the order in which they are
     *   normally checked during initialization.
     *
     *   \sa SDL_GetNumVideoDrivers()
     *)
    SDL_GetVideoDriver: TSDL_GetVideoDriver;
    (**
     *  \brief Initialize the video subsystem, optionally specifying a video driver.
     *
     *  \param driver_name Initialize a specific driver by name, or NULL for the
     *                     default video driver.
     *
     *  \return 0 on success, -1 on error
     *
     *  This function initializes the video subsystem; setting up a connection
     *  to the window manager, etc, and determines the available display modes
     *  and pixel formats, but does not initialize a window or graphics mode.
     *
     *  \sa SDL_VideoQuit()
     *)
    SDL_VideoInit: TSDL_VideoInit;
    (**
     *  \brief Shuts down the video subsystem.
     *
     *  This function closes all windows, and restores the original video mode.
     *
     *  \sa SDL_VideoInit()
     *)
    SDL_VideoQuit: TSDL_VideoQuit;
    (**
     *  \brief Returns the name of the currently initialized video driver.
     *
     *  \return The name of the current video driver or NULL if no driver
     *          has been initialized
     *
     *  \sa SDL_GetNumVideoDrivers()
     *  \sa SDL_GetVideoDriver()
     *)
    SDL_GetCurrentVideoDriver: TSDL_GetCurrentVideoDriver;
    (**
     *  \brief Returns the number of available video displays.
     *
     *  \sa SDL_GetDisplayBounds()
     *)
    SDL_GetNumVideoDisplays: TSDL_GetNumVideoDisplays;
    (**
     *  \brief Get the name of a display in UTF-8 encoding
     *
     *  \return The name of a display, or NULL for an invalid display index.
     *
     *  \sa SDL_GetNumVideoDisplays()
     *)
    SDL_GetDisplayName: TSDL_GetDisplayName;
    (**
     *  \brief Get the desktop area represented by a display, with the primary
     *         display located at 0,0
     *
     *  \return 0 on success, or -1 if the index is out of range.
     *
     *  \sa SDL_GetNumVideoDisplays()
     *)
    SDL_GetDisplayBounds: TSDL_GetDisplayBounds;
    {$IFNDEF DISABLE_SDL2_2_0_5}
    SDL_GetDisplayUsableBounds: TSDL_GetDisplayUsableBounds;
    {$ENDIF}

    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Get the dots/pixels-per-inch for a display
     *
     *  \note Diagonal, horizontal and vertical DPI can all be optionally
     *        returned if the parameter is non-NULL.
     *
     *  \return 0 on success, or -1 if no DPI information is available or the index is out of range.
     *
     *  \sa SDL_GetNumVideoDisplays()
     *)
    (** @{ *)
    SDL_GetDisplayDPI: TSDL_GetDisplayDPI;
    SDL_GetDisplayDPIEx: TSDL_GetDisplayDPIEx;
    (** @} *)
    {$ENDIF}
    (**
     *  \brief Returns the number of available display modes.
     *
     *  \sa SDL_GetDisplayMode()
     *)
    SDL_GetNumDisplayModes: TSDL_GetNumDisplayModes;
    (**
     *  \brief Fill in information about a specific display mode.
     *
     *  \note The display modes are sorted in this priority:
     *        \li bits per pixel -> more colors to fewer colors
     *        \li width -> largest to smallest
     *        \li height -> largest to smallest
     *        \li refresh rate -> highest to lowest
     *
     *  \sa SDL_GetNumDisplayModes()
     *)
    SDL_GetDisplayMode: TSDL_GetDisplayMode;
    (**
     *  \brief Fill in information about the desktop display mode.
     *)
    SDL_GetDesktopDisplayMode: TSDL_GetDesktopDisplayMode;
    (**
     *  \brief Fill in information about the current display mode.
     *)
    SDL_GetCurrentDisplayMode: TSDL_GetCurrentDisplayMode;
    (**
     *  \brief Get the closest match to the requested display mode.
     *
     *  \param displayIndex The index of display from which mode should be queried.
     *  \param mode The desired display mode
     *  \param closest A pointer to a display mode to be filled in with the closest
     *                 match of the available display modes.
     *
     *  \return The passed in value \c closest, or NULL if no matching video mode
     *          was available.
     *
     *  The available display modes are scanned, and \c closest is filled in with the
     *  closest mode matching the requested mode and returned.  The mode format and
     *  refresh_rate default to the desktop mode if they are 0.  The modes are
     *  scanned with size being first priority, format being second priority, and
     *  finally checking the refresh_rate.  If all the available modes are too
     *  small, then NULL is returned.
     *
     *  \sa SDL_GetNumDisplayModes()
     *  \sa SDL_GetDisplayMode()
     *)
    SDL_GetClosestDisplayMode: TSDL_GetClosestDisplayMode;
    (**
     *  \brief Get the display index associated with a window.
     *
     *  \return the display index of the display containing the center of the
     *          window, or -1 on error.
     *)
    SDL_GetWindowDisplayIndex: TSDL_GetWindowDisplayIndex;
    (**
     *  \brief Set the display mode used when a fullscreen window is visible.
     *
     *  By default the window's dimensions and the desktop format and refresh rate
     *  are used.
     *
     *  \param window The window for which the display mode should be set.
     *  \param mode The mode to use, or NULL for the default mode.
     *
     *  \return 0 on success, or -1 if setting the display mode failed.
     *
     *  \sa SDL_GetWindowDisplayMode()
     *  \sa SDL_SetWindowFullscreen()
     *)
    SDL_SetWindowDisplayMode: TSDL_SetWindowDisplayMode;
    (**
     *  \brief Fill in information about the display mode used when a fullscreen
     *         window is visible.
     *
     *  \sa SDL_SetWindowDisplayMode()
     *  \sa SDL_SetWindowFullscreen()
     *)
    SDL_GetWindowDisplayMode: TSDL_GetWindowDisplayMode;
    (**
     *  \brief Get the pixel format associated with the window.
     *)
    SDL_GetWindowPixelFormat: TSDL_GetWindowPixelFormat;
    (**
     *  \brief Create a window with the specified position, dimensions, and flags.
     *
     *  \param title The title of the window, in UTF-8 encoding.
     *  \param x     The x position of the window, ::SDL_WINDOWPOS_CENTERED, or
     *               ::SDL_WINDOWPOS_UNDEFINED.
     *  \param y     The y position of the window, ::SDL_WINDOWPOS_CENTERED, or
     *               ::SDL_WINDOWPOS_UNDEFINED.
     *  \param w     The width of the window, in screen coordinates.
     *  \param h     The height of the window, in screen coordinates.
     *  \param flags The flags for the window, a mask of any of the following:
     *               ::SDL_WINDOW_FULLSCREEN,    ::SDL_WINDOW_OPENGL,
     *               ::SDL_WINDOW_HIDDEN,        ::SDL_WINDOW_BORDERLESS,
     *               ::SDL_WINDOW_RESIZABLE,     ::SDL_WINDOW_MAXIMIZED,
     *               ::SDL_WINDOW_MINIMIZED,     ::SDL_WINDOW_INPUT_GRABBED,
     *               ::SDL_WINDOW_ALLOW_HIGHDPI.
     *
     *  \return The id of the window created, or zero if window creation failed.
     *
     *  If the window is created with the SDL_WINDOW_ALLOW_HIGHDPI flag, its size
     *  in pixels may differ from its size in screen coordinates on platforms with
     *  high-DPI support (e.g. iOS and Mac OS X). Use SDL_GetWindowSize() to query
     *  the client area's size in screen coordinates, and SDL_GL_GetDrawableSize()
     *  or SDL_GetRendererOutputSize() to query the drawable size in pixels.
     *
     *  \sa SDL_DestroyWindow()
     *)
    SDL_CreateWindow: TSDL_CreateWindow;
     (**
      *  \brief Create an SDL window from an existing native window.
      *
      *  \param data A pointer to driver-dependent window creation data
      *
      *  \return The id of the window created, or zero if window creation failed.
      *
      *  \sa SDL_DestroyWindow()
      *)
    SDL_CreateWindowFrom: TSDL_CreateWindowFrom;
    (**
     *  \brief Get the numeric ID of a window, for logging purposes.
     *)
    SDL_GetWindowID: TSDL_GetWindowID;
    (**
     *  \brief Get a window from a stored ID, or NULL if it doesn't exist.
     *)
    SDL_GetWindowFromID: TSDL_GetWindowFromID;
    (**
     *  \brief Get the window flags.
     *)
    SDL_GetWindowFlags: TSDL_GetWindowFlags;
    (**
     *  \brief Set the title of a window, in UTF-8 format.
     *
     *  \sa SDL_GetWindowTitle()
     *)
    SDL_SetWindowTitle: TSDL_SetWindowTitle;
    (**
     *  \brief Get the title of a window, in UTF-8 format.
     *
     *  \sa SDL_SetWindowTitle()
     *)
    SDL_GetWindowTitle: TSDL_GetWindowTitle;
    (**
     *  \brief Set the icon for a window.
     *
     *  \param window The window for which the icon should be set.
     *  \param icon The icon for the window.
     *)
    SDL_SetWindowIcon: TSDL_SetWindowIcon;
    (**
     *  \brief Associate an arbitrary named pointer with a window.
     *
     *  \param window   The window to associate with the pointer.
     *  \param name     The name of the pointer.
     *  \param userdata The associated pointer.
     *
     *  \return The previous value associated with 'name'
     *
     *  \note The name is case-sensitive.
     *
     *  \sa SDL_GetWindowData()
     *)
    SDL_SetWindowData: TSDL_SetWindowData;
    (**
     *  \brief Retrieve the data pointer associated with a window.
     *
     *  \param window   The window to query.
     *  \param name     The name of the pointer.
     *
     *  \return The value associated with 'name'
     *
     *  \sa SDL_SetWindowData()
     *)
    SDL_GetWindowData: TSDL_GetWindowData;
    (**
     *  \brief Set the position of a window.
     *
     *  \param window   The window to reposition.
     *  \param x        The x coordinate of the window in screen coordinates, or
     *                  ::SDL_WINDOWPOS_CENTERED or ::SDL_WINDOWPOS_UNDEFINED.
     *  \param y        The y coordinate of the window in screen coordinates, or
     *                  ::SDL_WINDOWPOS_CENTERED or ::SDL_WINDOWPOS_UNDEFINED.
     *
     *  \note The window coordinate origin is the upper left of the display.
     *
     *  \sa SDL_GetWindowPosition()
     *)
    SDL_SetWindowPosition: TSDL_SetWindowPosition;
    (**
     *  \brief Get the position of a window.
     *
     *  \param window   The window to query.
     *  \param x        Pointer to variable for storing the x position, in screen
     *                  coordinates. May be NULL.
     *  \param y        Pointer to variable for storing the y position, in screen
     *                  coordinates. May be NULL.
     *
     *  \sa SDL_SetWindowPosition()
     *)
    (* @{ *)
    SDL_GetWindowPosition: TSDL_GetWindowPosition;
    SDL_GetWindowPositionEx: TSDL_GetWindowPositionEx;
    (* @} *)
    (**
     *  \brief Set the size of a window's client area.
     *
     *  \param window   The window to resize.
     *  \param w        The width of the window, in screen coordinates. Must be >0.
     *  \param h        The height of the window, in screen coordinates. Must be >0.
     *
     *  \note You can't change the size of a fullscreen window, it automatically
     *        matches the size of the display mode.
     *
     *  The window size in screen coordinates may differ from the size in pixels, if
     *  the window was created with SDL_WINDOW_ALLOW_HIGHDPI on a platform with
     *  high-dpi support (e.g. iOS or OS X). Use SDL_GL_GetDrawableSize() or
     *  SDL_GetRendererOutputSize() to get the real client area size in pixels.
     *
     *  \sa SDL_GetWindowSize()
     *)
    SDL_SetWindowSize: TSDL_SetWindowSize;
    (**
     *  \brief Get the size of a window's client area.
     *
     *  \param window   The window to query.
     *  \param w        Pointer to variable for storing the width, in screen
     *                  coordinates. May be NULL.
     *  \param h        Pointer to variable for storing the height, in screen
     *                  coordinates. May be NULL.
     *
     *  The window size in screen coordinates may differ from the size in pixels, if
     *  the window was created with SDL_WINDOW_ALLOW_HIGHDPI on a platform with
     *  high-dpi support (e.g. iOS or OS X). Use SDL_GL_GetDrawableSize() or
     *  SDL_GetRendererOutputSize() to get the real client area size in pixels.
     *
     *  \sa SDL_SetWindowSize()
     *)
    (** @{ *)
    SDL_GetWindowSize: TSDL_GetWindowSize;
    SDL_GetWindowSizeEx: TSDL_GetWindowSizeEx;
    (** @} *)
    (**
     *  \brief Set the minimum size of a window's client area.
     *
     *  \param window    The window to set a new minimum size.
     *  \param min_w     The minimum width of the window, must be >0
     *  \param min_h     The minimum height of the window, must be >0
     *
     *  \note You can't change the minimum size of a fullscreen window, it
     *        automatically matches the size of the display mode.
     *
     *  \sa SDL_GetWindowMinimumSize()
     *  \sa SDL_SetWindowMaximumSize()
     *)
    SDL_SetWindowMinimumSize: TSDL_SetWindowMinimumSize;
    (**
     *  \brief Get the minimum size of a window's client area.
     *
     *  \param window   The window to query.
     *  \param w        Pointer to variable for storing the minimum width, may be NULL
     *  \param h        Pointer to variable for storing the minimum height, may be NULL
     *
     *  \sa SDL_GetWindowMaximumSize()
     *  \sa SDL_SetWindowMinimumSize()
     *)
    SDL_GetWindowMinimumSize: TSDL_GetWindowMinimumSize;
    (**
     *  \brief Set the maximum size of a window's client area.
     *
     *  \param window    The window to set a new maximum size.
     *  \param max_w     The maximum width of the window, must be >0
     *  \param max_h     The maximum height of the window, must be >0
     *
     *  \note You can't change the maximum size of a fullscreen window, it
     *        automatically matches the size of the display mode.
     *
     *  \sa SDL_GetWindowMaximumSize()
     *  \sa SDL_SetWindowMinimumSize()
     *)
    SDL_SetWindowMaximumSize: TSDL_SetWindowMaximumSize;
    (**
     *  \brief Get the maximum size of a window's client area.
     *
     *  \param window   The window to query.
     *  \param w        Pointer to variable for storing the maximum width, may be NULL
     *  \param h        Pointer to variable for storing the maximum height, may be NULL
     *
     *  \sa SDL_GetWindowMinimumSize()
     *  \sa SDL_SetWindowMaximumSize()
     *)
    SDL_GetWindowMaximumSize: TSDL_GetWindowMaximumSize;
    (**
     *  \brief Set the border state of a window.
     *
     *  This will add or remove the window's SDL_WINDOW_BORDERLESS flag and
     *  add or remove the border from the actual window. This is a no-op if the
     *  window's border already matches the requested state.
     *
     *  \param window The window of which to change the border state.
     *  \param bordered SDL_FALSE to remove border, SDL_TRUE to add border.
     *
     *  \note You can't change the border state of a fullscreen window.
     *
     *  \sa SDL_GetWindowFlags()
     *)
    SDL_SetWindowBordered: TSDL_SetWindowBordered;
    (**
     *  \brief Show a window.
     *
     *  \sa SDL_HideWindow()
     *)
    SDL_ShowWindow: TSDL_ShowWindow;
    (**
     *  \brief Hide a window.
     *
     *  \sa SDL_ShowWindow()
     *)
    SDL_HideWindow: TSDL_HideWindow;
    (**
     *  \brief Raise a window above other windows and set the input focus.
     *)
    SDL_RaiseWindow: TSDL_RaiseWindow;
    (**
     *  \brief Make a window as large as possible.
     *
     *  \sa SDL_RestoreWindow()
     *)
    SDL_MaximizeWindow: TSDL_MaximizeWindow;
    (**
     *  \brief Minimize a window to an iconic representation.
     *
     *  \sa SDL_RestoreWindow()
     *)
    SDL_MinimizeWindow: TSDL_MinimizeWindow;
    (**
     *  \brief Restore the size and position of a minimized or maximized window.
     *
     *  \sa SDL_MaximizeWindow()
     *  \sa SDL_MinimizeWindow()
     *)
    SDL_RestoreWindow: TSDL_RestoreWindow;
    (**
     *  \brief Set a window's fullscreen state.
     *
     *  \pram window the window to change
     *  \pram flags SDL_WINDOW_FULLSCREEN, SDL_WINDOW_FULLSCREEN_DESKTOP or 0;
     *
     *  \return 0 on success, or -1 if setting the display mode failed.
     *
     *  \sa SDL_SetWindowDisplayMode()
     *  \sa SDL_GetWindowDisplayMode()
     *)
    SDL_SetWindowFullscreen: TSDL_SetWindowFullscreen;
    {$IFNDEF DISABLE_SDL2_2_0_5}
	  {**
	   *  \brief Explicitly sets input focus to the window.
	   *
	   *  You almost certainly want SDL_RaiseWindow() instead of this function. Use
	   *  this with caution, as you might give focus to a window that's completely
	   *  obscured by other windows.
	   *
	   *  \param window The window that should get the input focus
	   *
	   *  \return 0 on success, or -1 otherwise.
	   *  \sa SDL_RaiseWindow()
	   *}
	SDL_SetWindowInputFocus: TSDL_SetWindowInputFocus;
    {$ENDIF}
    (**
     *  \brief Get the SDL surface associated with the window.
     *
     *  \return The window's framebuffer surface, or NULL on error.
     *
     *  A new surface will be created with the optimal format for the window,
     *  if necessary. This surface will be freed when the window is destroyed.
     *
     *  \note You may not combine this with 3D or the rendering API on this window.
     *
     *  \sa SDL_UpdateWindowSurface()
     *  \sa SDL_UpdateWindowSurfaceRects()
     *)
    SDL_GetWindowSurface: TSDL_GetWindowSurface;
    (**
     *  \brief Copy the window surface to the screen.
     *
     *  \return 0 on success, or -1 on error.
     *
     *  \sa SDL_GetWindowSurface()
     *  \sa SDL_UpdateWindowSurfaceRects()
     *)
    SDL_UpdateWindowSurface: TSDL_UpdateWindowSurface;
    (**
     *  \brief Copy a number of rectangles on the window surface to the screen.
     *
     *  \return 0 on success, or -1 on error.
     *
     *  \sa SDL_GetWindowSurface()
     *  \sa SDL_UpdateWindowSurfaceRect()
     *)
    SDL_UpdateWindowSurfaceRects: TSDL_UpdateWindowSurfaceRects;
    (**
     *  \brief Set a window's input grab mode.
     *
     *  \param window The window for which the input grab mode should be set.
     *  \param grabbed This is SDL_TRUE to grab input, and SDL_FALSE to release input.
     *
     *  If the caller enables a grab while another window is currently grabbed,
     *  the other window loses its grab in favor of the caller's window.
     *
     *  \sa SDL_GetWindowGrab()
     *)
    SDL_SetWindowGrab: TSDL_SetWindowGrab;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Get the window that currently has an input grab enabled.
     *
     *  \return This returns the window if input is grabbed, and NULL otherwise.
     *
     *  \sa SDL_SetWindowGrab()
     *)
    SDL_GetGrabbedWindow: TSDL_GetGrabbedWindow;
    {$ENDIF}
    (**
     *  \brief Get a window's input grab mode.
     *
     *  \return This returns SDL_TRUE if input is grabbed, and SDL_FALSE otherwise.
     *
     *  \sa SDL_SetWindowGrab()
     *)
    SDL_GetWindowGrab: TSDL_GetWindowGrab;
    (**
     *  \brief Set the brightness (gamma correction) for a window.
     *
     *  \return 0 on success, or -1 if setting the brightness isn't supported.
     *
     *  \sa SDL_GetWindowBrightness()
     *  \sa SDL_SetWindowGammaRamp()
     *)
    SDL_SetWindowBrightness: TSDL_SetWindowBrightness;
    (**
     *  \brief Get the brightness (gamma correction) for a window.
     *
     *  \return The last brightness value passed to SDL_SetWindowBrightness()
     *
     *  \sa SDL_SetWindowBrightness()
     *)
    SDL_GetWindowBrightness: TSDL_GetWindowBrightness;
    (**
     *  \brief Set the gamma ramp for a window.
     *
     *  \param window The window for which the gamma ramp should be set.
     *  \param red The translation table for the red channel, or NULL.
     *  \param green The translation table for the green channel, or NULL.
     *  \param blue The translation table for the blue channel, or NULL.
     *
     *  \return 0 on success, or -1 if gamma ramps are unsupported.
     *
     *  Set the gamma translation table for the red, green, and blue channels
     *  of the video hardware.  Each table is an array of 256 16-bit quantities,
     *  representing a mapping between the input and output for that channel.
     *  The input is the index into the array, and the output is the 16-bit
     *  gamma value at that index, scaled to the output color precision.
     *
     *  \sa SDL_GetWindowGammaRamp()
     *)
    SDL_SetWindowGammaRamp: TSDL_SetWindowGammaRamp;
    (**
     *  \brief Get the gamma ramp for a window.
     *
     *  \param window The window from which the gamma ramp should be queried.
     *  \param red   A pointer to a 256 element array of 16-bit quantities to hold
     *               the translation table for the red channel, or NULL.
     *  \param green A pointer to a 256 element array of 16-bit quantities to hold
     *               the translation table for the green channel, or NULL.
     *  \param blue  A pointer to a 256 element array of 16-bit quantities to hold
     *               the translation table for the blue channel, or NULL.
     *
     *  \return 0 on success, or -1 if gamma ramps are unsupported.
     *
     *  \sa SDL_SetWindowGammaRamp()
     *)
    SDL_GetWindowGammaRamp: TSDL_GetWindowGammaRamp;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  \brief Provide a callback that decides if a window region has special properties.
     *
     *  Normally windows are dragged and resized by decorations provided by the
     *  system window manager (a title bar, borders, etc), but for some apps, it
     *  makes sense to drag them from somewhere else inside the window itself; for
     *  example, one might have a borderless window that wants to be draggable
     *  from any part, or simulate its own title bar, etc.
     *
     *  This function lets the app provide a callback that designates pieces of
     *  a given window as special. This callback is run during event processing
     *  if we need to tell the OS to treat a region of the window specially; the
     *  use of this callback is known as "hit testing."
     *
     *  Mouse input may not be delivered to your application if it is within
     *  a special area; the OS will often apply that input to moving the window or
     *  resizing the window and not deliver it to the application.
     *
     *  Specifying NULL for a callback disables hit-testing. Hit-testing is
     *  disabled by default.
     *
     *  Platforms that don't support this functionality will return -1
     *  unconditionally, even if you're attempting to disable hit-testing.
     *
     *  Your callback may fire at any time, and its firing does not indicate any
     *  specific behavior (for example, on Windows, this certainly might fire
     *  when the OS is deciding whether to drag your window, but it fires for lots
     *  of other reasons, too, some unrelated to anything you probably care about
     *  _and when the mouse isn't actually at the location it is testing_).
     *  Since this can fire at any time, you should try to keep your callback
     *  efficient, devoid of allocations, etc.
     *
     *  \param window The window to set hit-testing on.
     *  \param callback The callback to call when doing a hit-test.
     *  \param callback_data An app-defined void pointer passed to the callback.
     *  \return 0 on success, -1 on error (including unsupported).
     *)
    SDL_SetWindowHitTest: TSDL_SetWindowHitTest;
    {$ENDIF}
    (**
     *  \brief Destroy a window.
     *)
    SDL_DestroyWindow: TSDL_DestroyWindow;
    (**
     *  \brief Returns whether the screensaver is currently enabled (default on).
     *
     *  \sa SDL_EnableScreenSaver()
     *  \sa SDL_DisableScreenSaver()
     *)
    SDL_IsScreenSaverEnabled: TSDL_IsScreenSaverEnabled;
    (**
     *  \brief Allow the screen to be blanked by a screensaver
     *
     *  \sa SDL_IsScreenSaverEnabled()
     *  \sa SDL_DisableScreenSaver()
     *)
    SDL_EnableScreenSaver: TSDL_EnableScreenSaver;
    (**
     *  \brief Prevent the screen from being blanked by a screensaver
     *
     *  \sa SDL_IsScreenSaverEnabled()
     *  \sa SDL_EnableScreenSaver()
     *)
    SDL_DisableScreenSaver: TSDL_DisableScreenSaver;
    (**
     *  \brief Dynamically load an OpenGL library.
     *
     *  \param path The platform dependent OpenGL library name, or NULL to open the
     *              default OpenGL library.
     *
     *  \return 0 on success, or -1 if the library couldn't be loaded.
     *
     *  This should be done after initializing the video driver, but before
     *  creating any OpenGL windows.  If no OpenGL library is loaded, the default
     *  library will be loaded upon creation of the first OpenGL window.
     *
     *  \note If you do this, you need to retrieve all of the GL functions used in
     *        your program from the dynamic library using SDL_GL_GetProcAddress().
     *
     *  \sa SDL_GL_GetProcAddress()
     *  \sa SDL_GL_UnloadLibrary()
     *)
    SDL_GL_LoadLibrary: TSDL_GL_LoadLibrary;
    (**
     *  \brief Get the address of an OpenGL function.
     *)
    SDL_GL_GetProcAddress: TSDL_GL_GetProcAddress;
    (**
     *  \brief Unload the OpenGL library previously loaded by SDL_GL_LoadLibrary().
     *
     *  \sa SDL_GL_LoadLibrary()
     *)
    SDL_GL_UnloadLibrary: TSDL_GL_UnloadLibrary;
    (**
     *  \brief Return true if an OpenGL extension is supported for the current
     *         context.
     *)
    SDL_GL_ExtensionSupported: TSDL_GL_ExtensionSupported;
    (**
     *  \brief Reset all previously set OpenGL context attributes to their default values
     *)
    SDL_GL_ResetAttributes: TSDL_GL_ResetAttributes;
    (**
     *  \brief Set an OpenGL window attribute before window creation.
     *)
    SDL_GL_SetAttribute: TSDL_GL_SetAttribute;
    (**
     *  \brief Get the actual value for an attribute from the current context.
     *)
    SDL_GL_GetAttribute: TSDL_GL_GetAttribute;
    (**
     *  \brief Create an OpenGL context for use with an OpenGL window, and make it
     *         current.
     *
     *  \sa SDL_GL_DeleteContext()
     *)
    SDL_GL_CreateContext: TSDL_GL_CreateContext;
    (**
     *  \brief Set up an OpenGL context for rendering into an OpenGL window.
     *
     *  \note The context must have been created with a compatible window.
     *)
    SDL_GL_MakeCurrent: TSDL_GL_MakeCurrent;
    (**
     *  \brief Get the currently active OpenGL window.
     *)
    SDL_GL_GetCurrentWindow: TSDL_GL_GetCurrentWindow;
    (**
     *  \brief Get the currently active OpenGL context.
     *)
    SDL_GL_GetCurrentContext: TSDL_GL_GetCurrentContext;
    (**
     *  \brief Get the size of a window's underlying drawable in pixels (for use
     *         with glViewport).
     *
     *  \param window   Window from which the drawable size should be queried
     *  \param w        Pointer to variable for storing the width in pixels, may be NULL
     *  \param h        Pointer to variable for storing the height in pixels, may be NULL
     *
     * This may differ from SDL_GetWindowSize() if we're rendering to a high-DPI
     * drawable, i.e. the window was created with SDL_WINDOW_ALLOW_HIGHDPI on a
     * platform with high-DPI support (Apple calls this "Retina"), and not disabled
     * by the SDL_HINT_VIDEO_HIGHDPI_DISABLED hint.
     *
     *  \sa SDL_GetWindowSize()
     *  \sa SDL_CreateWindow()
     *)
    SDL_GL_GetDrawableSize: TSDL_GL_GetDrawableSize;
    (**
     *  \brief Set the swap interval for the current OpenGL context.
     *
     *  \param interval 0 for immediate updates, 1 for updates synchronized with the
     *                  vertical retrace. If the system supports it, you may
     *                  specify -1 to allow late swaps to happen immediately
     *                  instead of waiting for the next retrace.
     *
     *  \return 0 on success, or -1 if setting the swap interval is not supported.
     *
     *  \sa SDL_GL_GetSwapInterval()
     *)
    SDL_GL_SetSwapInterval: TSDL_GL_SetSwapInterval;
    (**
     *  \brief Get the swap interval for the current OpenGL context.
     *
     *  \return 0 if there is no vertical retrace synchronization, 1 if the buffer
     *          swap is synchronized with the vertical retrace, and -1 if late
     *          swaps happen immediately instead of waiting for the next retrace.
     *          If the system can't determine the swap interval, or there isn't a
     *          valid current context, this will return 0 as a safe default.
     *
     *  \sa SDL_GL_SetSwapInterval()
     *)
    SDL_GL_GetSwapInterval: TSDL_GL_GetSwapInterval;
    (**
     * \brief Swap the OpenGL buffers for a window, if double-buffering is
     *        supported.
     *)
    SDL_GL_SwapWindow: TSDL_GL_SwapWindow;
    (**
     *  \brief Delete an OpenGL context.
     *
     *  \sa SDL_GL_CreateContext()
     *)
    SDL_GL_DeleteContext: TSDL_GL_DeleteContext;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;

  public
    (**
     *  \brief Used to indicate that you don't care what the window position is.
     *)
    (* @{ *)
    function SDL_WINDOWPOS_UNDEFINED_DISPLAY(const X: SDL_SInt32): SDL_SInt32;
    function SDL_WINDOWPOS_UNDEFINED: SDL_SInt32;
    function SDL_WINDOWPOS_ISUNDEFINED(const X: SDL_SInt32): Boolean;
    (* @} *)

    (**
     *  \brief Used to indicate that the window position should be centered.
     *)
    (* @{ *)
    function SDL_WINDOWPOS_CENTERED: SDL_SInt32;
    function SDL_WINDOWPOS_CENTERED_DISPLAY(const X: SDL_SInt32): SDL_SInt32;
    function SDL_WINDOWPOS_ISCENTERED(const X: SDL_SInt32): Boolean;
    (* @} *)
  end;

implementation

{ TSDLVideoApi }

procedure TSDLVideoApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetNumVideoDrivers', @SDL_GetNumVideoDrivers);
  List.Add('SDL_GetVideoDriver', @SDL_GetVideoDriver);
  List.Add('SDL_VideoInit', @SDL_VideoInit);
  List.Add('SDL_VideoQuit', @SDL_VideoQuit);
  List.Add('SDL_GetCurrentVideoDriver', @SDL_GetCurrentVideoDriver);
  List.Add('SDL_GetNumVideoDisplays', @SDL_GetNumVideoDisplays);
  List.Add('SDL_GetDisplayName', @SDL_GetDisplayName);
  List.Add('SDL_GetDisplayBounds', @SDL_GetDisplayBounds);
  {$IFNDEF DISABLE_SDL2_2_0_5}
  List.Add('SDL_GetDisplayUsableBounds', @SDL_GetDisplayUsableBounds, False);
  {$ENDIF}
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_GetDisplayDPI', @SDL_GetDisplayDPI, False);
  List.Add('SDL_GetDisplayDPI', @SDL_GetDisplayDPIEx, False);
  {$ENDIF}
  List.Add('SDL_GetNumDisplayModes', @SDL_GetNumDisplayModes);
  List.Add('SDL_GetDisplayMode', @SDL_GetDisplayMode);
  List.Add('SDL_GetDesktopDisplayMode', @SDL_GetDesktopDisplayMode);
  List.Add('SDL_GetCurrentDisplayMode', @SDL_GetCurrentDisplayMode);
  List.Add('SDL_GetClosestDisplayMode', @SDL_GetClosestDisplayMode);
  List.Add('SDL_GetWindowDisplayIndex', @SDL_GetWindowDisplayIndex);
  List.Add('SDL_SetWindowDisplayMode', @SDL_SetWindowDisplayMode);
  List.Add('SDL_GetWindowDisplayMode', @SDL_GetWindowDisplayMode);
  List.Add('SDL_GetWindowPixelFormat', @SDL_GetWindowPixelFormat);
  List.Add('SDL_CreateWindow', @SDL_CreateWindow);
  List.Add('SDL_CreateWindowFrom', @SDL_CreateWindowFrom);
  List.Add('SDL_GetWindowID', @SDL_GetWindowID);
  List.Add('SDL_GetWindowFromID', @SDL_GetWindowFromID);
  List.Add('SDL_GetWindowFlags', @SDL_GetWindowFlags);
  List.Add('SDL_SetWindowTitle', @SDL_SetWindowTitle);
  List.Add('SDL_GetWindowTitle', @SDL_GetWindowTitle);
  List.Add('SDL_SetWindowIcon', @SDL_SetWindowIcon);
  List.Add('SDL_SetWindowData', @SDL_SetWindowData);
  List.Add('SDL_GetWindowData', @SDL_GetWindowData);
  List.Add('SDL_SetWindowPosition', @SDL_SetWindowPosition);
  List.Add('SDL_GetWindowPosition', @SDL_GetWindowPosition);
  List.Add('SDL_GetWindowPosition', @SDL_GetWindowPositionEx);
  List.Add('SDL_SetWindowSize', @SDL_SetWindowSize);
  List.Add('SDL_GetWindowSize', @SDL_GetWindowSize);
  List.Add('SDL_GetWindowSize', @SDL_GetWindowSizeEx);
  List.Add('SDL_SetWindowMinimumSize', @SDL_SetWindowMinimumSize);
  List.Add('SDL_GetWindowMinimumSize', @SDL_GetWindowMinimumSize);
  List.Add('SDL_SetWindowMaximumSize', @SDL_SetWindowMaximumSize);
  List.Add('SDL_GetWindowMaximumSize', @SDL_GetWindowMaximumSize);
  List.Add('SDL_SetWindowBordered', @SDL_SetWindowBordered);
  List.Add('SDL_ShowWindow', @SDL_ShowWindow);
  List.Add('SDL_HideWindow', @SDL_HideWindow);
  List.Add('SDL_RaiseWindow', @SDL_RaiseWindow);
  List.Add('SDL_MaximizeWindow', @SDL_MaximizeWindow);
  List.Add('SDL_MinimizeWindow', @SDL_MinimizeWindow);
  List.Add('SDL_RestoreWindow', @SDL_RestoreWindow);
  List.Add('SDL_SetWindowFullscreen', @SDL_SetWindowFullscreen);
  List.Add('SDL_GetWindowSurface', @SDL_GetWindowSurface);
  List.Add('SDL_UpdateWindowSurface', @SDL_UpdateWindowSurface);
  List.Add('SDL_UpdateWindowSurfaceRects', @SDL_UpdateWindowSurfaceRects);
  List.Add('SDL_SetWindowGrab', @SDL_SetWindowGrab);
  List.Add('SDL_GetWindowGrab', @SDL_GetWindowGrab);
  {$IFNDEF DISABLE_SDL2_2_0_5}
  List.Add('SDL_SetWindowInputFocus', @SDL_SetWindowInputFocus, False);
  {$ENDIF}
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_GetGrabbedWindow', @SDL_GetGrabbedWindow, False);
  {$ENDIF}
  List.Add('SDL_SetWindowBrightness', @SDL_SetWindowBrightness);
  List.Add('SDL_GetWindowBrightness', @SDL_GetWindowBrightness);
  List.Add('SDL_SetWindowGammaRamp', @SDL_SetWindowGammaRamp);
  List.Add('SDL_GetWindowGammaRamp', @SDL_GetWindowGammaRamp);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_SetWindowHitTest', @SDL_SetWindowHitTest, False);
  {$ENDIF}
  List.Add('SDL_DestroyWindow', @SDL_DestroyWindow);
  List.Add('SDL_IsScreenSaverEnabled', @SDL_IsScreenSaverEnabled);
  List.Add('SDL_EnableScreenSaver', @SDL_EnableScreenSaver);
  List.Add('SDL_DisableScreenSaver', @SDL_DisableScreenSaver);
  List.Add('SDL_GL_LoadLibrary', @SDL_GL_LoadLibrary);
  List.Add('SDL_GL_GetProcAddress', @SDL_GL_GetProcAddress);
  List.Add('SDL_GL_UnloadLibrary', @SDL_GL_UnloadLibrary);
  List.Add('SDL_GL_ExtensionSupported', @SDL_GL_ExtensionSupported);
  List.Add('SDL_GL_ResetAttributes', @SDL_GL_ResetAttributes);
  List.Add('SDL_GL_SetAttribute', @SDL_GL_SetAttribute);
  List.Add('SDL_GL_GetAttribute', @SDL_GL_GetAttribute);
  List.Add('SDL_GL_CreateContext', @SDL_GL_CreateContext);
  List.Add('SDL_GL_MakeCurrent', @SDL_GL_MakeCurrent);
  List.Add('SDL_GL_GetCurrentWindow', @SDL_GL_GetCurrentWindow);
  List.Add('SDL_GL_GetCurrentContext', @SDL_GL_GetCurrentContext);
  List.Add('SDL_GL_GetDrawableSize', @SDL_GL_GetDrawableSize);
  List.Add('SDL_GL_SetSwapInterval', @SDL_GL_SetSwapInterval);
  List.Add('SDL_GL_GetSwapInterval', @SDL_GL_GetSwapInterval);
  List.Add('SDL_GL_SwapWindow', @SDL_GL_SwapWindow);
  List.Add('SDL_GL_DeleteContext', @SDL_GL_DeleteContext);
end;

function TSDLVideoApi.SDL_WINDOWPOS_UNDEFINED_DISPLAY(const X: SDL_SInt32): SDL_SInt32;
begin
  Result := Ord(SDL_WINDOWPOS_UNDEFINED_MASK) or X
end;

function TSDLVideoApi.SDL_WINDOWPOS_UNDEFINED: SDL_SInt32;
begin
  Result := SDL_WINDOWPOS_UNDEFINED_DISPLAY(0)
end;

function TSDLVideoApi.SDL_WINDOWPOS_ISUNDEFINED(const X: SDL_SInt32): Boolean;
begin
  Result := (X and $FFFF0000) = Ord(SDL_WINDOWPOS_UNDEFINED_MASK)
end;

function TSDLVideoApi.SDL_WINDOWPOS_CENTERED: SDL_SInt32;
begin
  Result := SDL_WINDOWPOS_CENTERED_DISPLAY(0)
end;

function TSDLVideoApi.SDL_WINDOWPOS_CENTERED_DISPLAY(const X: SDL_SInt32): SDL_SInt32;
begin
  Result := Ord(SDL_WINDOWPOS_CENTERED_MASK) or X
end;

function TSDLVideoApi.SDL_WINDOWPOS_ISCENTERED(const X: SDL_SInt32): Boolean;
begin
  Result := (X and $FFFF0000) = Ord(SDL_WINDOWPOS_CENTERED_MASK)
end;

end.

