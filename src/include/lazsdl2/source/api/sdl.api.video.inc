
type

  PPSDL_Window = ^PSDL_Window;
  PSDL_Window = ^SDL_Window;
  PSDL_DisplayMode = ^SDL_DisplayMode;
  PSDL_WindowUserData = ^SDL_WindowUserData;
  PSDL_WindowShaper = ^SDL_WindowShaper;

  SDL_WindowFlags = record Value: SDL_UInt32; end;

  SDL_WindowPositionMaks = (
    SDL_WINDOWPOS_UNDEFINED_MASK = $1FFF0000,
    SDL_WINDOWPOS_CENTERED_MASK = $2FFF0000
  );

  (**
   *  \brief The flags on a window
   *
   *  \sa SDL_GetWindowFlags()
   *)
  SDL_WindowFlag = (
    SDL_WINDOW_EMPTY := 0,                      (** Noething *)
    SDL_WINDOW_WINDOWED := 0{%H-},              (** windowed mode *)
    SDL_WINDOW_FULLSCREEN := $00000001,         (** fullscreen window *)
    SDL_WINDOW_OPENGL := $00000002,             (** window usable with OpenGL context *)
    SDL_WINDOW_SHOWN := $00000004,              (** window is visible *)
    SDL_WINDOW_HIDDEN := $00000008,             (** window is not visible *)
    SDL_WINDOW_BORDERLESS := $00000010,         (** no window decoration *)
    SDL_WINDOW_RESIZABLE := $00000020,          (** window can be resized *)
    SDL_WINDOW_MINIMIZED := $00000040,          (** window is minimized *)
    SDL_WINDOW_MAXIMIZED := $00000080,          (** window is maximized *)
    SDL_WINDOW_INPUT_GRABBED := $00000100,      (** window has grabbed input focus *)
    SDL_WINDOW_INPUT_FOCUS := $00000200,        (** window has input focus *)
    SDL_WINDOW_MOUSE_FOCUS := $00000400,        (** window has mouse focus *)
    SDL_WINDOW_FOREIGN := $00000800,            (** window not created by SDL *)
    SDL_WINDOW_FULLSCREEN_DESKTOP := Integer(SDL_WINDOW_FULLSCREEN) or $00001000,
    SDL_WINDOW_ALLOW_HIGHDPI = $00002000,       (** window should be created in high-DPI mode if supported *)
    SDL_WINDOW_MOUSE_CAPTURE = $00004000        (** window has mouse captured (unrelated to INPUT_GRABBED) *)
  );

  (**
   *  \brief Event subtype for window events
   *)
  {$PACKENUM 1}
  SDL_WindowEventID = (
    SDL_WINDOWEVENT_NONE,           (** Never used *)
    SDL_WINDOWEVENT_SHOWN,          (** Window has been shown *)
    SDL_WINDOWEVENT_HIDDEN,         (** Window has been hidden *)
    SDL_WINDOWEVENT_EXPOSED,        (** Window has been exposed and should be redrawn *)
    SDL_WINDOWEVENT_MOVED,          (** Window has been moved to data1, data2 *)
    SDL_WINDOWEVENT_RESIZED,        (** Window has been resized to data1xdata2 *)
    SDL_WINDOWEVENT_SIZE_CHANGED,   (** The window size has changed, either as
                                             a result of an API call or through the
                                             system or user changing the window size. *)
    SDL_WINDOWEVENT_MINIMIZED,      (** Window has been minimized *)
    SDL_WINDOWEVENT_MAXIMIZED,      (** Window has been maximized *)
    SDL_WINDOWEVENT_RESTORED,       (** Window has been restored to normal size and position *)
    SDL_WINDOWEVENT_ENTER,          (** Window has gained mouse focus *)
    SDL_WINDOWEVENT_LEAVE,          (** Window has lost mouse focus *)
    SDL_WINDOWEVENT_FOCUS_GAINED,   (** Window has gained keyboard focus *)
    SDL_WINDOWEVENT_FOCUS_LOST,     (** Window has lost keyboard focus *)
    SDL_WINDOWEVENT_CLOSE           (** The window manager requests that the window be closed *)
  );
  {$PACKENUM 4}

  (**
   *  \brief OpenGL configuration attributes
   *)
  SDL_GLattr = (
    SDL_GL_RED_SIZE,
    SDL_GL_GREEN_SIZE,
    SDL_GL_BLUE_SIZE,
    SDL_GL_ALPHA_SIZE,
    SDL_GL_BUFFER_SIZE,
    SDL_GL_DOUBLEBUFFER,
    SDL_GL_DEPTH_SIZE,
    SDL_GL_STENCIL_SIZE,
    SDL_GL_ACCUM_RED_SIZE,
    SDL_GL_ACCUM_GREEN_SIZE,
    SDL_GL_ACCUM_BLUE_SIZE,
    SDL_GL_ACCUM_ALPHA_SIZE,
    SDL_GL_STEREO,
    SDL_GL_MULTISAMPLEBUFFERS,
    SDL_GL_MULTISAMPLESAMPLES,
    SDL_GL_ACCELERATED_VISUAL,
    SDL_GL_RETAINED_BACKING,
    SDL_GL_CONTEXT_MAJOR_VERSION,
    SDL_GL_CONTEXT_MINOR_VERSION,
    SDL_GL_CONTEXT_EGL,
    SDL_GL_CONTEXT_FLAGS,
    SDL_GL_CONTEXT_PROFILE_MASK,
    SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
    SDL_GL_FRAMEBUFFER_SRGB_CAPABLE,
    SDL_GL_CONTEXT_RELEASE_BEHAVIOR
  );

  SDL_GLprofile = (
    SDL_GL_CONTEXT_PROFILE_CORE           = $0001,
    SDL_GL_CONTEXT_PROFILE_COMPATIBILITY  = $0002,
    SDL_GL_CONTEXT_PROFILE_ES             = $0004 (** GLX_CONTEXT_ES2_PROFILE_BIT_EXT *)
  );

  SDL_GLcontextFlag = (
    SDL_GL_CONTEXT_DEBUG_FLAG              = $0001,
    SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = $0002,
    SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG      = $0004,
    SDL_GL_CONTEXT_RESET_ISOLATION_FLAG    = $0008
  );

  SDL_GLcontextReleaseFlag = (
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE   = $0000,
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH  = $0001
  );

  (**
   *  \brief Possible return values from the SDL_HitTest callback.
   *
   *  \sa SDL_HitTest
   *)
  SDL_HitTestResult = (
      SDL_HITTEST_NORMAL,  (** Region is normal. No special properties. *)
      SDL_HITTEST_DRAGGABLE,  (** Region can drag entire window. *)
      SDL_HITTEST_RESIZE_TOPLEFT,
      SDL_HITTEST_RESIZE_TOP,
      SDL_HITTEST_RESIZE_TOPRIGHT,
      SDL_HITTEST_RESIZE_RIGHT,
      SDL_HITTEST_RESIZE_BOTTOMRIGHT,
      SDL_HITTEST_RESIZE_BOTTOM,
      SDL_HITTEST_RESIZE_BOTTOMLEFT,
      SDL_HITTEST_RESIZE_LEFT
  );

  (**
   *  \brief An opaque handle to an OpenGL context.
   *)
  SDL_GLContext = Pointer;

  (**
   *  \brief  The structure that defines a display mode
   *
   *  \sa SDL_GetNumDisplayModes()
   *  \sa SDL_GetDisplayMode()
   *  \sa SDL_GetDesktopDisplayMode()
   *  \sa SDL_GetCurrentDisplayMode()
   *  \sa SDL_GetClosestDisplayMode()
   *  \sa SDL_SetWindowDisplayMode()
   *  \sa SDL_GetWindowDisplayMode()
   *)
  SDL_DisplayMode = record
    format: SDL_PixelFormatEnum;     (** pixel format *)
    w: SDL_SInt32;                   (** width, in screen coordinates *)
    h: SDL_SInt32;                   (** height, in screen coordinates *)
    refresh_rate: SDL_SInt32;        (** refresh rate (or zero for unspecified) *)
    driverdata: SDL_Data;            (** driver-specific data, initialize to nil *)
  end;

  (** Define the SDL window user data structure *)
  SDL_WindowUserData = record
    name: SDL_String;
    data: SDL_Data;
    next: PSDL_WindowUserData;
  end;

  (** Define the SDL window-shaper structure *)
  SDL_WindowShaper = record
    window: PSDL_Window; (** The window associated with the shaper *)
    userx,usery: SDL_UInt32; (** The user's specified coordinates for the window, for once we give it a shape. *)
    mode: SDL_WindowShapeMode; (** The parameters for shape calculation. *)
    hasshape: SDL_Bool; (** Has this window been assigned a shape? *)
    driverdata: SDL_Data;
  end;

  (**
   *  \brief Define the SDL window structure, corresponding to toplevel windows
   *
   *  \sa SDL_CreateWindow()
   *  \sa SDL_CreateWindowFrom()
   *  \sa SDL_DestroyWindow()
   *  \sa SDL_GetWindowData()
   *  \sa SDL_GetWindowFlags()
   *  \sa SDL_GetWindowGrab()
   *  \sa SDL_GetWindowPosition()
   *  \sa SDL_GetWindowSize()
   *  \sa SDL_GetWindowTitle()
   *  \sa SDL_HideWindow()
   *  \sa SDL_MaximizeWindow()
   *  \sa SDL_MinimizeWindow()
   *  \sa SDL_RaiseWindow()
   *  \sa SDL_RestoreWindow()
   *  \sa SDL_SetWindowData()
   *  \sa SDL_SetWindowFullscreen()
   *  \sa SDL_SetWindowGrab()
   *  \sa SDL_SetWindowIcon()
   *  \sa SDL_SetWindowPosition()
   *  \sa SDL_SetWindowSize()
   *  \sa SDL_SetWindowBordered()
   *  \sa SDL_SetWindowTitle()
   *  \sa SDL_ShowWindow()
   *)
  SDL_Window = record
    magic: Pointer;
    id: SDL_UInt32;
    title: SDL_String;
    icon: PSDL_Surface;
    x,y: SDL_SInt32;
    w,h: SDL_SInt32;
    min_w, min_h: SDL_SInt32;
    max_w, max_h: SDL_SInt32;
    flags: SDL_WindowFlags;

    windowed: SDL_Rect; (** Stored position and size for windowed mode *)
    fullscreen_mode: SDL_DisplayMode;
    brightness: SDL_Float;
    gamma: PSDL_UInt16;
    saved_gamma: PSDL_UInt16;  (** just offset into gamma *)
    surface: PSDL_Surface;
    surface_valid: SDL_Bool;
    shaper: PSDL_WindowShaper;
    data: PSDL_WindowUserData;
    driverdata: Pointer;

    prev: PSDL_Window;
    next: PSDL_Window;
  end;
