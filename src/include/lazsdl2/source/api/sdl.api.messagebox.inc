

type

  PSDL_MessageBoxColorScheme = ^SDL_MessageBoxColorScheme;
  PSDL_MessageBoxButtonData = ^SDL_MessageBoxButtonData;
  PSDL_MessageBoxData = ^SDL_MessageBoxData;

  (**
   * \brief SDL_MessageBox flags. If supported will display warning icon, etc.
   *)
  SDL_MessageBoxFlags = (
      SDL_MESSAGEBOX_ERROR        = $00000010,   (** error dialog *)
      SDL_MESSAGEBOX_WARNING      = $00000020,   (** warning dialog *)
      SDL_MESSAGEBOX_INFORMATION  = $00000040    (** informational dialog *)
  );

  (**
   * \brief Flags for SDL_MessageBoxButtonData.
   *)
  SDL_MessageBoxButtonFlags = (
      SDL_MESSAGEBOX_BUTTON_NONE              = 0,
      SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $0000001,   (** Marks the default button when return is hit *)
      SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002   (** Marks the default button when escape is hit *)
  );

  SDL_MessageBoxColorType = (
    SDL_MESSAGEBOX_COLOR_BACKGROUND,
    SDL_MESSAGEBOX_COLOR_TEXT,
    SDL_MESSAGEBOX_COLOR_BUTTON_BORDER,
    SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND,
    SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED,
    SDL_MESSAGEBOX_COLOR_MAX
  );

  (**
   *  \brief Individual button data.
   *)
  SDL_MessageBoxButtonData = record
      flags: SDL_MessageBoxButtonFlags; (** ::SDL_MessageBoxButtonFlags *)
      buttonid: SDL_SInt32;             (** User defined button id (value returned via SDL_ShowMessageBox) *)
      text: SDL_String;                 (** The UTF-8 button text *)
  end;

  (**
   * \brief RGB value used in a message box color scheme
   *)
  SDL_MessageBoxColor = record
      r: SDL_UInt8;
      g: SDL_UInt8;
      b: SDL_UInt8;
  end;

  (**
   * \brief A set of colors to use for message box dialogs
   *)
  SDL_MessageBoxColorScheme = record
      colors: array [SDL_MessageBoxColorType] of SDL_MessageBoxColor
  end;

  (**
   *  \brief MessageBox structure containing title, text, window, etc.
   *)
  SDL_MessageBoxData = record
      flags: SDL_MessageBoxFlags;                (** ::SDL_MessageBoxFlags *)
      window: PSDL_Window;                       (** Parent window, can be NULL *)
      title: SDL_String;                         (** UTF-8 title *)
      message: SDL_String;                       (** UTF-8 message text *)
      numbuttons: SDL_SInt32;
      buttons: PSDL_MessageBoxButtonData;
      colorScheme: PSDL_MessageBoxColorScheme;   (** ::SDL_MessageBoxColorScheme, can be NULL to use system settings *)
  end;

