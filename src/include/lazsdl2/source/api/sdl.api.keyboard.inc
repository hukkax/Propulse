

type

  PSDLKeyStatesArr = ^SDLKeyStatesArr;

  { SDLKeyStatesArr }

  SDLKeyStatesArr = record
  private
    Data: array[0..65000] of ByteBool;
    function GetItem(Index: SDL_Scancode): ByteBool;
  public
    property Item[Index: SDL_Scancode]: ByteBool read GetItem; default;
  end;

  (**
   *  \brief The SDL keysym structure, used in key events.
   *
   *  \note  If you are looking for translated character input, see the ::SDL_TEXTINPUT event.
   *)
  SDL_Keysym = record
      scancode: SDL_Scancode;      (** SDL physical key code - see ::SDL_Scancode for details *)
      sym: SDL_Keycode;            (** SDL virtual key code - see ::SDL_Keycode for details *)
      amod: SDL_Keymods;           (** current key modifiers *)
      unused: SDL_UInt32;
  end;

