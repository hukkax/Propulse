

type

  PSDL_Cursor = ^SDL_Cursor;

  {$PackEnum 1}
  SDL_ButtonEnum = (
    SDL_BUTTON_LEFT	= 1,
    SDL_BUTTON_MIDDLE	= 2,
    SDL_BUTTON_RIGHT	= 3,
    SDL_BUTTON_X1	= 4,
    SDL_BUTTON_X2	= 5
  );
  {$PackEnum 4}

  SDL_ButtonMasks = (
    SDL_BUTTON_LMASK  = 1 shl ((Integer(SDL_BUTTON_LEFT)) - 1),
    SDL_BUTTON_MMASK  = 1 shl ((Integer(SDL_BUTTON_MIDDLE)) - 1),
    SDL_BUTTON_RMASK  = 1 shl ((Integer(SDL_BUTTON_RIGHT)) - 1),
    SDL_BUTTON_X1MASK = 1 shl ((Integer(SDL_BUTTON_X1)) - 1),
    SDL_BUTTON_X2MASK = 1 shl ((Integer(SDL_BUTTON_X2)) - 1)
  );

  {$PackEnum 1}
  (** General keyboard/mouse state definitions *)
  SDL_ButtonState = (
    SDL_RELEASED,
    SDL_PRESSED
  );
  {$PackEnum 4}

  (**
   * \brief Cursor types for SDL_CreateSystemCursor.
   *)
  SDL_SystemCursor = (
    SDL_SYSTEM_CURSOR_ARROW,     (** Arrow *)
    SDL_SYSTEM_CURSOR_IBEAM,     (** I-beam *)
    SDL_SYSTEM_CURSOR_WAIT,      (** Wait *)
    SDL_SYSTEM_CURSOR_CROSSHAIR, (** Crosshair *)
    SDL_SYSTEM_CURSOR_WAITARROW, (** Small wait cursor (or Wait if not available) *)
    SDL_SYSTEM_CURSOR_SIZENWSE,  (** Double arrow pointing northwest and southeast *)
    SDL_SYSTEM_CURSOR_SIZENESW,  (** Double arrow pointing northeast and southwest *)
    SDL_SYSTEM_CURSOR_SIZEWE,    (** Double arrow pointing west and east *)
    SDL_SYSTEM_CURSOR_SIZENS,    (** Double arrow pointing north and south *)
    SDL_SYSTEM_CURSOR_SIZEALL,   (** Four pointed arrow pointing north, south, east, and west *)
    SDL_SYSTEM_CURSOR_NO,        (** Slashed circle or crossbones *)
    SDL_SYSTEM_CURSOR_HAND,      (** Hand *)
    SDL_NUM_SYSTEM_CURSORS
  );

  (**
   * \brief Scroll direction types for the Scroll event
   *)
  SDL_MouseWheelDirection = (
      SDL_MOUSEWHEEL_NORMAL,    (** The scroll direction is normal *)
      SDL_MOUSEWHEEL_FLIPPED    (** The scroll direction is flipped / natural *)
  );

  SDL_CursorAction = (
    SDL_Cursor_State = -1,
    SDL_Cursor_Hide = 0,
    SDL_Cursor_Show = 1
  );

  SDL_CursorState = (
    SDL_Cursor_Hidden = 0,
    SDL_Cursor_Visible = 1
  );

  (* Implementation dependent *)
  SDL_Cursor = record end;

