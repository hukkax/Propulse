

type

  PSDL_Joystick = ^SDL_Joystick;

  SDL_JoystickID = SDL_SInt32;

  SDL_HatState = (
    SDL_HAT_CENTERED  = $00,
    SDL_HAT_UP        = $01,
    SDL_HAT_RIGHT     = $02,
    SDL_HAT_DOWN      = $04,
    SDL_HAT_LEFT      = $08,
    SDL_HAT_RIGHTUP   = Integer(SDL_HAT_RIGHT) or Integer(SDL_HAT_UP){%H-},
    SDL_HAT_RIGHTDOWN = Integer(SDL_HAT_RIGHT) or Integer(SDL_HAT_DOWN),
    SDL_HAT_LEFTUP    = Integer(SDL_HAT_LEFT) or Integer(SDL_HAT_UP),
    SDL_HAT_LEFTDOWN  = Integer(SDL_HAT_LEFT) or Integer(SDL_HAT_DOWN)
  );

  SDL_BSDJOY = (
      BSDJOY_UHID,            (** uhid(4) *)
      BSDJOY_JOY              (** joy(4) *)
  );

  SDL_SREPORT = (
    SREPORT_UNINIT,
    SREPORT_CLEAN,
    SREPORT_DIRTY
  );

  SDL_JoystickPowerLevel = (
      SDL_JOYSTICK_POWER_UNKNOWN = -1,
      SDL_JOYSTICK_POWER_EMPTY,
      SDL_JOYSTICK_POWER_LOW,
      SDL_JOYSTICK_POWER_MEDIUM,
      SDL_JOYSTICK_POWER_FULL,
      SDL_JOYSTICK_POWER_WIRED,
      SDL_JOYSTICK_POWER_MAX
  );

  (* The joystick structure used to identify an SDL joystick *)
  SDL_Joystick = record
    instance_id: SDL_JoystickID; (** Device instance, monotonically increasing from 0 *)
    name: SDL_String;            (** Joystick name - system dependent *)

    naxes: SDL_SInt32;           (** Number of axis controls on the joystick *)
    axes: PSDL_SInt16;           (** Current axis states *)

    nhats: SDL_SInt32;           (** Number of hats on the joystick *)
    hats: SDL_UInt8;             (** Current hat states *)

    nballs: SDL_SInt32;          (** Number of trackballs on the joystick *)
    balls: SDL_Pointer;          (** Current ball motion deltas *)

    nbuttons: SDL_SInt32;        (** Number of buttons on the joystick *)
    buttons: PSDL_UInt8;         (** Current button states *)

    hwdata: SDL_Pointer;         (** Driver dependent information *)

    ref_count: SDL_SInt32;       (** Reference count for multiple opens *)

    closed: SDL_UInt8;           (** 1 if this device is no longer valid *)
    uncentered: SDL_UInt8;       (** 1 if this device needs to have its state reset to 0 *)
    next: PSDL_Joystick;         (** pointer to next joystick we have allocated *)
  end;

  (* A structure that encodes the stable unique id for a joystick device *)
  SDL_JoystickGUID = record
    data: array[0..15] of SDL_UInt8;
  end;

