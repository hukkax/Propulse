
type

  PSDL_GameController = ^SDL_GameController;

  SDL_GameControllerBindType = (
      SDL_CONTROLLER_BINDTYPE_NONE = 0,
      SDL_CONTROLLER_BINDTYPE_BUTTON,
      SDL_CONTROLLER_BINDTYPE_AXIS,
      SDL_CONTROLLER_BINDTYPE_HAT
  );

  (**
   *  The list of buttons available from a controller
   *)
  SDL_GameControllerButton = (
      SDL_CONTROLLER_BUTTON_INVALID = -1,
      SDL_CONTROLLER_BUTTON_A,
      SDL_CONTROLLER_BUTTON_B,
      SDL_CONTROLLER_BUTTON_X,
      SDL_CONTROLLER_BUTTON_Y,
      SDL_CONTROLLER_BUTTON_BACK,
      SDL_CONTROLLER_BUTTON_GUIDE,
      SDL_CONTROLLER_BUTTON_START,
      SDL_CONTROLLER_BUTTON_LEFTSTICK,
      SDL_CONTROLLER_BUTTON_RIGHTSTICK,
      SDL_CONTROLLER_BUTTON_LEFTSHOULDER,
      SDL_CONTROLLER_BUTTON_RIGHTSHOULDER,
      SDL_CONTROLLER_BUTTON_DPAD_UP,
      SDL_CONTROLLER_BUTTON_DPAD_DOWN,
      SDL_CONTROLLER_BUTTON_DPAD_LEFT,
      SDL_CONTROLLER_BUTTON_DPAD_RIGHT,
      SDL_CONTROLLER_BUTTON_MAX
  );

  (**
   *  The list of axes available from a controller
   *)
  SDL_GameControllerAxis = (
      SDL_CONTROLLER_AXIS_INVALID = -1,
      SDL_CONTROLLER_AXIS_LEFTX,
      SDL_CONTROLLER_AXIS_LEFTY,
      SDL_CONTROLLER_AXIS_RIGHTX,
      SDL_CONTROLLER_AXIS_RIGHTY,
      SDL_CONTROLLER_AXIS_TRIGGERLEFT,
      SDL_CONTROLLER_AXIS_TRIGGERRIGHT,
      SDL_CONTROLLER_AXIS_MAX
  );

  SDL_GameControllerButtonBind = record
      bindType: SDL_GameControllerBindType;
      value: record
        case SDL_SInt32 of
          0: ( button: SDL_SInt32; );
          1: ( axis: SDL_SInt32; );
          2: ( hat: record
                hat: Integer;
                hat_mask: Integer;
              end; );
      end
  end;

  (* keep track of the hat and mask value that transforms this hat movement into a button/axis press *)
  SDL_HatMapping = record
    hat: SDL_SInt32;
    mask: SDL_UInt8;
  end;

  SDL_ControllerMapping  = record
      guid: SDL_JoystickGUID;
      name: SDL_String;

      (** mapping of axis/button id to controller version *)
      axes: array [0..Integer(SDL_CONTROLLER_AXIS_MAX) - 1] of SDL_SInt32;
      buttonasaxis: array [0..Integer(SDL_CONTROLLER_AXIS_MAX) -1] of SDL_SInt32;

      buttons: array [0..iNTEGER(SDL_CONTROLLER_BUTTON_MAX) -1] of SDL_SInt32;
      axesasbutton: array [0..Integer(SDL_CONTROLLER_BUTTON_MAX) -1] of SDL_SInt32;
      hatasbutton: array [0..Integer(SDL_CONTROLLER_BUTTON_MAX) -1] of SDL_HatMapping;

      (* reverse mapping, joystick indices to buttons *)
      raxes: array [0..19] of SDL_GameControllerAxis;
      rbuttonasaxis: array [0..19] of SDL_GameControllerAxis;

      rbuttons: array [0..19] of SDL_GameControllerButton;
      raxesasbutton: array [0..19] of SDL_GameControllerButton;
      rhatasbutton: array [0..19] of SDL_GameControllerButton;
  end;

  (* The SDL game controller structure *)
  SDL_GameController = record
      joystick: PSDL_Joystick;             (** underlying joystick device *)
       ref_count: SDL_SInt32;
      hatState: array [0..3] of SDL_Uint8; (** the current hat state for this controller *)
      mapping: SDL_ControllerMapping;      (** the mapping object for this controller *)
      next: PSDL_GameController;           (** pointer to next game controller we have allocated *)
  end;
