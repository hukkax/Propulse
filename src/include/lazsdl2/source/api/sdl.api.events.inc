
type

  PSDL_Keysym = ^SDL_Keysym;
  PSDL_SysWMEvent = ^SDL_SysWMEvent;
  PSDL_Event = ^SDL_Event;

  (**
   * \brief The types of events that can be delivered.
   *)
  SDL_EventType = (
    SDL_INVALID := -1,
    SDL_FIRSTEVENT := 0,     (** Unused in peek events to specify first event type *)

    // Application events
    SDL_QUITEV := $100, (** User-requested quit (originally SDL_QUIT, but changed, cause theres a method called SDL_QUIT) *)

    // These application events have special meaning on iOS, see README-ios.md for details
    SDL_APP_TERMINATING,        (** The application is being terminated by the OS
                                     Called on iOS in applicationWillTerminate()
                                     Called on Android in onDestroy()
                                *)
    SDL_APP_LOWMEMORY,          (** The application is low on memory, free memory if possible.
                                     Called on iOS in applicationDidReceiveMemoryWarning()
                                     Called on Android in onLowMemory()
                                *)
    SDL_APP_WILLENTERBACKGROUND, (** The application is about to enter the background
                                     Called on iOS in applicationWillResignActive()
                                     Called on Android in onPause()
                                *)
    SDL_APP_DIDENTERBACKGROUND, (** The application did enter the background and may not get CPU for some time
                                     Called on iOS in applicationDidEnterBackground()
                                     Called on Android in onPause()
                                *)
    SDL_APP_WILLENTERFOREGROUND, (** The application is about to enter the foreground
                                     Called on iOS in applicationWillEnterForeground()
                                     Called on Android in onResume()
                                *)
    SDL_APP_DIDENTERFOREGROUND, (** The application is now interactive
                                     Called on iOS in applicationDidBecomeActive()
                                     Called on Android in onResume()
                                  *)
    // Window events
    SDL_WINDOWEVENT_EV := $200, (** Window state change *)
    SDL_SYSWMEVENT_EV, (** System specific event *)

    //Keyboard events
    SDL_KEYDOWN := $300, (** Key pressed *)
    SDL_KEYUP, (** Key released *)
    SDL_TEXTEDITING, (** Keyboard text editing (composition) *)
    SDL_TEXTINPUT, (** Keyboard text input *)
    SDL_KEYMAPCHANGED, (** Keymap changed due to a system event such as an
                                     input language or keyboard layout change.
                         *)

    // Mouse events
    SDL_MOUSEMOTION := $400, (** Mouse moved *)
    SDL_MOUSEBUTTONDOWN, (** Mouse button pressed *)
    SDL_MOUSEBUTTONUP, (** Mouse button released *)
    SDL_MOUSEWHEEL, (** Mouse wheel motion *)

    // Joystick events
    SDL_JOYAXISMOTION := $600, (** Joystick axis motion *)
    SDL_JOYBALLMOTION, (** Joystick trackball motion *)
    SDL_JOYHATMOTION , (** Joystick hat position change *)
    SDL_JOYBUTTONDOWN, (** Joystick button pressed *)
    SDL_JOYBUTTONUP, (** Joystick button released *)
    SDL_JOYDEVICEADDED, (** A new joystick has been inserted into the system *)
    SDL_JOYDEVICEREMOVED, (** An opened joystick has been removed *)

    // Game controller events
    SDL_CONTROLLERAXISMOTION = $650, (** Game controller axis motion *)
    SDL_CONTROLLERBUTTONDOWN , (** Game controller button pressed *)
    SDL_CONTROLLERBUTTONUP, (** Game controller button released *)
    SDL_CONTROLLERDEVICEADDED, (** A new Game controller has been inserted into the system *)
    SDL_CONTROLLERDEVICEREMOVED, (** An opened Game controller has been removed *)
    SDL_CONTROLLERDEVICEREMAPPED, (** The controller mapping was updated *)

    // Touch events
    SDL_FINGERDOWN := $700,
    SDL_FINGERUP,
    SDL_FINGERMOTION,

    // Gesture events
    SDL_DOLLARGESTURE := $800,
    SDL_DOLLARRECORD,
    SDL_MULTIGESTURE,

    // Clipboard events
    SDL_CLIPBOARDUPDATE = $900, (** The clipboard changed *)

    // Drag and drop events
    SDL_DROPFILE := $1000, (** The system requests a file open *)

    // Audio hotplug events
    SDL_AUDIODEVICEADDED = $1100, (** A new audio device is available *)
    SDL_AUDIODEVICEREMOVED,        (** An audio device has been removed. *)

    // Render events
    SDL_RENDER_TARGETS_RESET = $2000, (** The render targets have been reset and their contents need to be updated *)
    SDL_RENDER_DEVICE_RESET, (** The device has been reset and all textures need to be recreated *)

    SDL_USEREVENT_EV := $8000,     (** Events ::SDL_USEREVENT through ::SDL_LASTEVENT are for your use,
                                     *  and should be allocated with SDL_RegisterEvents()
                                     *)
    SDL_LASTEVENT := $FFFF  (**
                              *  This last event is only for bounding internal arrays
                              *)
  );
  SDL_EventTypes = record Value: array of SDL_EventType; end;

  SDL_EventAction = (
    SDL_ADDEVENT,
    SDL_PEEKEVENT,
    SDL_GETEVENT
  );

  SDL_EventState = (
    SDL_QUERY := -1,
    SDL_IGNORE := 0,
    SDL_DISABLE := 0{%H-},
    SDL_ENABLE := 1
  );

  (**
   *  \brief Fields shared by every event
   *)
  SDL_CommonEvent = record
    _type: SDL_EventType;
    timestamp: SDL_UInt32;
  end;

  (**
   *  \brief Window state change event data (event.window.* )
   *)
  SDL_WindowEvent = record
    _type: SDL_EventType;    (** SDL_WINDOWEVENT *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;    (** The associated window *)
    event: SDL_WindowEventID;(** ::SDL_WindowEventID *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
    padding3: SDL_UInt8;
    data1: SDL_SInt32;       (** event dependent data *)
    data2: SDL_SInt32;       (** event dependent data *)
  end;

  (**
   *  \brief Keyboard button event structure (event.key.* )
   *)
  SDL_KeyboardEvent = record
    _type: SDL_EventType;   (** SDL_KEYDOWN or SDL_KEYUP *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;   (** The window with keyboard focus, if any *)
    state: SDL_ButtonState; (** SDL_PRESSED or SDL_RELEASED *)
    _repeat: SDL_UInt8;     (** Non-zero if this is a key repeat *)
    padding2: SDL_UInt8;
    padding3: SDL_UInt8;
    keysym: SDL_KeySym;     (** The key that was pressed or released *)
  end;

  (**
   *  \brief Keyboard text editing event structure (event.edit.* )
   *)
  SDL_TextEditingEvent = record
    _type: SDL_EventType;            (** SDL_TEXTEDITING *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;            (** The window with keyboard focus, if any *)
    text: array[0..31] of AnsiChar;  (** The editing text *)
    start: SDL_SInt32;               (** The start cursor of selected editing text *)
    length: SDL_SInt32;              (** The length of selected editing text *)
  end;

  (**
   *  \brief Keyboard text input event structure (event.text.* )
   *)
  SDL_TextInputEvent = record
    _type: SDL_EventType;         (** SDL_TEXTINPUT *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;         (** The window with keyboard focus, if any *)
    text: array[0..31] of Char;   (** The input text *)
  end;

  (**
   *  \brief Mouse motion event structure (event.motion.* )
   *)
  SDL_MouseMotionEvent = record
    _type: SDL_EventType;    (** SDL_MOUSEMOTION *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;    (** The window with mouse focus, if any *)
    which: SDL_UInt32;       (** The mouse instance id, or SDL_TOUCH_MOUSEID *)
    state: SDL_SInt32;        (** The current button state *)
    x: SDL_SInt32;           (** X coordinate, relative to window *)
    y: SDL_SInt32;           (** Y coordinate, relative to window *)
    xrel: SDL_SInt32;        (** The relative motion in the X direction *)
    yrel: SDL_SInt32;        (** The relative motion in the Y direction *)
  end;

  (**
   *  \brief Mouse button event structure (event.button.* )
   *)
  SDL_MouseButtonEvent = record
    _type: SDL_EventType;    (** SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;    (** The window with mouse focus, if any *)
    which: SDL_UInt32;       (** The mouse instance id, or SDL_TOUCH_MOUSEID *)
    button: SDL_ButtonEnum;  (** The mouse button index *)
    state: SDL_ButtonState;  (** SDL_PRESSED or SDL_RELEASED *)
    clicks: SDL_UInt8;       (** 1 for single-click, 2 for double-click, etc. *)
    padding1: SDL_UInt8;
    x: SDL_SInt32;           (** X coordinate, relative to window *)
    y: SDL_SInt32;           (** Y coordinate, relative to window *)
  end;

  (**
   *  \brief Mouse wheel event structure (event.wheel.* )
   *)
  SDL_MouseWheelEvent = record
    _type: SDL_EventType;    (** SDL_MOUSEWHEEL *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;    (** The window with mouse focus, if any *)
    which: SDL_UInt32;       (** The mouse instance id, or SDL_TOUCH_MOUSEID *)
    x: SDL_SInt32;           (** The amount scrolled horizontally *)
    y: SDL_SInt32;           (** The amount scrolled vertically *)
    direction: SDL_MouseWheelDirection;   (** Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back *)
  end;

  (**
   *  \brief Joystick axis motion event structure (event.jaxis.* )
   *)
  SDL_JoyAxisEvent = record
    _type: SDL_EventType;      (** SDL_JOYAXISMOTION *)
    timestamp: SDL_UInt32;
    which: SDL_JoystickID;     (** The joystick instance id *)
    axis: SDL_UInt8;           (** The joystick axis index *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
    padding3: SDL_UInt8;
    value: SDL_SInt16;         (** The axis value (range: -32768 to 32767) *)
    padding4: SDL_UInt16;
  end;

  (**
   *  \brief Joystick trackball motion event structure (event.jball.* )
   *)
  SDL_JoyBallEvent = record
    _type: SDL_EventType;      (** SDL_JOYBALLMOTION) *)
    timestamp: SDL_UInt32;
    which: SDL_JoystickID;     (** The joystick instance id) *)
    ball: SDL_UInt8;           (** The joystick trackball index) *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
    padding3: SDL_UInt8;
    xrel: SDL_SInt16;          (** The relative motion in the X direction) *)
    yrel: SDL_SInt16;          (** The relative motion in the Y direction) *)
  end;

  (**
   *  \brief Joystick hat position change event structure (event.jhat.* )
   *)
  SDL_JoyHatEvent = record
    _type: SDL_EventType;  (** SDL_JOYHATMOTION) *)
    timestamp: SDL_UInt32;
    which: SDL_JoystickID; (** The joystick instance id) *)
    hat: SDL_UInt8;            (** The joystick hat index) *)
    value: SDL_UInt8;      (**  The hat position value.
                             *  SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
                             *  SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
                             *  SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
                             *
                             *  /Note that zero means the POV is centered.
                             *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
  end;

  (**
   *  \brief Joystick button event structure (event.jbutton.* )
   *)
  SDL_JoyButtonEvent = record
    _type: SDL_EventType;     (** SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP *)
    timestamp: SDL_UInt32;
    which: SDL_JoystickID;    (** The joystick instance id *)
    button: SDL_UInt8;        (** The joystick button index *)
    state: SDL_UInt8;         (** SDL_PRESSED or SDL_RELEASED *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
  end;

  (**
   *  \brief Joystick device event structure (event.jdevice.* )
   *)
  SDL_JoyDeviceEvent = record
    _type: SDL_EventType;   (** SDL_JOYDEVICEADDED or SDL_JOYDEVICEREMOVED *)
    timestamp: SDL_UInt32;
    which: SDL_SInt32;      (** The joystick device index for the ADDED event, instance id for the REMOVED event *)
  end;

  (**
   *  \brief Game controller axis motion event structure (event.caxis.* )
   *)
  SDL_ControllerAxisEvent = record
    _type: SDL_EventType;  (** SDL_CONTROLLERAXISMOTION *)
    timestamp: SDL_UInt32;
    which: SDL_JoystickID; (** The joystick instance id *)
    axis: SDL_UInt8;       (** The controller axis (SDL_GameControllerAxis) *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
    padding3: SDL_UInt8;
    value: SDL_SInt16;     (** The axis value (range: -32768 to 32767) *)
    padding4: SDL_UInt16;
  end;

  (**
   *  \brief Game controller button event structure (event.cbutton.* )
   *)
  SDL_ControllerButtonEvent = record
    _type: SDL_EventType;     (** SDL_CONTROLLERBUTTONDOWN or SDL_CONTROLLERBUTTONUP *)
    timestamp: SDL_UInt32;
    which: SDL_JoystickID;    (** The joystick instance id *)
    button: SDL_UInt8;        (** The controller button (SDL_GameControllerButton) *)
    state: SDL_UInt8;         (** SDL_PRESSED or SDL_RELEASED *)
    padding1: SDL_UInt8;
    padding2: SDL_UInt8;
  end;

  (**
   *  \brief Controller device event structure (event.cdevice. * )
   *)
  SDL_ControllerDeviceEvent = record
    _type: SDL_EventType;    (** SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED, or SDL_CONTROLLERDEVICEREMAPPED *)
    timestamp: SDL_UInt32;
    which: SDL_SInt32;       (** The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event *)
  end;

  (**
   *  \brief Audio device event structure (event.adevice.* )
   *)
  SDL_AudioDeviceEvent = record
    _type: SDL_EventType;    (** ::SDL_AUDIODEVICEADDED, or ::SDL_AUDIODEVICEREMOVED *)
    timestamp: SDL_Uint32;
    which: SDL_Uint32;       (** The audio device index for the ADDED event (valid until next SDL_GetNumAudioDevices() call), SDL_AudioDeviceID for the REMOVED event *)
    iscapture: SDL_Uint8;    (** zero if an output device, non-zero if a capture device. *)
    padding1: SDL_Uint8;
    padding2: SDL_Uint8;
    padding3: SDL_Uint8;
  end;

  (**
   *  \brief Touch finger event structure (event.tfinger.* )
   *)
  SDL_TouchFingerEvent = record
    _type: SDL_EventType;    (** SDL_FINGERMOTION or SDL_FINGERDOWN or SDL_FINGERUP *)
    timestamp: SDL_UInt32;
    touchId: SDL_TouchID;    (** The touch device id *)
    fingerId: SDL_FingerID;
    x: SDL_Float;            (** Normalized in the range 0...1 *)
    y: SDL_Float;            (** Normalized in the range 0...1 *)
    dx: SDL_Float;           (** Normalized in the range -1...1 *)
    dy: SDL_Float;           (** Normalized in the range -1...1 *)
    pressure: SDL_Float;     (** Normalized in the range 0...1 *)
  end;

  (**
   *  \brief Multiple Finger Gesture Event (event.mgesture.* )
   *)
  SDL_MultiGestureEvent = record
    _type: SDL_EventType;   (** SDL_MULTIGESTURE *)
    timestamp: SDL_UInt32;
    touchId: SDL_TouchID;   (** The touch device index *)
    dTheta: SDL_Float;
    dDist: SDL_Float;
    x: SDL_Float;
    y: SDL_Float;
    numFingers: SDL_UInt16;
    padding: SDL_UInt16;
  end;

  (**
   * \brief Dollar Gesture Event (event.dgesture.* )
   *)
  SDL_DollarGestureEvent = record
    _type: SDL_EventType;     (** ::SDL_DOLLARGESTURE or ::SDL_DOLLARRECORD *)
    timestamp: SDL_UInt32;
    touchId: SDL_TouchID;     (** The touch device id *)
    gestureId: SDL_GestureID;
    numFingers: SDL_UInt32;
    error: SDL_Float;
    x: SDL_Float;             (** Normalized center of gesture *)
    y: SDL_Float;             (** Normalized center of gesture *)
  end;

  (**
   *  \brief An event used to request a file open by the system (event.drop.* )
   *         This event is enabled by default, you can disable it with SDL_EventState().
   *  \note If this event is enabled, you must free the filename in the event.
   *)
  SDL_DropEvent = record
    _type: SDL_EventType; (** SDL_DROPFILE *)
    timestamp: UInt32;
    _file: SDL_String;    (** The file name, which should be freed with SDL_free() *)
  end;

  (**
   *  \brief The "quit requested" event
   *)
  SDL_QuitEvent = record
    _type: SDL_EventType;    (** SDL_QUIT *)
    timestamp: SDL_UInt32;
  end;

  (**
   *  \brief OS Specific event
   *)
  SDL_OSEvent = record
    _type: SDL_EventType;    (** SDL_QUIT *)
    timestamp: SDL_UInt32;
  end;

  (**
   *  \brief A user-defined event type (event.user.* )
   *)
  SDL_UserEvent = record
    _type: SDL_EventType;   (** SDL_USEREVENT through SDL_NUMEVENTS-1 *)
    timestamp: SDL_UInt32;
    windowID: SDL_UInt32;   (** The associated window if any *)
    code: SDL_SInt32;       (** User defined event code *)
    data1: SDL_Data;        (** User defined data pointer *)
    data2: SDL_Data;        (** User defined data pointer *)
  end;

  (**
   *  \brief A video driver dependent system event (event.syswm.* )
   *         This event is disabled by default, you can enable it with SDL_EventState()
   *
   *  \note If you want to use this event, you should include SDL_syswm.h.
   *)
  SDL_SysWMEvent = record
    _type: SDL_EventType;  (** SDL_SYSWMEVENT *)
    timestamp: SDL_UInt32;
    msg: SDL_Data;         (** driver dependent data PSDL_SysWMmsg (defined in SDL_syswm.h) *)
  end;

  SDL_Event = record
    case Integer of
      0: (_type: SDL_EventType);                  (** Event type, shared with all events *)
      1: (common: SDL_CommonEvent;);              (** Common event data *)
      2: (window: SDL_WindowEvent;);              (** Window event data *)
      3: (key: SDL_KeyboardEvent;);               (** Keyboard event data *)
      4: (edit: SDL_TextEditingEvent;);           (** Text editing event data *)
      5: (text: SDL_TextInputEvent;);             (** Text input event data *)
      6: (motion: SDL_MouseMotionEvent;);         (** Mouse motion event data *)
      7: (button: SDL_MouseButtonEvent;);         (** Mouse button event data *)
      8: (wheel: SDL_MouseWheelEvent;);           (** Mouse wheel event data *)
      9: (jaxis: SDL_JoyAxisEvent;);              (** Joystick axis event data *)
      10: (jball: SDL_JoyBallEvent;);             (** Joystick ball event data *)
      11: (jhat: SDL_JoyHatEvent;);               (** Joystick hat event data *)
      12: (jbutton: SDL_JoyButtonEvent;);         (** Joystick button event data *)
      13: (jdevice: SDL_JoyDeviceEvent;);         (** Joystick device change event data *)
      14: (caxis: SDL_ControllerAxisEvent;);      (** Game Controller axis event data *)
      15: (cbutton: SDL_ControllerButtonEvent;);  (** Game Controller button event data *)
      16: (cdevice: SDL_ControllerDeviceEvent;);  (** Game Controller device event data *)
      17: (adevice: SDL_AudioDeviceEvent);        (** Audio device event data *)
      18: (quit: SDL_QuitEvent;);                 (** Quit request event data *)
      19: (user: SDL_UserEvent;);                 (** Custom event data *)
      20: (syswm: SDL_SysWMEvent;);               (** System dependent window event data *)
      21: (tfinger: SDL_TouchFingerEvent;);       (** Touch finger event data *)
      22: (mgesture: SDL_MultiGestureEvent;);     (** Gesture event data *)
      23: (dgesture: SDL_DollarGestureEvent;);    (** Gesture event data *)
      24: (drop: SDL_DropEvent;);                 (** Drag and drop event data *)

      (* This is necessary for ABI compatibility between Visual C++ and GCC
         Visual C++ will respect the push pack pragma and use 52 bytes for
         this structure, and GCC will use the alignment of the largest datatype
         within the union, which is 8 bytes.

         So... we'll add padding to force the size to be 56 bytes for both.
      *)
      25: (padding: array [0..55] of SDL_Uint8;);
  end;

