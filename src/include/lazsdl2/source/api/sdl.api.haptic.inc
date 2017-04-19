

type

  PSDL_Haptic = ^SDL_Haptic;
  PSDL_HapticEffect = ^SDL_HapticEffect;

  SDLHapticConsts = object
    public const
    (**
     *   Constant effect supported.
     *
     *  Constant haptic effect.
     *
     *   SDL_HapticCondition
     *)
    SDL_HAPTIC_CONSTANT = (1 shl 0);
    (**
     *   Sine wave effect supported.
     *
     *  Periodic haptic effect that simulates sine waves.
     *
     *   SDL_HapticPeriodic
     *)
    SDL_HAPTIC_SINE     = (1 shl 1);
    (**
     *   Square wave effect supported.
     *
     *  Periodic haptic effect that simulates square waves.
     *
     *   SDL_HapticPeriodic
     *)
    SDL_HAPTIC_SQUARE   = (1 shl 2);
    (**
     *   Triangle wave effect supported.
     *
     *  Periodic haptic effect that simulates triangular waves.
     *
     *   SDL_HapticPeriodic
     *)
    SDL_HAPTIC_TRIANGLE = (1 shl 3);
    (**
     *   Sawtoothup wave effect supported.
     *
     *  Periodic haptic effect that simulates saw tooth up waves.
     *
     *   SDL_HapticPeriodic
     *)
    SDL_HAPTIC_SAWTOOTHUP = (1 shl 4);
    (**
     *   Sawtoothdown wave effect supported.
     *
     *  Periodic haptic effect that simulates saw tooth down waves.
     *
     *   SDL_HapticPeriodic
     *)
    SDL_HAPTIC_SAWTOOTHDOWN = (1 shl 5);
    (**
     *   Ramp effect supported.
     *
     *  Ramp haptic effect.
     *
     *   SDL_HapticRamp
     *)
    SDL_HAPTIC_RAMP = (1 shl 6);
    (**
     *   Spring effect supported - uses axes position.
     *
     *  Condition haptic effect that simulates a spring.  Effect is based on the
     *  axes position.
     *
     *   SDL_HapticCondition
     *)
    SDL_HAPTIC_SPRING = (1 shl 7);
    (**
     *   Damper effect supported - uses axes velocity.
     *
     *  Condition haptic effect that simulates dampening.  Effect is based on the
     *  axes velocity.
     *
     *   SDL_HapticCondition
     *)
    SDL_HAPTIC_DAMPER = (1 shl 8);
    (**
     *   Inertia effect supported - uses axes acceleration.
     *
     *  Condition haptic effect that simulates inertia.  Effect is based on the axes
     *  acceleration.
     *
     *   SDL_HapticCondition
     *)
    SDL_HAPTIC_INERTIA = (1 shl 9);
    (**
     *   Friction effect supported - uses axes movement.
     *
     *  Condition haptic effect that simulates friction.  Effect is based on the
     *  axes movement.
     *
     *   SDL_HapticCondition
     *)
    SDL_HAPTIC_FRICTION = (1 shl 10);
    (**
     *   Custom effect is supported.
     *
     *  User defined custom haptic effect.
     *)
    SDL_HAPTIC_CUSTOM = (1 shl 11);

    //Haptic effects

    //These last few are features the device has, not effects

    (**
     *   Device can set global gain.
     *
     *  Device supports setting the global gain.
     *
     *   SDL_HapticSetGain
     *)
    SDL_HAPTIC_GAIN = (1 shl 12);
    (**
     *   Device can set autocenter.
     *
     *  Device supports setting autocenter.
     *
     *   SDL_HapticSetAutocenter
     *)
    SDL_HAPTIC_AUTOCENTER = (1 shl 13);
    (**
     *   Device can be queried for effect status.
     *
     *  Device can be queried for effect status.
     *
     *   SDL_HapticGetEffectStatus
     *)
    SDL_HAPTIC_STATUS = (1 shl 14);
    (**
     *   Device can be paused.
     *
     *   SDL_HapticPause
     *   SDL_HapticUnpause
     *)
    SDL_HAPTIC_PAUSE = (1 shl 15);

    // Direction encodings

    (**
     *   Uses polar coordinates for the direction.
     *
     *   SDL_HapticDirection
     *)
    SDL_HAPTIC_POLAR = 0;
    (**
     *   Uses cartesian coordinates for the direction.
     *
     *   SDL_HapticDirection
     *)
    SDL_HAPTIC_CARTESIAN = 1;
    (**
     *   Uses spherical coordinates for the direction.
     *
     *   SDL_HapticDirection
     *)
    SDL_HAPTIC_SPHERICAL = 2;

    //Haptic features
    //Misc defines.

    (**
     *  Used to play a device an infinite number of times.
     *
     *  SDL_HapticRunEffect
     *)
    SDL_HAPTIC_INFINITY = SDL_UInt32(4294967295);
  end;

  (**
   *  \brief Structure that represents a haptic direction.
   *
   *  This is the direction where the force comes from,
   *  instead of the direction in which the force is exerted.
   *
   *  Directions can be specified by:
   *   - ::SDL_HAPTIC_POLAR : Specified by polar coordinates.
   *   - ::SDL_HAPTIC_CARTESIAN : Specified by cartesian coordinates.
   *   - ::SDL_HAPTIC_SPHERICAL : Specified by spherical coordinates.
   *
   *  Cardinal directions of the haptic device are relative to the positioning
   *  of the device.  North is considered to be away from the user.
   *
   *  The following diagram represents the cardinal directions:
   *  \verbatim
                   .--.
                   |__| .-------.
                   |=.| |.-----.|
                   |--| ||     ||
                   |  | |'-----'|
                   |__|~')_____('
                     [ COMPUTER ]


                       North (0,-1)
                           ^
                           |
                           |
     (-1,0)  West <----[ HAPTIC ]----> East (1,0)
                           |
                           |
                           v
                        South (0,1)


                        [ USER ]
                          \|||/
                          (o o)
                    ---ooO-(_)-Ooo---
      \endverbatim
   *
   *  If type is ::SDL_HAPTIC_POLAR, direction is encoded by hundredths of a
   *  degree starting north and turning clockwise.  ::SDL_HAPTIC_POLAR only uses
   *  the first \c dir parameter.  The cardinal directions would be:
   *   - North: 0 (0 degrees)
   *   - East: 9000 (90 degrees)
   *   - South: 18000 (180 degrees)
   *   - West: 27000 (270 degrees)
   *
   *  If type is ::SDL_HAPTIC_CARTESIAN, direction is encoded by three positions
   *  (X axis, Y axis and Z axis (with 3 axes)).  ::SDL_HAPTIC_CARTESIAN uses
   *  the first three \c dir parameters.  The cardinal directions would be:
   *   - North:  0,-1, 0
   *   - East:   1, 0, 0
   *   - South:  0, 1, 0
   *   - West:  -1, 0, 0
   *
   *  The Z axis represents the height of the effect if supported, otherwise
   *  it's unused.  In cartesian encoding (1, 2) would be the same as (2, 4), you
   *  can use any multiple you want, only the direction matters.
   *
   *  If type is ::SDL_HAPTIC_SPHERICAL, direction is encoded by two rotations.
   *  The first two \c dir parameters are used.  The \c dir parameters are as
   *  follows (all values are in hundredths of degrees):
   *   - Degrees from (1, 0) rotated towards (0, 1).
   *   - Degrees towards (0, 0, 1) (device needs at least 3 axes).
   *
   *
   *  Example of force coming from the south with all encodings (force coming
   *  from the south means the user will have to pull the stick to counteract):
   *  \code
   *  SDL_HapticDirection direction;
   *
   *  // Cartesian directions
   *  direction.type = SDL_HAPTIC_CARTESIAN; // Using cartesian direction encoding.
   *  direction.dir[0] = 0; // X position
   *  direction.dir[1] = 1; // Y position
   *  // Assuming the device has 2 axes, we don't need to specify third parameter.
   *
   *  // Polar directions
   *  direction.type = SDL_HAPTIC_POLAR; // We'll be using polar direction encoding.
   *  direction.dir[0] = 18000; // Polar only uses first parameter
   *
   *  // Spherical coordinates
   *  direction.type = SDL_HAPTIC_SPHERICAL; // Spherical encoding
   *  direction.dir[0] = 9000; // Since we only have two axes we don't need more parameters.
   *  \endcode
   *
   *  \sa SDL_HAPTIC_POLAR
   *  \sa SDL_HAPTIC_CARTESIAN
   *  \sa SDL_HAPTIC_SPHERICAL
   *  \sa SDL_HapticEffect
   *  \sa SDL_HapticNumAxes
   *)
  SDL_HapticDirection = record
    _type: SDL_UInt8;               (**< The type of encoding. *)
    dir: array[0..2] of SDL_SInt32; (**< The encoded direction. *)
  end;

  (**
   *   A structure containing a template for a Constant effect.
   *
   *  The struct is exclusive to the ::SDL_HAPTIC_CONSTANT effect.
   *
   *  A constant effect applies a constant force in the specified direction
   *  to the joystick.
   *
   *   SDL_HAPTIC_CONSTANT
   *   SDL_HapticEffect
   *)
  SDL_HapticConstant = record
    (** Header *)
    _type: SDL_UInt16;              (**< SDL_HAPTIC_CONSTANT *)
    direction: SDL_HapticDirection; (**< Direction of the effect. *)

    (** Replay *)
    length: SDL_UInt32;             (**< Duration of the effect. *)
    delay: SDL_UInt16;              (**< Delay before starting the effect. *)

    (** Trigger *)
    button: SDL_UInt16;             (**< Button that triggers the effect. *)
    interval: SDL_UInt16;           (**< How soon it can be triggered again after button. *)

    (** Constant *)
    level: SDL_SInt16;              (**< Strength of the constant effect. *)

    (** Envelope *)
    attack_length: SDL_UInt16;      (**< Duration of the attack. *)
    attack_level: SDL_UInt16;       (**< Level at the start of the attack. *)
    fade_length: SDL_UInt16;        (**< Duration of the fade. *)
    fade_level: SDL_UInt16;         (**< Level at the end of the fade. *)
  end;

  (**
   *  \brief A structure containing a template for a Periodic effect.
   *
   *  The struct handles the following effects:
   *   - ::SDL_HAPTIC_SINE
   *   - ::SDL_HAPTIC_LEFTRIGHT
   *   - ::SDL_HAPTIC_TRIANGLE
   *   - ::SDL_HAPTIC_SAWTOOTHUP
   *   - ::SDL_HAPTIC_SAWTOOTHDOWN
   *
   *  A periodic effect consists in a wave-shaped effect that repeats itself
   *  over time.  The type determines the shape of the wave and the parameters
   *  determine the dimensions of the wave.
   *
   *  Phase is given by hundredth of a degree meaning that giving the phase a value
   *  of 9000 will displace it 25% of its period.  Here are sample values:
   *   -     0: No phase displacement.
   *   -  9000: Displaced 25% of its period.
   *   - 18000: Displaced 50% of its period.
   *   - 27000: Displaced 75% of its period.
   *   - 36000: Displaced 100% of its period, same as 0, but 0 is preferred.
   *
   *  Examples:
   *  \verbatim
      SDL_HAPTIC_SINE
        __      __      __      __
       /  \    /  \    /  \    /
      /    \__/    \__/    \__/

      SDL_HAPTIC_SQUARE
       __    __    __    __    __
      |  |  |  |  |  |  |  |  |  |
      |  |__|  |__|  |__|  |__|  |

      SDL_HAPTIC_TRIANGLE
        /\    /\    /\    /\    /\
       /  \  /  \  /  \  /  \  /
      /    \/    \/    \/    \/

      SDL_HAPTIC_SAWTOOTHUP
        /|  /|  /|  /|  /|  /|  /|
       / | / | / | / | / | / | / |
      /  |/  |/  |/  |/  |/  |/  |

      SDL_HAPTIC_SAWTOOTHDOWN
      \  |\  |\  |\  |\  |\  |\  |
       \ | \ | \ | \ | \ | \ | \ |
        \|  \|  \|  \|  \|  \|  \|
      \endverbatim
   *
   *  \sa SDL_HAPTIC_SINE
   *  \sa SDL_HAPTIC_LEFTRIGHT
   *  \sa SDL_HAPTIC_TRIANGLE
   *  \sa SDL_HAPTIC_SAWTOOTHUP
   *  \sa SDL_HAPTIC_SAWTOOTHDOWN
   *  \sa SDL_HapticEffect
   *)
  SDL_HapticPeriodic = record
    // Header
    _type: SDL_UInt16;               (** SDL_HAPTIC_SINE, SDL_HAPTIC_SQUARE,
                                         SDL_HAPTIC_TRIANGLE, SDL_HAPTIC_SAWTOOTHUP or
                                         SDL_HAPTIC_SAWTOOTHDOWN *)
    direction: SDL_HapticDirection;  (** Direction of the effect. *)

    // Replay
    length: SDL_UInt32;              (** Duration of the effect. *)
    delay: SDL_UInt16;               (** Delay before starting the effect. *)

    // Trigger
    button: SDL_UInt16;              (** Button that triggers the effect. *)
    interval: SDL_UInt16;            (** How soon it can be triggered again after button. *)

    // Periodic
    period: SDL_UInt16;              (** Period of the wave. *)
    magnitude: SDL_SInt16;           (** Peak value; if negative, equivalent to 180 degrees extra phase shift. *)
    offset: SDL_SInt16;              (** Mean value of the wave. *)
    phase: SDL_UInt16;               (** Positive phase shift given by hundredth of a degree. *)

    // Envelope
    attack_length: SDL_UInt16;       (** Duration of the attack. *)
    attack_level: SDL_UInt16;        (** Level at the start of the attack. *)
    fade_length: SDL_UInt16;         (** Duration of the fade. *)
    fade_level: SDL_UInt16;          (** Level at the end of the fade. *)
  end;

  (**
   *   A structure containing a template for a Condition effect.
   *
   *  The struct handles the following effects:
   *   - SDL_HAPTIC_SPRING: Effect based on axes position.
   *   - SDL_HAPTIC_DAMPER: Effect based on axes velocity.
   *   - SDL_HAPTIC_INERTIA: Effect based on axes acceleration.
   *   - SDL_HAPTIC_FRICTION: Effect based on axes movement.
   *
   *  Direction is handled by condition internals instead of a direction member.
   *  The condition effect specific members have three parameters.  The first
   *  refers to the X axis, the second refers to the Y axis and the third
   *  refers to the Z axis.  The right terms refer to the positive side of the
   *  axis and the left terms refer to the negative side of the axis.  Please
   *  refer to the ::SDL_HapticDirection diagram for which side is positive and
   *  which is negative.
   *
   *   SDL_HapticDirection
   *   SDL_HAPTIC_SPRING
   *   SDL_HAPTIC_DAMPER
   *   SDL_HAPTIC_INERTIA
   *   SDL_HAPTIC_FRICTION
   *   SDL_HapticEffect
   *)
  SDL_HapticCondition = record
    // Header
    _type: SDL_UInt16;                       (** SDL_HAPTIC_SPRING, SDL_HAPTIC_DAMPER,
                                               * SDL_HAPTIC_INERTIA or SDL_HAPTIC_FRICTION *)
    direction: SDL_HapticDirection;         (** Direction of the effect - Not used ATM. *)

    // Replay
    length: SDL_UInt32;                      (** Duration of the effect. *)
    delay: SDL_UInt16;                       (** Delay before starting the effect. *)

    // Trigger
    button: SDL_UInt16;                      (** Button that triggers the effect. *)
    interval: SDL_UInt16;                    (** How soon it can be triggered again after button. *)

    // Condition
    right_sat: array[0..2] of SDL_UInt16;    (** Level when joystick is to the positive side; max 0xFFFF. *)
    left_sat: array[0..2] of SDL_UInt16;     (** Level when joystick is to the negative side; max 0xFFFF. *)
    right_coeff: array[0..2] of SDL_SInt16;  (** How fast to increase the force towards the positive side. *)
    left_coeff: array[0..2] of SDL_SInt16;   (** How fast to increase the force towards the negative side. *)
    deadband: array[0..2] of SDL_UInt16;     (** Size of the dead zone; max 0xFFFF: whole axis-range when 0-centered. *)
    center: array[0..2] of SDL_SInt16;       (** Position of the dead zone. *)
  end;

  (**
   *   A structure containing a template for a Ramp effect.
   *
   *  This struct is exclusively for the ::SDL_HAPTIC_RAMP effect.
   *
   *  The ramp effect starts at start strength and ends at end strength.
   *  It augments in linear fashion.  If you use attack and fade with a ramp
   *  the effects get added to the ramp effect making the effect become
   *  quadratic instead of linear.
   *
   *   SDL_HAPTIC_RAMP
   *   SDL_HapticEffect
   *)
  SDL_HapticRamp = record
    (** Header *)
    _type: SDL_UInt16;                    (** SDL_HAPTIC_RAMP *)
    direction: SDL_HapticDirection;       (** Direction of the effect. *)

    (** Replay *)
    length: SDL_UInt32;                   (** Duration of the effect. *)
    delay: SDL_UInt16;                    (** Delay before starting the effect. *)

    (** Trigger *)
    button: SDL_UInt16;                   (** Button that triggers the effect. *)
    interval: SDL_UInt16;                 (** How soon it can be triggered again after button. *)

    (** Ramp *)
    start: SDL_SInt16;                    (** Beginning strength level. *)
    _end: SDL_SInt16;                     (** Ending strength level. *)

    (** Envelope *)
    attack_length: SDL_UInt16;            (** Duration of the attack. *)
    attack_level: SDL_UInt16;             (** Level at the start of the attack. *)
    fade_length: SDL_UInt16;              (** Duration of the fade. *)
    fade_level: SDL_UInt16;               (** Level at the end of the fade. *)
  end;

  (**
   *   A structure containing a template for the ::SDL_HAPTIC_CUSTOM effect.
   *
   *  A custom force feedback effect is much like a periodic effect, where the
   *  application can define its exact shape.  You will have to allocate the
   *  data yourself.  Data should consist of channels * samples Uint16 samples.
   *
   *  If channels is one, the effect is rotated using the defined direction.
   *  Otherwise it uses the samples in data for the different axes.
   *
   *   SDL_HAPTIC_CUSTOM
   *   SDL_HapticEffect
   *)
  SDL_HapticCustom = record
    (** Header *)
    _type: SDL_UInt16;                    (** SDL_HAPTIC_CUSTOM *)
    direction: SDL_HapticDirection;       (** Direction of the effect. *)

    (** Replay *)
    length: SDL_UInt32;                   (** Duration of the effect. *)
    delay: SDL_UInt16;                    (** Delay before starting the effect. *)

    (** Trigger *)
    button: SDL_UInt16;                   (** Button that triggers the effect. *)
    interval: SDL_UInt16;                 (** How soon it can be triggered again after button. *)

    (** Custom *)
    channels: SDL_UInt8;                  (** Axes to use, minimum of one. *)
    period: SDL_UInt16;                   (** Sample periods. *)
    samples: SDL_UInt16;                  (** Amount of samples. *)
    data: PSDL_UInt16;                    (** Should contain channels*samples items. *)

    (** Envelope *)
    attack_length: SDL_UInt16;            (** Duration of the attack. *)
    attack_level: SDL_UInt16;             (** Level at the start of the attack. *)
    fade_length: SDL_UInt16;              (** Duration of the fade. *)
    fade_level: SDL_UInt16;               (** Level at the end of the fade. *)
  end;

  (**
   *   The generic template for any haptic effect.
   *
   *  All values max at 32767 (0x7FFF).  Signed values also can be negative.
   *  Time values unless specified otherwise are in milliseconds.
   *
   *  You can also pass SDL_HAPTIC_INFINITY to length instead of a 0-32767
   *  value.  Neither delay, interval, attack_length nor fade_length support
   *  SDL_HAPTIC_INFINITY.  Fade will also not be used since effect never ends.
   *
   *  Additionally, the SDL_HAPTIC_RAMP effect does not support a duration of
   *  SDL_HAPTIC_INFINITY.
   *
   *  Button triggers may not be supported on all devices, it is advised to not
   *  use them if possible.  Buttons start at index 1 instead of index 0 like
   *  the joystick.
   *
   *  If both attack_length and fade_level are 0, the envelope is not used,
   *  otherwise both values are used.
   *
   *  Common parts:
   *
   *  // Replay - All effects have this
   *  Uint32 length;        // Duration of effect (ms).
   *  Uint16 delay;         // Delay before starting effect.
   *
   *  // Trigger - All effects have this
   *  Uint16 button;        // Button that triggers effect.
   *  Uint16 interval;      // How soon before effect can be triggered again.
   *
   *  // Envelope - All effects except condition effects have this
   *  Uint16 attack_length; // Duration of the attack (ms).
   *  Uint16 attack_level;  // Level at the start of the attack.
   *  Uint16 fade_length;   // Duration of the fade out (ms).
   *  Uint16 fade_level;    // Level at the end of the fade.
   *
   *
   *
   *  Here we have an example of a constant effect evolution in time:
   *
      Strength
      ^
      |
      |    effect level -->  _________________
      |                     /                 \
      |                    /                   \
      |                   /                     \
      |                  /                       \
      | attack_level --> |                        \
      |                  |                        |  <---  fade_level
      |
      +--------------------------------------------------> Time
                         [--]                 [---]
                         attack_length        fade_length

      [------------------][-----------------------]
      delay               length

   *
   *  Note either the attack_level or the fade_level may be above the actual
   *  effect level.
   *
   *   SDL_HapticConstant
   *   SDL_HapticPeriodic
   *   SDL_HapticCondition
   *   SDL_HapticRamp
   *   SDL_HapticCustom
   *)
  SDL_HapticEffect = record
    _type: UInt16;                           (** Effect type. Common for all force feedback effects. *)
    case UInt16 of
      0: (constant: SDL_HapticConstant;);    (** Constant effect. *)
      1: (periodic: SDL_HapticPeriodic;);    (** Periodic effect. *)
      2: (condition: SDL_HapticCondition;);  (** Condition effect. *)
      3: (ramp: SDL_HapticRamp;);            (** Ramp effect. *)
      4: (custom: SDL_HapticCustom;);        (** Custom effect. *)
  end;

  (**
   *   SDL_Haptic
   *
   *  The haptic structure used to identify an SDL haptic.
   *
   *   SDL_HapticOpen
   *   SDL_HapticOpenFromJoystick
   *   SDL_HapticClose
   *)
  SDL_Haptic = record
      index: SDL_Uint8;                (** Stores index it is attached to *)

      effects: SDL_Pointer;            (** Allocated effects *)
      neffects: SDL_SInt32;            (** Maximum amount of effects *)
      nplaying: SDL_SInt32;            (** Maximum amount of effects to play at the same time *)
      supported: SDL_UInt32;           (** Supported effects *)
      naxes: SDL_SInt32;               (** Number of axes on the device. *)

      hwdata: SDL_Pointer;             (** Driver dependent *)
      ref_count: SDL_SInt32;           (** Count for multiple opens *)

      rumble_id: SDL_SInt32;           (** ID of rumble effect for simple rumble API. *)
      rumble_effect: SDL_HapticEffect; (** Rumble effect. *)
      next: PSDL_Haptic;               (** pointer to next haptic we have allocated *)
  end;

