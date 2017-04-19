(**
 *  \file SDL_haptic.h
 *
 *  \brief The SDL Haptic subsystem allows you to control haptic (force feedback)
 *         devices.
 *
 *  The basic usage is as follows:
 *   - Initialize the Subsystem (::SDL_INIT_HAPTIC).
 *   - Open a Haptic Device.
 *    - SDL_HapticOpen() to open from index.
 *    - SDL_HapticOpenFromJoystick() to open from an existing joystick.
 *   - Create an effect (::SDL_HapticEffect).
 *   - Upload the effect with SDL_HapticNewEffect().
 *   - Run the effect with SDL_HapticRunEffect().
 *   - (optional) Free the effect with SDL_HapticDestroyEffect().
 *   - Close the haptic device with SDL_HapticClose().
 *
 * \par Simple rumble example:
 * \code
 *    SDL_Haptic *haptic;
 *
 *    // Open the device
 *    haptic = SDL_HapticOpen( 0 );
 *    if (haptic == NULL)
 *       return -1;
 *
 *    // Initialize simple rumble
 *    if (SDL_HapticRumbleInit( haptic ) != 0)
 *       return -1;
 *
 *    // Play effect at 50% strength for 2 seconds
 *    if (SDL_HapticRumblePlay( haptic, 0.5, 2000 ) != 0)
 *       return -1;
 *    SDL_Delay( 2000 );
 *
 *    // Clean up
 *    SDL_HapticClose( haptic );
 * \endcode
 *
 * \par Complete example:
 * \code
 * int test_haptic( SDL_Joystick * joystick ) {
 *    SDL_Haptic *haptic;
 *    SDL_HapticEffect effect;
 *    int effect_id;
 *
 *    // Open the device
 *    haptic = SDL_HapticOpenFromJoystick( joystick );
 *    if (haptic == NULL) return -1; // Most likely joystick isn't haptic
 *
 *    // See if it can do sine waves
 *    if ((SDL_HapticQuery(haptic) & SDL_HAPTIC_SINE)==0) {
 *       SDL_HapticClose(haptic); // No sine effect
 *       return -1;
 *    }
 *
 *    // Create the effect
 *    memset( &effect, 0, sizeof(SDL_HapticEffect) ); // 0 is safe default
 *    effect.type = SDL_HAPTIC_SINE;
 *    effect.periodic.direction.type = SDL_HAPTIC_POLAR; // Polar coordinates
 *    effect.periodic.direction.dir[0] = 18000; // Force comes from south
 *    effect.periodic.period = 1000; // 1000 ms
 *    effect.periodic.magnitude = 20000; // 20000/32767 strength
 *    effect.periodic.length = 5000; // 5 seconds long
 *    effect.periodic.attack_length = 1000; // Takes 1 second to get max strength
 *    effect.periodic.fade_length = 1000; // Takes 1 second to fade away
 *
 *    // Upload the effect
 *    effect_id = SDL_HapticNewEffect( haptic, &effect );
 *
 *    // Test the effect
 *    SDL_HapticRunEffect( haptic, effect_id, 1 );
 *    SDL_Delay( 5000); // Wait for the effect to finish
 *
 *    // We destroy the effect, although closing the device also does this
 *    SDL_HapticDestroyEffect( haptic, effect_id );
 *
 *    // Close the device
 *    SDL_HapticClose(haptic);
 *
 *    return 0; // Success
 * }
 * \endcode
 *
 * You can also find out more information on my blog:
 * http://bobbens.dyndns.org/journal/2010/sdl_haptic/
 *
 * \author Edgar Simo Serra
 *)
unit SDL.Api.Haptic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLHapticApi }

  TSDLHapticApi = class(TSubLibrary)
  private type
    TSDL_NumHaptics = function: SDL_SInt32 cdecl;
    TSDL_HapticName = function(device_index: SDL_SInt32): SDL_String cdecl;
    TSDL_HapticOpen = function(device_index: SDL_SInt32): PSDL_Haptic cdecl;
    TSDL_HapticOpened = function(device_index: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_HapticIndex = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_MouseIsHaptic = function: SDL_SInt32 cdecl;
    TSDL_HapticOpenFromMouse = function: PSDL_Haptic cdecl;
    TSDL_JoystickIsHaptic = function(joystick: PSDL_Joystick): SDL_SInt32 cdecl;
    TSDL_HapticOpenFromJoystick = function(joystick: PSDL_Joystick): PSDL_Haptic cdecl;
    TSDL_HapticClose = procedure(haptic: PSDL_Haptic) cdecl;
    TSDL_HapticNumEffects = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticNumEffectsPlaying = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticQuery = function(haptic: PSDL_Haptic): SDL_UInt32 cdecl;
    TSDL_HapticNumAxes = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticEffectSupported = function(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer cdecl;
    TSDL_HapticNewEffect = function(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): SDL_SInt32 cdecl;
    TSDL_HapticUpdateEffect = function(haptic: PSDL_Haptic; effect: SDL_SInt32; data: PSDL_HapticEffect): SDL_SInt32 cdecl;
    TSDL_HapticRunEffect = function(haptic: PSDL_Haptic; effect: SDL_SInt32; iterations: SDL_UInt32): SDL_SInt32 cdecl;
    TSDL_HapticStopEffect = function(haptic: PSDL_Haptic; effect: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_HapticDestroyEffect = procedure(haptic: PSDL_Haptic; effect: SDL_SInt32) cdecl;
    TSDL_HapticGetEffectStatus = function(haptic: PSDL_Haptic; effect: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_HapticSetGain = function(haptic: PSDL_Haptic; gain: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_HapticSetAutocenter = function(haptic: PSDL_Haptic; autocenter: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_HapticPause = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticUnpause = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticStopAll = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticRumbleSupported = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticRumbleInit = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
    TSDL_HapticRumblePlay = function(haptic: PSDL_Haptic; strength: SDL_Float; length: SDL_UInt32): SDL_SInt32 cdecl;
    TSDL_HapticRumbleStop = function(haptic: PSDL_Haptic): SDL_SInt32 cdecl;
  public
    (**
     *  \brief Count the number of haptic devices attached to the system.
     *
     *  \return Number of haptic devices detected on the system.
     *)
    SDL_NumHaptics: TSDL_NumHaptics;
    (**
     *  \brief Get the implementation dependent name of a Haptic device.
     *
     *  This can be called before any joysticks are opened.
     *  If no name can be found, this function returns NULL.
     *
     *  \param device_index Index of the device to get its name.
     *  \return Name of the device or NULL on error.
     *
     *  \sa SDL_NumHaptics
     *)
    SDL_HapticName: TSDL_HapticName;
    (**
     *  \brief Opens a Haptic device for usage.
     *
     *  The index passed as an argument refers to the N'th Haptic device on this
     *  system.
     *
     *  When opening a haptic device, its gain will be set to maximum and
     *  autocenter will be disabled.  To modify these values use
     *  SDL_HapticSetGain() and SDL_HapticSetAutocenter().
     *
     *  \param device_index Index of the device to open.
     *  \return Device identifier or NULL on error.
     *
     *  \sa SDL_HapticIndex
     *  \sa SDL_HapticOpenFromMouse
     *  \sa SDL_HapticOpenFromJoystick
     *  \sa SDL_HapticClose
     *  \sa SDL_HapticSetGain
     *  \sa SDL_HapticSetAutocenter
     *  \sa SDL_HapticPause
     *  \sa SDL_HapticStopAll
     *)
    SDL_HapticOpen: TSDL_HapticOpen;
    (**
     *  \brief Checks if the haptic device at index has been opened.
     *
     *  \param device_index Index to check to see if it has been opened.
     *  \return 1 if it has been opened or 0 if it hasn't.
     *
     *  \sa SDL_HapticOpen
     *  \sa SDL_HapticIndex
     *)
    SDL_HapticOpened: TSDL_HapticOpened;
    (**
     *  \brief Gets the index of a haptic device.
     *
     *  \param haptic Haptic device to get the index of.
     *  \return The index of the haptic device or -1 on error.
     *
     *  \sa SDL_HapticOpen
     *  \sa SDL_HapticOpened
     *)
    SDL_HapticIndex: TSDL_HapticIndex;
    (**
     *  \brief Gets whether or not the current mouse has haptic capabilities.
     *
     *  \return SDL_TRUE if the mouse is haptic, SDL_FALSE if it isn't.
     *
     *  \sa SDL_HapticOpenFromMouse
     *)
    SDL_MouseIsHaptic: TSDL_MouseIsHaptic;
    (**
     *  \brief Tries to open a haptic device from the current mouse.
     *
     *  \return The haptic device identifier or NULL on error.
     *
     *  \sa SDL_MouseIsHaptic
     *  \sa SDL_HapticOpen
     *)
    SDL_HapticOpenFromMouse: TSDL_HapticOpenFromMouse;
    (**
     *  \brief Checks to see if a joystick has haptic features.
     *
     *  \param joystick Joystick to test for haptic capabilities.
     *  \return 1 if the joystick is haptic, 0 if it isn't
     *          or -1 if an error ocurred.
     *
     *  \sa SDL_HapticOpenFromJoystick
     *)
    SDL_JoystickIsHaptic: TSDL_JoystickIsHaptic;
    (**
     *  \brief Opens a Haptic device for usage from a Joystick device.
     *
     *  You must still close the haptic device separately.  It will not be closed
     *  with the joystick.
     *
     *  When opening from a joystick you should first close the haptic device before
     *  closing the joystick device.  If not, on some implementations the haptic
     *  device will also get unallocated and you'll be unable to use force feedback
     *  on that device.
     *
     *  \param joystick Joystick to create a haptic device from.
     *  \return A valid haptic device identifier on success or NULL on error.
     *
     *  \sa SDL_HapticOpen
     *  \sa SDL_HapticClose
     *)
    SDL_HapticOpenFromJoystick: TSDL_HapticOpenFromJoystick;
    (**
     *  \brief Closes a Haptic device previously opened with SDL_HapticOpen().
     *
     *  \param haptic Haptic device to close.
     *)
    SDL_HapticClose: TSDL_HapticClose;
    (**
     *  \brief Returns the number of effects a haptic device can store.
     *
     *  On some platforms this isn't fully supported, and therefore is an
     *  approximation.  Always check to see if your created effect was actually
     *  created and do not rely solely on SDL_HapticNumEffects().
     *
     *  \param haptic The haptic device to query effect max.
     *  \return The number of effects the haptic device can store or
     *          -1 on error.
     *
     *  \sa SDL_HapticNumEffectsPlaying
     *  \sa SDL_HapticQuery
     *)
    SDL_HapticNumEffects: TSDL_HapticNumEffects;
    (**
     *  \brief Returns the number of effects a haptic device can play at the same
     *         time.
     *
     *  This is not supported on all platforms, but will always return a value.
     *  Added here for the sake of completeness.
     *
     *  \param haptic The haptic device to query maximum playing effects.
     *  \return The number of effects the haptic device can play at the same time
     *          or -1 on error.
     *
     *  \sa SDL_HapticNumEffects
     *  \sa SDL_HapticQuery
     *)
    SDL_HapticNumEffectsPlaying: TSDL_HapticNumEffectsPlaying;
    (**
     *  \brief Gets the haptic device's supported features in bitwise manner.
     *
     *  Example:
     *  \code
     *  if (SDL_HapticQuery(haptic) & SDL_HAPTIC_CONSTANT) {
     *      printf("We have constant haptic effect!");
     *  }
     *  \endcode
     *
     *  \param haptic The haptic device to query.
     *  \return Haptic features in bitwise manner (OR'd).
     *
     *  \sa SDL_HapticNumEffects
     *  \sa SDL_HapticEffectSupported
     *)
    SDL_HapticQuery: TSDL_HapticQuery;
    (**
     *  \brief Gets the number of haptic axes the device has.
     *
     *  \sa SDL_HapticDirection
     *)
    SDL_HapticNumAxes: TSDL_HapticNumAxes;
    (**
     *  \brief Checks to see if effect is supported by haptic.
     *
     *  \param haptic Haptic device to check on.
     *  \param effect Effect to check to see if it is supported.
     *  \return SDL_TRUE if effect is supported, SDL_FALSE if it isn't or -1 on error.
     *
     *  \sa SDL_HapticQuery
     *  \sa SDL_HapticNewEffect
     *)
    SDL_HapticEffectSupported: TSDL_HapticEffectSupported;
    (**
     *  \brief Creates a new haptic effect on the device.
     *
     *  \param haptic Haptic device to create the effect on.
     *  \param effect Properties of the effect to create.
     *  \return The id of the effect on success or -1 on error.
     *
     *  \sa SDL_HapticUpdateEffect
     *  \sa SDL_HapticRunEffect
     *  \sa SDL_HapticDestroyEffect
     *)
    SDL_HapticNewEffect: TSDL_HapticNewEffect;
    (**
     *  \brief Updates the properties of an effect.
     *
     *  Can be used dynamically, although behaviour when dynamically changing
     *  direction may be strange.  Specifically the effect may reupload itself
     *  and start playing from the start.  You cannot change the type either when
     *  running SDL_HapticUpdateEffect().
     *
     *  \param haptic Haptic device that has the effect.
     *  \param effect Effect to update.
     *  \param data New effect properties to use.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticNewEffect
     *  \sa SDL_HapticRunEffect
     *  \sa SDL_HapticDestroyEffect
     *)
    SDL_HapticUpdateEffect: TSDL_HapticUpdateEffect;
    (**
     *  \brief Runs the haptic effect on its associated haptic device.
     *
     *  If iterations are ::SDL_HAPTIC_INFINITY, it'll run the effect over and over
     *  repeating the envelope (attack and fade) every time.  If you only want the
     *  effect to last forever, set ::SDL_HAPTIC_INFINITY in the effect's length
     *  parameter.
     *
     *  \param haptic Haptic device to run the effect on.
     *  \param effect Identifier of the haptic effect to run.
     *  \param iterations Number of iterations to run the effect. Use
     *         ::SDL_HAPTIC_INFINITY for infinity.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticStopEffect
     *  \sa SDL_HapticDestroyEffect
     *  \sa SDL_HapticGetEffectStatus
     *)
    SDL_HapticRunEffect: TSDL_HapticRunEffect;
    (**
     *  \brief Stops the haptic effect on its associated haptic device.
     *
     *  \param haptic Haptic device to stop the effect on.
     *  \param effect Identifier of the effect to stop.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticRunEffect
     *  \sa SDL_HapticDestroyEffect
     *)
    SDL_HapticStopEffect: TSDL_HapticStopEffect;
    (**
     *  \brief Destroys a haptic effect on the device.
     *
     *  This will stop the effect if it's running.  Effects are automatically
     *  destroyed when the device is closed.
     *
     *  \param haptic Device to destroy the effect on.
     *  \param effect Identifier of the effect to destroy.
     *
     *  \sa SDL_HapticNewEffect
     *)
    SDL_HapticDestroyEffect: TSDL_HapticDestroyEffect;
    (**
     *  \brief Gets the status of the current effect on the haptic device.
     *
     *  Device must support the ::SDL_HAPTIC_STATUS feature.
     *
     *  \param haptic Haptic device to query the effect status on.
     *  \param effect Identifier of the effect to query its status.
     *  \return 0 if it isn't playing, 1 if it is playing or -1 on error.
     *
     *  \sa SDL_HapticRunEffect
     *  \sa SDL_HapticStopEffect
     *)
    SDL_HapticGetEffectStatus: TSDL_HapticGetEffectStatus;
    (**
     *  \brief Sets the global gain of the device.
     *
     *  Device must support the ::SDL_HAPTIC_GAIN feature.
     *
     *  The user may specify the maximum gain by setting the environment variable
     *  SDL_HAPTIC_GAIN_MAX which should be between 0 and 100.  All calls to
     *  SDL_HapticSetGain() will scale linearly using SDL_HAPTIC_GAIN_MAX as the
     *  maximum.
     *
     *  \param haptic Haptic device to set the gain on.
     *  \param gain Value to set the gain to, should be between 0 and 100.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticQuery
     *)
    SDL_HapticSetGain: TSDL_HapticSetGain;
    (**
     *  \brief Sets the global autocenter of the device.
     *
     *  Autocenter should be between 0 and 100.  Setting it to 0 will disable
     *  autocentering.
     *
     *  Device must support the ::SDL_HAPTIC_AUTOCENTER feature.
     *
     *  \param haptic Haptic device to set autocentering on.
     *  \param autocenter Value to set autocenter to, 0 disables autocentering.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticQuery
     *)
    SDL_HapticSetAutocenter: TSDL_HapticSetAutocenter;
    (**
     *  \brief Pauses a haptic device.
     *
     *  Device must support the ::SDL_HAPTIC_PAUSE feature.  Call
     *  SDL_HapticUnpause() to resume playback.
     *
     *  Do not modify the effects nor add new ones while the device is paused.
     *  That can cause all sorts of weird errors.
     *
     *  \param haptic Haptic device to pause.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticUnpause
     *)
    SDL_HapticPause: TSDL_HapticPause;
    (**
     *  \brief Unpauses a haptic device.
     *
     *  Call to unpause after SDL_HapticPause().
     *
     *  \param haptic Haptic device to unpause.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticPause
     *)
    SDL_HapticUnpause: TSDL_HapticUnpause;
    (**
     *  \brief Stops all the currently playing effects on a haptic device.
     *
     *  \param haptic Haptic device to stop.
     *  \return 0 on success or -1 on error.
     *)
    SDL_HapticStopAll: TSDL_HapticStopAll;
    (**
     *  \brief Checks to see if rumble is supported on a haptic device.
     *
     *  \param haptic Haptic device to check to see if it supports rumble.
     *  \return SDL_TRUE if effect is supported, SDL_FALSE if it isn't or -1 on error.
     *
     *  \sa SDL_HapticRumbleInit
     *  \sa SDL_HapticRumblePlay
     *  \sa SDL_HapticRumbleStop
     *)
    SDL_HapticRumbleSupported: TSDL_HapticRumbleSupported;
    (**
     *  \brief Initializes the haptic device for simple rumble playback.
     *
     *  \param haptic Haptic device to initialize for simple rumble playback.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticOpen
     *  \sa SDL_HapticRumbleSupported
     *  \sa SDL_HapticRumblePlay
     *  \sa SDL_HapticRumbleStop
     *)
    SDL_HapticRumbleInit: TSDL_HapticRumbleInit;
    (**
     *  \brief Runs simple rumble on a haptic device
     *
     *  \param haptic Haptic device to play rumble effect on.
     *  \param strength Strength of the rumble to play as a 0-1 float value.
     *  \param length Length of the rumble to play in milliseconds.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticRumbleSupported
     *  \sa SDL_HapticRumbleInit
     *  \sa SDL_HapticRumbleStop
     *)
    SDL_HapticRumblePlay: TSDL_HapticRumblePlay;
    (**
     *  \brief Stops the simple rumble on a haptic device.
     *
     *  \param haptic Haptic to stop the rumble on.
     *  \return 0 on success or -1 on error.
     *
     *  \sa SDL_HapticRumbleSupported
     *  \sa SDL_HapticRumbleInit
     *  \sa SDL_HapticRumblePlay
     *)
    SDL_HapticRumbleStop: TSDL_HapticRumbleStop;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLHapticApi }

procedure TSDLHapticApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_NumHaptics', @SDL_NumHaptics);
  List.Add('SDL_HapticName', @SDL_HapticName);
  List.Add('SDL_HapticOpen', @SDL_HapticOpen);
  List.Add('SDL_HapticOpened', @SDL_HapticOpened);
  List.Add('SDL_HapticIndex', @SDL_HapticIndex);
  List.Add('SDL_MouseIsHaptic', @SDL_MouseIsHaptic);
  List.Add('SDL_HapticOpenFromMouse', @SDL_HapticOpenFromMouse);
  List.Add('SDL_JoystickIsHaptic', @SDL_JoystickIsHaptic);
  List.Add('SDL_HapticOpenFromJoystick', @SDL_HapticOpenFromJoystick);
  List.Add('SDL_HapticClose', @SDL_HapticClose);
  List.Add('SDL_HapticNumEffects', @SDL_HapticNumEffects);
  List.Add('SDL_HapticNumEffectsPlaying', @SDL_HapticNumEffectsPlaying);
  List.Add('SDL_HapticQuery', @SDL_HapticQuery);
  List.Add('SDL_HapticNumAxes', @SDL_HapticNumAxes);
  List.Add('SDL_HapticEffectSupported', @SDL_HapticEffectSupported);
  List.Add('SDL_HapticNewEffect', @SDL_HapticNewEffect);
  List.Add('SDL_HapticUpdateEffect', @SDL_HapticUpdateEffect);
  List.Add('SDL_HapticRunEffect', @SDL_HapticRunEffect);
  List.Add('SDL_HapticStopEffect', @SDL_HapticStopEffect);
  List.Add('SDL_HapticDestroyEffect', @SDL_HapticDestroyEffect);
  List.Add('SDL_HapticGetEffectStatus', @SDL_HapticGetEffectStatus);
  List.Add('SDL_HapticSetGain', @SDL_HapticSetGain);
  List.Add('SDL_HapticSetAutocenter', @SDL_HapticSetAutocenter);
  List.Add('SDL_HapticPause', @SDL_HapticPause);
  List.Add('SDL_HapticUnpause', @SDL_HapticUnpause);
  List.Add('SDL_HapticStopAll', @SDL_HapticStopAll);
  List.Add('SDL_HapticRumbleSupported', @SDL_HapticRumbleSupported);
  List.Add('SDL_HapticRumbleInit', @SDL_HapticRumbleInit);
  List.Add('SDL_HapticRumblePlay', @SDL_HapticRumblePlay);
  List.Add('SDL_HapticRumbleStop', @SDL_HapticRumbleStop);
end;

end.

