(**
 *  \file SDL_audio.h
 *
 *  Access to the raw audio mixing buffer for the SDL library.
 *)
unit SDL.Api.Audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLAudioApi }

  TSDLAudioApi = class(TSubLibrary)
  private type
    TSDL_GetNumAudioDrivers = function: SDL_SInt32 cdecl;
    TSDL_GetAudioDriver = function(index: SDL_SInt32): SDL_String cdecl;
    TSDL_AudioInit = function(driver_name: SDL_String): SDL_ResultCode cdecl;
    TSDL_AudioQuit = procedure cdecl;
    TSDL_GetCurrentAudioDriver = function: SDL_String cdecl;
    TSDL_OpenAudio = function(desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec): SDL_ResultCode cdecl;
    TSDL_GetNumAudioDevices = function(iscapture: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_GetAudioDeviceName = function(index: SDL_SInt32; iscapture: SDL_SInt32): SDL_String cdecl;
    TSDL_OpenAudioDevice = function(device: SDL_String; iscapture: SDL_SInt32; desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec; allowed_changes: SDL_SInt32): SDL_AudioDeviceID cdecl;
    TSDL_GetAudioStatus = function: SDL_AudioStatus cdecl;
    TSDL_GetAudioDeviceStatus = function(dev: SDL_AudioDeviceID): SDL_AudioStatus cdecl;
    TSDL_PauseAudio = procedure(pause_on: SDL_SInt32) cdecl;
    TSDL_PauseAudioDevice = procedure(dev: SDL_AudioDeviceID; pause_on: SDL_SInt32) cdecl;
    TSDL_LoadWAV_RW = function(src: PSDL_RWops; freesrc: Boolean; spec: PSDL_AudioSpec; out audio_buf: PSDL_UInt8; audio_len: PSDL_UInt32): PSDL_AudioSpec cdecl;
    TSDL_FreeWAV = procedure(audio_buf: PSDL_UInt8) cdecl;
    TSDL_BuildAudioCVT = function(cvt: PSDL_AudioCVT; src_format: SDL_AudioFormat; src_channels: SDL_UInt8; src_rate: SDL_SInt32; dst_format: SDL_AudioFormat; dst_channels: SDL_UInt8; dst_rate: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_ConvertAudio = function(cvt: PSDL_AudioCVT): SDL_ResultCode cdecl;
    TSDL_MixAudio = procedure(dst: PSDL_UInt8; src: PSDL_UInt8; len: SDL_UInt32; volume: SDL_SInt32) cdecl;
    TSDL_MixAudioFormat = procedure(dst: PSDL_UInt8; src: PSDL_UInt8; format: SDL_AudioFormat; len: SDL_UInt32; volume: SDL_SInt32) cdecl;
    TSDL_LockAudio = procedure cdecl;
    TSDL_LockAudioDevice = procedure(dev: SDL_AudioDeviceID) cdecl;
    TSDL_UnlockAudio = procedure cdecl;
    TSDL_UnlockAudioDevice = procedure(dev: SDL_AudioDeviceID) cdecl;
    TSDL_CloseAudio = procedure cdecl;
    TSDL_CloseAudioDevice = procedure(dev: SDL_AudioDeviceID) cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_QueueAudio = function(dev: SDL_AudioDeviceID; data: SDL_Data; len: SDL_UInt32): SDL_SInt32 cdecl;
    TSDL_GetQueuedAudioSize = function(dev: SDL_AudioDeviceID): SDL_UInt32 cdecl;
    TSDL_ClearQueuedAudio = procedure(dev: SDL_AudioDeviceID) cdecl;
    {$ENDIF}
  public
    (**
     *  \name Driver discovery functions
     *
     *  These functions return the list of built in audio drivers, in the
     *  order that they are normally initialized by default.
     *)
    (* @{ *)
    SDL_GetNumAudioDrivers: TSDL_GetNumAudioDrivers;
    SDL_GetAudioDriver: TSDL_GetAudioDriver;
    (* @} *)
    (**
     *  \name Initialization and cleanup
     *
     *  \internal These functions are used internally, and should not be used unless
     *            you have a specific need to specify the audio driver you want to
     *            use.  You should normally use SDL_Init() or SDL_InitSubSystem().
     *)
    (* @{ *)
    SDL_AudioInit: TSDL_AudioInit;
    SDL_AudioQuit: TSDL_AudioQuit;
    (* @} *)
    (**
     *  This function returns the name of the current audio driver, or NULL
     *  if no driver has been initialized.
     *)
    SDL_GetCurrentAudioDriver: TSDL_GetCurrentAudioDriver;
    (**
     *  This function opens the audio device with the desired parameters, and
     *  returns 0 if successful, placing the actual hardware parameters in the
     *  structure pointed to by \c obtained.  If \c obtained is NULL, the audio
     *  data passed to the callback function will be guaranteed to be in the
     *  requested format, and will be automatically converted to the hardware
     *  audio format if necessary.  This function returns -1 if it failed
     *  to open the audio device, or couldn't set up the audio thread.
     *
     *  When filling in the desired audio spec structure,
     *    - \c desired->freq should be the desired audio frequency in samples-per-
     *      second.
     *    - \c desired->format should be the desired audio format.
     *    - \c desired->samples is the desired size of the audio buffer, in
     *      samples.  This number should be a power of two, and may be adjusted by
     *      the audio driver to a value more suitable for the hardware.  Good values
     *      seem to range between 512 and 8096 inclusive, depending on the
     *      application and CPU speed.  Smaller values yield faster response time,
     *      but can lead to underflow if the application is doing heavy processing
     *      and cannot fill the audio buffer in time.  A stereo sample consists of
     *      both right and left channels in LR ordering.
     *      Note that the number of samples is directly related to time by the
     *      following formula:  \code ms = (samples*1000)/freq \endcode
     *    - \c desired->size is the size in bytes of the audio buffer, and is
     *      calculated by SDL_OpenAudio().
     *    - \c desired->silence is the value used to set the buffer to silence,
     *      and is calculated by SDL_OpenAudio().
     *    - \c desired->callback should be set to a function that will be called
     *      when the audio device is ready for more data.  It is passed a pointer
     *      to the audio buffer, and the length in bytes of the audio buffer.
     *      This function usually runs in a separate thread, and so you should
     *      protect data structures that it accesses by calling SDL_LockAudio()
     *      and SDL_UnlockAudio() in your code. Alternately, you may pass a NULL
     *      pointer here, and call SDL_QueueAudio() with some frequency, to queue
     *      more audio samples to be played.
     *    - \c desired->userdata is passed as the first parameter to your callback
     *      function. If you passed a NULL callback, this value is ignored.
     *
     *  The audio device starts out playing silence when it's opened, and should
     *  be enabled for playing by calling \c SDL_PauseAudio(0) when you are ready
     *  for your audio callback function to be called.  Since the audio driver
     *  may modify the requested size of the audio buffer, you should allocate
     *  any local mixing buffers after you open the audio device.
     *)
    SDL_OpenAudio: TSDL_OpenAudio;
    (**
     *  Get the number of available devices exposed by the current driver.
     *  Only valid after a successfully initializing the audio subsystem.
     *  Returns -1 if an explicit list of devices can't be determined; this is
     *  not an error. For example, if SDL is set up to talk to a remote audio
     *  server, it can't list every one available on the Internet, but it will
     *  still allow a specific host to be specified to SDL_OpenAudioDevice().
     *
     *  In many common cases, when this function returns a value <= 0, it can still
     *  successfully open the default device (NULL for first argument of
     *  SDL_OpenAudioDevice()).
     *)
    SDL_GetNumAudioDevices: TSDL_GetNumAudioDevices;
    (**
     *  Get the human-readable name of a specific audio device.
     *  Must be a value between 0 and (number of audio devices-1).
     *  Only valid after a successfully initializing the audio subsystem.
     *  The values returned by this function reflect the latest call to
     *  SDL_GetNumAudioDevices(); recall that function to redetect available
     *  hardware.
     *
     *  The string returned by this function is UTF-8 encoded, read-only, and
     *  managed internally. You are not to free it. If you need to keep the
     *  string for any length of time, you should make your own copy of it, as it
     *  will be invalid next time any of several other SDL functions is called.
     *)
    SDL_GetAudioDeviceName: TSDL_GetAudioDeviceName;
    (**
     *  Open a specific audio device. Passing in a device name of NULL requests
     *  the most reasonable default (and is equivalent to calling SDL_OpenAudio()).
     *
     *  The device name is a UTF-8 string reported by SDL_GetAudioDeviceName(), but
     *  some drivers allow arbitrary and driver-specific strings, such as a
     *  hostname/IP address for a remote audio server, or a filename in the
     *  diskaudio driver.
     *
     *  \return 0 on error, a valid device ID that is >= 2 on success.
     *
     *  SDL_OpenAudio(), unlike this function, always acts on device ID 1.
     *)
    SDL_OpenAudioDevice: TSDL_OpenAudioDevice;
    (**
      *  Get the current audio state.
      *)
    (* @{ *)
    SDL_GetAudioStatus: TSDL_GetAudioStatus;
    SDL_GetAudioDeviceStatus: TSDL_GetAudioDeviceStatus;
    (* @} *//* Audio status functions *)
    (**
     *  \name Pause audio functions
     *
     *  These functions pause and unpause the audio callback processing.
     *  They should be called with a parameter of 0 after opening the audio
     *  device to start playing sound.  This is so you can safely initialize
     *  data for your callback function after opening the audio device.
     *  Silence will be written to the audio device during the pause.
     *)
    (* @{ *)
    SDL_PauseAudio: TSDL_PauseAudio;
    SDL_PauseAudioDevice: TSDL_PauseAudioDevice;
    (* @} *//* Pause audio functions *)
    (**
     *  This function loads a WAVE from the data source, automatically freeing
     *  that source if \c freesrc is non-zero.  For example, to load a WAVE file,
     *  you could do:
     *  \code
     *      SDL_LoadWAV_RW(SDL_RWFromFile("sample.wav", "rb"), 1, ...);
     *  \endcode
     *
     *  If this function succeeds, it returns the given SDL_AudioSpec,
     *  filled with the audio data format of the wave data, and sets
     *  \c *audio_buf to a malloc()'d buffer containing the audio data,
     *  and sets \c *audio_len to the length of that audio buffer, in bytes.
     *  You need to free the audio buffer with SDL_FreeWAV() when you are
     *  done with it.
     *
     *  This function returns NULL and sets the SDL error message if the
     *  wave file cannot be opened, uses an unknown data format, or is
     *  corrupt.  Currently raw and MS-ADPCM WAVE files are supported.
     *)
    SDL_LoadWAV_RW: TSDL_LoadWAV_RW;
    (**
     *  This function frees data previously allocated with SDL_LoadWAV_RW()
     *)
    SDL_FreeWAV: TSDL_FreeWAV;
    (**
     *  This function takes a source format and rate and a destination format
     *  and rate, and initializes the \c cvt structure with information needed
     *  by SDL_ConvertAudio() to convert a buffer of audio data from one format
     *  to the other.
     *
     *  \return -1 if the format conversion is not supported, 0 if there's
     *  no conversion needed, or 1 if the audio filter is set up.
     *)
    SDL_BuildAudioCVT: TSDL_BuildAudioCVT;
    (**
     *  Once you have initialized the \c cvt structure using SDL_BuildAudioCVT(),
     *  created an audio buffer \c cvt->buf, and filled it with \c cvt->len bytes of
     *  audio data in the source format, this function will convert it in-place
     *  to the desired format.
     *
     *  The data conversion may expand the size of the audio data, so the buffer
     *  \c cvt->buf should be allocated after the \c cvt structure is initialized by
     *  SDL_BuildAudioCVT(), and should be \c cvt->len*cvt->len_mult bytes long.
     *)
    SDL_ConvertAudio: TSDL_ConvertAudio;
    (**
     *  This takes two audio buffers of the playing audio format and mixes
     *  them, performing addition, volume adjustment, and overflow clipping.
     *  The volume ranges from 0 - 128, and should be set to ::SDL_MIX_MAXVOLUME
     *  for full audio volume.  Note this does not change hardware volume.
     *  This is provided for convenience -- you can mix your own audio data.
     *)
    SDL_MixAudio: TSDL_MixAudio;
    (**
     *  This works like SDL_MixAudio(), but you specify the audio format instead of
     *  using the format of audio device 1. Thus it can be used when no audio
     *  device is open at all.
     *)
    SDL_MixAudioFormat: TSDL_MixAudioFormat;
    (**
     *  \name Audio lock functions
     *
     *  The lock manipulated by these functions protects the callback function.
     *  During a SDL_LockAudio()/SDL_UnlockAudio() pair, you can be guaranteed that
     *  the callback function is not running.  Do not call these from the callback
     *  function or you will cause deadlock.
     *)
    (* @{ *)
    SDL_LockAudio: TSDL_LockAudio;
    SDL_LockAudioDevice: TSDL_LockAudioDevice;
    SDL_UnlockAudio: TSDL_UnlockAudio;
    SDL_UnlockAudioDevice: TSDL_UnlockAudioDevice;
    (* @} *//* Audio lock functions *)
    (**
     *  This function shuts down audio processing and closes the audio device.
     *)
    (* @{ *)
    SDL_CloseAudio: TSDL_CloseAudio;
    SDL_CloseAudioDevice: TSDL_CloseAudioDevice;
    (* @} *)
    {$IFNDEF DISABLE_SDL2_2_0_4}
    (**
     *  Queue more audio on non-callback devices.
     *
     *  SDL offers two ways to feed audio to the device: you can either supply a
     *  callback that SDL triggers with some frequency to obtain more audio
     *  (pull method), or you can supply no callback, and then SDL will expect
     *  you to supply data at regular intervals (push method) with this function.
     *
     *  There are no limits on the amount of data you can queue, short of
     *  exhaustion of address space. Queued data will drain to the device as
     *  necessary without further intervention from you. If the device needs
     *  audio but there is not enough queued, it will play silence to make up
     *  the difference. This means you will have skips in your audio playback
     *  if you aren't routinely queueing sufficient data.
     *
     *  This function copies the supplied data, so you are safe to free it when
     *  the function returns. This function is thread-safe, but queueing to the
     *  same device from two threads at once does not promise which buffer will
     *  be queued first.
     *
     *  You may not queue audio on a device that is using an application-supplied
     *  callback; doing so returns an error. You have to use the audio callback
     *  or queue audio with this function, but not both.
     *
     *  You should not call SDL_LockAudio() on the device before queueing; SDL
     *  handles locking internally for this function.
     *
     *  \param dev The device ID to which we will queue audio.
     *  \param data The data to queue to the device for later playback.
     *  \param len The number of bytes (not samples!) to which (data) points.
     *  \return zero on success, -1 on error.
     *
     *  \sa SDL_GetQueuedAudioSize
     *  \sa SDL_ClearQueuedAudio
     *)
    SDL_QueueAudio: TSDL_QueueAudio;
    (**
     *  Get the number of bytes of still-queued audio.
     *
     *  This is the number of bytes that have been queued for playback with
     *  SDL_QueueAudio(), but have not yet been sent to the hardware.
     *
     *  Once we've sent it to the hardware, this function can not decide the exact
     *  byte boundary of what has been played. It's possible that we just gave the
     *  hardware several kilobytes right before you called this function, but it
     *  hasn't played any of it yet, or maybe half of it, etc.
     *
     *  You may not queue audio on a device that is using an application-supplied
     *  callback; calling this function on such a device always returns 0.
     *  You have to use the audio callback or queue audio with SDL_QueueAudio(),
     *  but not both.
     *
     *  You should not call SDL_LockAudio() on the device before querying; SDL
     *  handles locking internally for this function.
     *
     *  \param dev The device ID of which we will query queued audio size.
     *  \return Number of bytes (not samples!) of queued audio.
     *
     *  \sa SDL_QueueAudio
     *  \sa SDL_ClearQueuedAudio
     *)
    SDL_GetQueuedAudioSize: TSDL_GetQueuedAudioSize;
    (**
     *  Drop any queued audio data waiting to be sent to the hardware.
     *
     *  Immediately after this call, SDL_GetQueuedAudioSize() will return 0 and
     *  the hardware will start playing silence if more audio isn't queued.
     *
     *  This will not prevent playback of queued audio that's already been sent
     *  to the hardware, as we can not undo that, so expect there to be some
     *  fraction of a second of audio that might still be heard. This can be
     *  useful if you want to, say, drop any pending music during a level change
     *  in your game.
     *
     *  You may not queue audio on a device that is using an application-supplied
     *  callback; calling this function on such a device is always a no-op.
     *  You have to use the audio callback or queue audio with SDL_QueueAudio(),
     *  but not both.
     *
     *  You should not call SDL_LockAudio() on the device before clearing the
     *  queue; SDL handles locking internally for this function.
     *
     *  This function always succeeds and thus returns void.
     *
     *  \param dev The device ID of which to clear the audio queue.
     *
     *  \sa SDL_QueueAudio
     *  \sa SDL_GetQueuedAudioSize
     *)
    SDL_ClearQueuedAudio: TSDL_ClearQueuedAudio;
    {$ENDIF}
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     *  \name Audio flags
     *)
    (* @{ *)
    function SDL_AUDIO_BITSIZE(x: SDL_UInt32): SDL_UInt32;
    function SDL_AUDIO_ISFLOAT(x: SDL_UInt32): SDL_UInt32;
    function SDL_AUDIO_ISBIGENDIAN(x: SDL_UInt32): SDL_UInt32;
    function SDL_AUDIO_ISSIGNED(x: SDL_UInt32): SDL_UInt32;
    function SDL_AUDIO_ISINT(x: SDL_UInt32): SDL_UInt32;
    function SDL_AUDIO_ISLITTLEENDIAN(x: SDL_UInt32): SDL_UInt32;
    function SDL_AUDIO_ISUNSIGNED(x: SDL_UInt32): SDL_UInt32;
    (* @} *)
    (**
     *  Loads a WAV from a file.
     *  Compatibility convenience function.
     *)
    function SDL_LoadWAV(_file: PAnsiChar; spec: PSDL_AudioSpec; out audio_buf: PSDL_UInt8; audio_len: PUInt32): PSDL_AudioSpec;
  end;

implementation

{ TSDLAudioApi }

procedure TSDLAudioApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_GetNumAudioDrivers', @SDL_GetNumAudioDrivers);
  List.Add('SDL_GetAudioDriver', @SDL_GetAudioDriver);
  List.Add('SDL_AudioInit', @SDL_AudioInit);
  List.Add('SDL_AudioQuit', @SDL_AudioQuit);
  List.Add('SDL_GetCurrentAudioDriver', @SDL_GetCurrentAudioDriver);
  List.Add('SDL_OpenAudio', @SDL_OpenAudio);
  List.Add('SDL_GetNumAudioDevices', @SDL_GetNumAudioDevices);
  List.Add('SDL_GetAudioDeviceName', @SDL_GetAudioDeviceName);
  List.Add('SDL_OpenAudioDevice', @SDL_OpenAudioDevice);
  List.Add('SDL_GetAudioStatus', @SDL_GetAudioStatus);
  List.Add('SDL_GetAudioDeviceStatus', @SDL_GetAudioDeviceStatus);
  List.Add('SDL_PauseAudio', @SDL_PauseAudio);
  List.Add('SDL_PauseAudioDevice', @SDL_PauseAudioDevice);
  List.Add('SDL_LoadWAV_RW', @SDL_LoadWAV_RW);
  List.Add('SDL_FreeWAV', @SDL_FreeWAV);
  List.Add('SDL_BuildAudioCVT', @SDL_BuildAudioCVT);
  List.Add('SDL_ConvertAudio', @SDL_ConvertAudio);
  List.Add('SDL_MixAudio', @SDL_MixAudio);
  List.Add('SDL_MixAudioFormat', @SDL_MixAudioFormat);
  List.Add('SDL_LockAudio', @SDL_LockAudio);
  List.Add('SDL_LockAudioDevice', @SDL_LockAudioDevice);
  List.Add('SDL_UnlockAudio', @SDL_UnlockAudio);
  List.Add('SDL_UnlockAudioDevice', @SDL_UnlockAudioDevice);
  List.Add('SDL_CloseAudio', @SDL_CloseAudio);
  List.Add('SDL_CloseAudioDevice', @SDL_CloseAudioDevice);
{$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_QueueAudio', @SDL_QueueAudio, False);
  List.Add('SDL_GetQueuedAudioSize', @SDL_GetQueuedAudioSize, False);
  List.Add('SDL_ClearQueuedAudio', @SDL_ClearQueuedAudio, False);
{$ENDIF}
end;

function TSDLAudioApi.SDL_AUDIO_BITSIZE(x: SDL_UInt32): SDL_UInt32;
begin
  Result := x and Integer(SDL_AUDIO_MASK_BITSIZE)
end;

function TSDLAudioApi.SDL_AUDIO_ISFLOAT(x: SDL_UInt32): SDL_UInt32;
begin
  Result := x and Integer(SDL_AUDIO_MASK_DATATYPE)
end;

function TSDLAudioApi.SDL_AUDIO_ISBIGENDIAN(x: SDL_UInt32): SDL_UInt32;
begin
  Result := x and Integer(SDL_AUDIO_MASK_ENDIAN)
end;

function TSDLAudioApi.SDL_AUDIO_ISSIGNED(x: SDL_UInt32): SDL_UInt32;
begin
  Result := x and Integer(SDL_AUDIO_MASK_SIGNED)
end;

function TSDLAudioApi.SDL_AUDIO_ISINT(x: SDL_UInt32): SDL_UInt32;
begin
  Result := not SDL_AUDIO_ISFLOAT(x);
end;

function TSDLAudioApi.SDL_AUDIO_ISLITTLEENDIAN(x: SDL_UInt32): SDL_UInt32;
begin
  Result := not SDL_AUDIO_ISLITTLEENDIAN(x);
end;

function TSDLAudioApi.SDL_AUDIO_ISUNSIGNED(x: SDL_UInt32): SDL_UInt32;
begin
  Result := not SDL_AUDIO_ISSIGNED(x);
end;

function TSDLAudioApi.SDL_LoadWAV(_file: PAnsiChar; spec: PSDL_AudioSpec; out
  audio_buf: PSDL_UInt8; audio_len: PUInt32): PSDL_AudioSpec;
begin
  Result := nil;
end;

end.

