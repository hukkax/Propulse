
type

  PSDL_AudioSpec = ^SDL_AudioSpec;
  PSDL_AudioCVT = ^SDL_AudioCVT;

  (**
   *  SDL Audio Device IDs.
   *
   *  A successful call to SDL_OpenAudio() is always device id 1, and legacy
   *  SDL audio APIs assume you want this device ID. SDL_OpenAudioDevice() calls
   *  always returns devices >= 2 on success. The legacy calls are good both
   *  for backwards compatibility and when you don't care about multiple,
   *  specific, or capture devices.
   *)
  SDL_AudioDeviceID = SDL_UInt32;

{$PACKENUM 2}

  (**
   *  \brief Audio format flags.
   *
   *  These are what the 16 bits in SDL_AudioFormat currently mean...
   *  (Unspecified bits are always zero).
   *
   *  \verbatim
      ++-----------------------sample is signed if set
      ||
      ||       ++-----------sample is bigendian if set
      ||       ||
      ||       ||          ++---sample is float if set
      ||       ||          ||
      ||       ||          || +---sample bit size---+
      ||       ||          || |                     |
      15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
      \endverbatim
   *
   *  There are macros in SDL 2.0 and later to query these bits.
   *)
  SDL_AudioFormat = (
    AUDIO_U8      = $0008,  {**< Unsigned 8-bit samples *}
    AUDIO_S8      = $8008,  {**< Signed 8-bit samples *}
    AUDIO_U16LSB  = $0010{%H-},  {**< Unsigned 16-bit samples *}
    AUDIO_S16LSB  = $8010,  {**< Signed 16-bit samples *}
    AUDIO_U16MSB  = $1010,  {**< As above, but big-endian byte order *}
    AUDIO_S16MSB  = $9010,  {**< As above, but big-endian byte order *}
    AUDIO_U16     = AUDIO_U16LSB,
    AUDIO_S16     = AUDIO_S16LSB,
    AUDIO_S32LSB  = $8020,  {**< 32-bit integer samples *}
    AUDIO_S32MSB  = $9020,  {**< As above, but big-endian byte order *}
    AUDIO_S32     = AUDIO_S32LSB,
    AUDIO_F32LSB  = $8120,  {**< 32-bit floating point samples *}
    AUDIO_F32MSB  = $9120,  {**< As above, but big-endian byte order *}
    AUDIO_F32     = AUDIO_F32LSB,
  {$IF DEFINED(ENDIAN_LITTLE)}
    AUDIO_U16SYS = AUDIO_U16LSB,
    AUDIO_S16SYS = AUDIO_S16LSB,
    AUDIO_S32SYS = AUDIO_S32LSB,
    AUDIO_F32SYS = AUDIO_F32LSB
  {$ELSEIF DEFINED(ENDIAN_BIG)}
    AUDIO_U16SYS = AUDIO_U16MSB,
    AUDIO_S16SYS = AUDIO_S16MSB,
    AUDIO_S32SYS = AUDIO_S32MSB,
    AUDIO_F32SYS = AUDIO_F32MSB
  {$ELSE}
     {$FATAL Cannot determine endianness.}
  {$ENDIF}
  );

{$PACKENUM 4}

  SDL_AudioMask = (
    SDL_AUDIO_MASK_BITSIZE      = ($FF),
    SDL_AUDIO_MASK_DATATYPE     = (1 shl 8),
    SDL_AUDIO_MASK_ENDIAN       = (1 shl 12),
    SDL_AUDIO_MASK_SIGNED       = (1 shl 15)
  );

  (**
   *  \name Audio state
   *)
  SDL_AudioStatus = (
      SDL_AUDIO_STOPPED = 0,
      SDL_AUDIO_PLAYING,
      SDL_AUDIO_PAUSED
  );

  (**
   *  This function is called when the audio device needs more data.
   *
   *  \param userdata An application-specific parameter saved in
   *                  the SDL_AudioSpec structure
   *  \param stream A pointer to the audio data buffer.
   *  \param len    The length of that buffer in bytes.
   *
   *  Once the callback returns, the buffer will no longer be valid.
   *  Stereo samples are stored in a LRLRLR ordering.
   *
   *  You can choose to avoid callbacks and use SDL_QueueAudio() instead, if
   *  you like. Just open your audio device with a NULL callback.
   *)
  TSDL_AudioCallback = procedure(userdata: SDL_Data; stream: PSDL_UInt8; len: SDL_SInt32);

  SDL_AudioSpec = record
    freq: SDL_SInt32;             (** DSP frequency -- samples per second *)
    format: SDL_AudioFormat;      (** Audio data format *)
    channels: SDL_UInt8;          (** Number of channels: 1 mono, 2 stereo *)
    silence: SDL_UInt8;           (** Audio buffer silence value (calculated) *)
    samples: SDL_UInt16;          (** Audio buffer size in samples (power of 2) *)
    padding: SDL_UInt16;          (** Necessary for some compile environments *)
    size: SDL_UInt32;             (** Audio buffer size in bytes (calculated) *)
    callback: TSDL_AudioCallback; (** Callback that feeds the audio device (NULL to use SDL_QueueAudio()). *)
    userdata: SDL_Data;           (** Userdata passed to callback (ignored for NULL callbacks). *)
  end;

  TSDL_AudioFilter = procedure(cvt: PSDL_AudioCVT; format: SDL_AudioFormat);

  (**
   *  A structure to hold a set of audio conversion filters and buffers.
   *)
  SDL_AudioCVT = record
    needed: SDL_SInt32;                       (** Set to 1 if conversion possible *)
    src_format: SDL_AudioFormat;              (** Source audio format *)
    dst_format: SDL_AudioFormat;              (** Target audio format *)
    rate_incr: SDL_Double;                    (** Rate conversion increment *)
    buf: PSDL_UInt8;                          (** Buffer to hold entire audio data *)
    len: SDL_SInt32;                          (** Length of original audio buffer *)
    len_cvt: SDL_SInt32;                      (** Length of converted audio buffer *)
    len_mult: SDL_SInt32;                     (** buffer must be len*len_mult big *)
    len_ratio: SDL_Double;                    (** Given len, final size is len*len_ratio *)
    filters: array[0..9] of TSDL_AudioFilter; (** Filter list *)
    filter_index: SDL_SInt32;                 (** Current audio conversion function *)
  end;

