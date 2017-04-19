unit SDL.Api.Helpers;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types;

type

  { TSDL_BoolHelper }

  TSDL_BoolHelper = record helper for SDL_Bool
  public
    function Int: SDL_SInt32;
    function Bool: Boolean;
    function ToString: String;
  end;

  { TSDL_InitFlagHelper }

  TSDL_InitFlagHelper = record helper for SDL_InitFlag
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TSDL_InitFlagsHelper }

  TSDL_InitFlagsHelper = record helper for SDL_InitFlags
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TSDL_EventStatusHelper }

  TSDL_EventStatusHelper = record helper for SDL_EventState
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TSDL_EventTypeHelper }

  TSDL_EventTypeHelper = record helper for SDL_EventType
  public
    function Valid: Boolean;
    function Int: SDL_SInt32;
    function ToString: String;
    function Next: SDL_EventType;
    function Prior: SDL_EventType;
  end;

  { TSDL_EventTypesHelper }

  TSDL_EventTypesHelper = record helper for SDL_EventTypes
  public
    function ToString: String;
  end;

  { TSDL_DisplayModeHelper }

  TSDL_DisplayModeHelper = record helper for SDL_DisplayMode
  public
    function Equals(const Mode: SDL_DisplayMode): Boolean;
  end;

  { TSDL_SwapInterval }

  TSDL_SwapInterval = record helper for SDL_SwapInterval
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TSDL_WindowFlagHelper }

  TSDL_WindowFlagHelper = record helper for SDL_WindowFlag
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TSDL_WindowFlagsHelper }

  TSDL_WindowFlagsHelper = record helper for SDL_WindowFlags
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_Surface_FlagHelper }

  TSDL_Surface_FlagHelper = record helper for SDL_Surface_Flag
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_Surface_FlagsHelper }

  TSDL_Surface_FlagsHelper = record helper for SDL_Surface_Flags
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_LogCategoryHelper }

  TSDL_LogCategoryHelper = record helper for SDL_LogCategory
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_LogPriorityHelper }

  TSDL_LogPriorityHelper = record helper for SDL_LogPriority
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_KeymodHelper }

  TSDL_KeymodHelper = record helper for SDL_Keymod
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_KeymodsHelper }

  TSDL_KeymodsHelper = record helper for SDL_Keymods
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_ButtonEnumHelper }

  TSDL_ButtonEnumHelper = record helper for SDL_ButtonEnum
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_AudioFormatHelper }

  TSDL_AudioFormatHelper = record helper for SDL_AudioFormat
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_RendererFlagHelper }

  TSDL_RendererFlagHelper = record helper for SDL_RendererFlag
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

  { TSDL_RendererFlagsHelper }

  TSDL_RendererFlagsHelper = record helper for SDL_RendererFlags
  public
    function Int: SDL_UInt32;
    function ToString: String;
  end;

implementation

uses
  typinfo;

{ TSDL_RendererFlagHelper }

function TSDL_RendererFlagHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_RendererFlagHelper.ToString: String;
begin
  case Self of
    SDL_RENDERER_SOFTWARE: Result := 'SDL_RENDERER_SOFTWARE';
    SDL_RENDERER_ACCELERATED: Result := 'SDL_RENDERER_ACCELERATED';
    SDL_RENDERER_PRESENTVSYNC: Result := 'SDL_RENDERER_PRESENTVSYNC';
    SDL_RENDERER_TARGETTEXTURE: Result := 'SDL_RENDERER_TARGETTEXTURE';
  else
    Result := 'SDL_RENDERER_UNKNOWN'
  end;
end;

{ TSDL_RendererFlagsHelper }

function TSDL_RendererFlagsHelper.Int: SDL_UInt32;
begin
  Result := Self.Value
end;

function TSDL_RendererFlagsHelper.ToString: String;
var
  Enum: SDL_RendererFlag;
begin
  Result := '';

  for Enum := Low(SDL_RendererFlag) to High(SDL_RendererFlag) do
  begin
    if Enum in Self then
    begin
      if Result = '' then
        Result := Result + Enum.ToString
      else
        Result := Result + Enum.ToString + ','
    end
  end;

  Result := '[' + Result + ']'
end;

{ TSDL_AudioFormatHelper }

function TSDL_AudioFormatHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_AudioFormatHelper.ToString: String;
begin
  case Self of
    AUDIO_U8: Result := 'AUDIO_U8';
    AUDIO_S8: Result := 'AUDIO_S8';
    AUDIO_U16LSB: Result := 'AUDIO_U16LSB';
    AUDIO_S16LSB: Result := 'AUDIO_S16LSB';
    AUDIO_U16MSB: Result := 'AUDIO_U16MSB';
    AUDIO_S16MSB: Result := 'AUDIO_S16MSB';
    AUDIO_S32LSB: Result := 'AUDIO_S32LSB';
    AUDIO_S32MSB: Result := 'AUDIO_S32MSB';
    AUDIO_F32LSB: Result := 'AUDIO_F32LSB';
    AUDIO_F32MSB: Result := 'AUDIO_F32MSB';
  else
    Result := 'AUDIO_UNKNOWN'
  end;
end;

{ TSDL_ButtonEnumHelper }

function TSDL_ButtonEnumHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_ButtonEnumHelper.ToString: String;
begin
  case Self of
    SDL_BUTTON_LEFT: Result := 'SDL_BUTTON_LEFT';
    SDL_BUTTON_MIDDLE: Result := 'SDL_BUTTON_MIDDLE';
    SDL_BUTTON_RIGHT: Result := 'SDL_BUTTON_RIGHT';
    SDL_BUTTON_X1: Result := 'SDL_BUTTON_X1';
    SDL_BUTTON_X2: Result := 'SDL_BUTTON_X2'
  else
    Result := 'SDL_BUTTON_UNKNOWN'
  end;
end;

{ TSDL_EventTypesHelper }

function TSDL_EventTypesHelper.ToString: String;
const
  cSDL_WindowFlagArray: array [0..46] of SDL_EventType = (
    SDL_INVALID,
    SDL_FIRSTEVENT,
    SDL_QUITEV,
    SDL_APP_TERMINATING,
    SDL_APP_LOWMEMORY,
    SDL_APP_WILLENTERBACKGROUND,
    SDL_APP_DIDENTERBACKGROUND,
    SDL_APP_WILLENTERFOREGROUND,
    SDL_APP_DIDENTERFOREGROUND,
    SDL_WINDOWEVENT_EV,
    SDL_SYSWMEVENT_EV,
    SDL_KEYDOWN,
    SDL_KEYUP,
    SDL_TEXTEDITING,
    SDL_TEXTINPUT,
    SDL_KEYMAPCHANGED,
    SDL_MOUSEMOTION,
    SDL_MOUSEBUTTONDOWN,
    SDL_MOUSEBUTTONUP,
    SDL_MOUSEWHEEL,
    SDL_JOYAXISMOTION,
    SDL_JOYBALLMOTION,
    SDL_JOYHATMOTION ,
    SDL_JOYBUTTONDOWN,
    SDL_JOYBUTTONUP,
    SDL_JOYDEVICEADDED,
    SDL_JOYDEVICEREMOVED,
    SDL_CONTROLLERAXISMOTION,
    SDL_CONTROLLERBUTTONDOWN,
    SDL_CONTROLLERBUTTONUP,
    SDL_CONTROLLERDEVICEADDED,
    SDL_CONTROLLERDEVICEREMOVED,
    SDL_CONTROLLERDEVICEREMAPPED,
    SDL_FINGERDOWN,
    SDL_FINGERUP,
    SDL_FINGERMOTION,
    SDL_DOLLARGESTURE,
    SDL_DOLLARRECORD,
    SDL_MULTIGESTURE,
    SDL_CLIPBOARDUPDATE,
    SDL_DROPFILE,
    SDL_AUDIODEVICEADDED,
    SDL_AUDIODEVICEREMOVED,
    SDL_RENDER_TARGETS_RESET,
    SDL_RENDER_DEVICE_RESET,
    SDL_USEREVENT_EV,
    SDL_LASTEVENT
  );
var
  i: Integer;
begin
  Result := '[';

  for i := 0 to High(cSDL_WindowFlagArray) do
  begin
    if cSDL_WindowFlagArray[i] in Self then
    begin
      if Result = '[' then
        Result := Result + cSDL_WindowFlagArray[i].ToString
      else
        Result := Result + ',' + cSDL_WindowFlagArray[i].ToString
    end
  end;

  Result := Result + ']'
end;

{ TSDL_KeymodsHelper }

function TSDL_KeymodsHelper.Int: SDL_UInt32;
begin
  Result := Self.Value
end;

function TSDL_KeymodsHelper.ToString: String;
const
  cArray: array [0..12] of SDL_Keymod = (
    KMOD_NONE,
    KMOD_LSHIFT,
    KMOD_RSHIFT,
    KMOD_LCTRL,
    KMOD_RCTRL,
    KMOD_LALT,
    KMOD_RALT,
    KMOD_LGUI,
    KMOD_RGUI,
    KMOD_NUM,
    KMOD_CAPS,
    KMOD_MODE,
    KMOD_RESERVED
  );
var
  Enum: SDL_Keymod;
begin
  Result := '[';
  for Enum in cArray do
  begin
    if Enum in Self then
    begin
      if Result = '[' then
        Result := Result + Enum.ToString
      else
        Result := Result + ',' + Enum.ToString
    end
  end;

  Result := Result + ']'
end;

{ TSDL_KeymodHelper }

function TSDL_KeymodHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_KeymodHelper.ToString: String;
begin
  case Self of
    KMOD_NONE: Result := 'KMOD_NONE';
    KMOD_LSHIFT: Result := 'KMOD_LSHIFT';
    KMOD_RSHIFT: Result := 'KMOD_RSHIFT';
    KMOD_LCTRL: Result := 'KMOD_LCTRL';
    KMOD_RCTRL: Result := 'KMOD_RCTRL';
    KMOD_LALT: Result := 'KMOD_LALT';
    KMOD_RALT: Result := 'KMOD_RALT';
    KMOD_LGUI: Result := 'KMOD_LGUI';
    KMOD_RGUI: Result := 'KMOD_RGUI';
    KMOD_NUM: Result := 'KMOD_NUM';
    KMOD_CAPS: Result := 'KMOD_CAPS';
    KMOD_MODE: Result := 'KMOD_MODE';
    KMOD_RESERVED: Result := 'KMOD_RESERVED'
  else
    Result := 'KMOD_UNKNOWN'
  end;
end;

{ TSDL_LogCategoryHelper }

function TSDL_LogCategoryHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_LogCategoryHelper.ToString: String;
begin
  case Self of
    SDL_LOG_CATEGORY_APPLICATION: Result := 'SDL_LOG_CATEGORY_APPLICATION';
    SDL_LOG_CATEGORY_ERROR: Result := 'SDL_LOG_CATEGORY_ERROR';
    SDL_LOG_CATEGORY_ASSERT: Result := 'SDL_LOG_CATEGORY_ASSERT';
    SDL_LOG_CATEGORY_SYSTEM: Result := 'SDL_LOG_CATEGORY_SYSTEM';
    SDL_LOG_CATEGORY_AUDIO: Result := 'SDL_LOG_CATEGORY_AUDIO';
    SDL_LOG_CATEGORY_VIDEO: Result := 'SDL_LOG_CATEGORY_VIDEO';
    SDL_LOG_CATEGORY_RENDER: Result := 'SDL_LOG_CATEGORY_RENDER';
    SDL_LOG_CATEGORY_INPUT: Result := 'SDL_LOG_CATEGORY_INPUT';
    SDL_LOG_CATEGORY_TEST: Result := 'SDL_LOG_CATEGORY_TEST';
    SDL_LOG_CATEGORY_RESERVED1: Result := 'SDL_LOG_CATEGORY_RESERVED1';
    SDL_LOG_CATEGORY_RESERVED2: Result := 'SDL_LOG_CATEGORY_RESERVED2';
    SDL_LOG_CATEGORY_RESERVED3: Result := 'SDL_LOG_CATEGORY_RESERVED3';
    SDL_LOG_CATEGORY_RESERVED4: Result := 'SDL_LOG_CATEGORY_RESERVED4';
    SDL_LOG_CATEGORY_RESERVED5: Result := 'SDL_LOG_CATEGORY_RESERVED5';
    SDL_LOG_CATEGORY_RESERVED6: Result := 'SDL_LOG_CATEGORY_RESERVED6';
    SDL_LOG_CATEGORY_RESERVED7: Result := 'SDL_LOG_CATEGORY_RESERVED7';
    SDL_LOG_CATEGORY_RESERVED8: Result := 'SDL_LOG_CATEGORY_RESERVED8';
    SDL_LOG_CATEGORY_RESERVED9: Result := 'SDL_LOG_CATEGORY_RESERVED9';
    SDL_LOG_CATEGORY_RESERVED10: Result := 'SDL_LOG_CATEGORY_RESERVED10';
  else
    begin
      if (Self.Int > SDL_LOG_CATEGORY_CUSTOM.Int) then
        Result := 'SDL_LOG_CATEGORY_INVALID'
      else
        Result := 'SDL_LOG_CATEGORY_CUSTOM'
    end;
  end
end;

{ TSDL_LogPriorityHelper }

function TSDL_LogPriorityHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_LogPriorityHelper.ToString: String;
begin
  case Self of
    SDL_LOG_PRIORITY_VERBOSE: Result := 'SDL_LOG_PRIORITY_VERBOSE';
    SDL_LOG_PRIORITY_DEBUG: Result := 'SDL_LOG_PRIORITY_DEBUG';
    SDL_LOG_PRIORITY_INFO: Result := 'SDL_LOG_PRIORITY_INFO';
    SDL_LOG_PRIORITY_WARN: Result := 'SDL_LOG_PRIORITY_WARN';
    SDL_LOG_PRIORITY_ERROR: Result := 'SDL_LOG_PRIORITY_ERROR';
    SDL_LOG_PRIORITY_CRITICAL: Result := 'SDL_LOG_PRIORITY_CRITICAL';
    SDL_NUM_LOG_PRIORITIES: Result := 'SDL_NUM_LOG_PRIORITIES'
  else
    Result := 'SDL_LOG_PRIORITY_UNKNOWN'
  end;
end;

{ TSDL_Surface_FlagsHelper }

function TSDL_Surface_FlagsHelper.Int: SDL_UInt32;
begin
  Result := Self.Value
end;

function TSDL_Surface_FlagsHelper.ToString: String;
const
  cArray: array [0..3] of SDL_Surface_Flag = (
    SDL_SWSURFACE,
    SDL_PREALLOC,
    SDL_RLEACCEL,
    SDL_DONTFREE
  );
var
  Enum: SDL_Surface_Flag;
begin
  Result := '[';
  for Enum in cArray do
  begin
    if Enum in Self then
    begin
      if Result = '[' then
        Result := Result + Enum.ToString
      else
        Result := Result + ',' + Enum.ToString
    end
  end;

  Result := Result + ']'
end;

{ TSDL_Surface_FlagHelper }

function TSDL_Surface_FlagHelper.Int: SDL_UInt32;
begin
  Result := Ord(Self)
end;

function TSDL_Surface_FlagHelper.ToString: String;
begin
  case Self of
    SDL_SWSURFACE: Result := 'SDL_SWSURFACE';
    SDL_PREALLOC: Result := 'SDL_PREALLOC';
    SDL_RLEACCEL: Result := 'SDL_RLEACCEL';
    SDL_DONTFREE: Result := 'SDL_DONTFREE'
  else
    Result := 'SDL_SURFACE_UNKNOWN'
  end;
end;

{ TSDL_InitFlagsHelper }

function TSDL_InitFlagsHelper.Int: SDL_SInt32;
begin
  Result := Self.Value;
end;

function TSDL_InitFlagsHelper.ToString: String;
const
  cArray: array [0..7] of SDL_InitFlag = (
    SDL_INIT_TIMER,
    SDL_INIT_AUDIO,
    SDL_INIT_VIDEO,
    SDL_INIT_JOYSTICK,
    SDL_INIT_HAPTIC,
    SDL_INIT_GAMECONTROLLER,
    SDL_INIT_EVENTS,
    SDL_INIT_NOPARACHUTE
  );
var
  Enum: SDL_InitFlag;
begin
  Result := '[';
  for Enum in cArray do
  begin
    if Enum in Self then
    begin
      if Result = '[' then
        Result := Result + Enum.ToString
      else
        Result := Result + ',' + Enum.ToString
    end
  end;

  Result := Result + ']'
end;

{ TSDL_InitFlagHelper }

function TSDL_InitFlagHelper.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TSDL_InitFlagHelper.ToString: String;
begin
  case Self of
    SDL_INIT_TIMER: Result := 'SDL_INIT_TIMER';
    SDL_INIT_AUDIO: Result := 'SDL_INIT_AUDIO';
    SDL_INIT_VIDEO: Result := 'SDL_INIT_VIDEO';
    SDL_INIT_JOYSTICK: Result := 'SDL_INIT_JOYSTICK';
    SDL_INIT_HAPTIC: Result := 'SDL_INIT_HAPTIC';
    SDL_INIT_GAMECONTROLLER: Result := 'SDL_INIT_GAMECONTROLLER';
    SDL_INIT_EVENTS: Result := 'SDL_INIT_EVENTS';
    SDL_INIT_NOPARACHUTE: Result := 'SDL_INIT_NOPARACHUTE';
    SDL_INIT_EVERYTHING: Result := 'SDL_INIT_EVERYTHING'
  else
    Result := 'SDL_INIT_UNKNOWN'
  end;
end;

{ TSDL_WindowFlagsHelper }

function TSDL_WindowFlagsHelper.Int: SDL_UInt32;
begin
  Result := Self.Value;
end;

function TSDL_WindowFlagsHelper.ToString: String;
const
  cSDL_WindowFlagArray: array [0..13] of SDL_WindowFlag = (
    SDL_WINDOW_FULLSCREEN,
    SDL_WINDOW_OPENGL,
    SDL_WINDOW_SHOWN,
    SDL_WINDOW_HIDDEN,
    SDL_WINDOW_BORDERLESS,
    SDL_WINDOW_RESIZABLE,
    SDL_WINDOW_MINIMIZED,
    SDL_WINDOW_MAXIMIZED,
    SDL_WINDOW_INPUT_GRABBED,
    SDL_WINDOW_INPUT_FOCUS,
    SDL_WINDOW_MOUSE_FOCUS,
    SDL_WINDOW_FOREIGN,
    SDL_WINDOW_FULLSCREEN_DESKTOP,
    SDL_WINDOW_ALLOW_HIGHDPI
  );
var
  i: Integer;
begin
  Result := '[';

  for i := 0 to High(cSDL_WindowFlagArray) do
  begin
    if cSDL_WindowFlagArray[i] in Self then
    begin
      if Result = '[' then
        Result := Result + cSDL_WindowFlagArray[i].ToString
      else
        Result := Result + ',' + cSDL_WindowFlagArray[i].ToString
    end
  end;

  Result := Result + ']'
end;

{ TSDL_WindowFlagHelper }

function TSDL_WindowFlagHelper.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TSDL_WindowFlagHelper.ToString: String;
begin
  case Self of
    SDL_WINDOW_FULLSCREEN: Result := 'SDL_WINDOW_FULLSCREEN';
    SDL_WINDOW_OPENGL: Result := 'SDL_WINDOW_OPENGL';
    SDL_WINDOW_SHOWN: Result := 'SDL_WINDOW_SHOWN';
    SDL_WINDOW_HIDDEN: Result := 'SDL_WINDOW_HIDDEN';
    SDL_WINDOW_BORDERLESS: Result := 'SDL_WINDOW_BORDERLESS';
    SDL_WINDOW_RESIZABLE: Result := 'SDL_WINDOW_RESIZABLE';
    SDL_WINDOW_MINIMIZED: Result := 'SDL_WINDOW_MINIMIZED';
    SDL_WINDOW_MAXIMIZED: Result := 'SDL_WINDOW_MAXIMIZED';
    SDL_WINDOW_INPUT_GRABBED: Result := 'SDL_WINDOW_INPUT_GRABBED';
    SDL_WINDOW_INPUT_FOCUS: Result := 'SDL_WINDOW_INPUT_FOCUS';
    SDL_WINDOW_MOUSE_FOCUS: Result := 'SDL_WINDOW_MOUSE_FOCUS';
    SDL_WINDOW_FOREIGN: Result := 'SDL_WINDOW_FOREIGN';
    SDL_WINDOW_FULLSCREEN_DESKTOP: Result := 'SDL_WINDOW_FULLSCREEN_DESKTOP';
    SDL_WINDOW_ALLOW_HIGHDPI: Result := 'SDL_WINDOW_ALLOW_HIGHDPI'
  else
    Result := 'SDL_WINDOW_UNKNOWN'
  end;
end;

{ TSDL_SwapInterval }

function TSDL_SwapInterval.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TSDL_SwapInterval.ToString: String;
begin
  case Self of
    SDL_TEARING: Result := 'SDL_TEARING';
    SDL_IMMEDIATE: Result := 'SDL_IMMEDIATE';
    SDL_SYNCHRONIZED: Result := 'SDL_SYNCHRONIZED'
  else
    Result := 'SDL_INTERVAL_UNKNOWN'
  end;
end;

{ TSDL_DisplayModeHelper }

function TSDL_DisplayModeHelper.Equals(const Mode: SDL_DisplayMode): Boolean;
begin
  Result :=
    (Self.format = Mode.format) and
    (Self.h = Mode.h) and
    (Self.w = Mode.w) and
    (Self.refresh_rate = Mode.refresh_rate)
end;

{ TSDL_BoolHelper }

function TSDL_BoolHelper.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TSDL_BoolHelper.Bool: Boolean;
begin
  Result := Self = SDL_TRUE
end;

function TSDL_BoolHelper.ToString: String;
begin
  Result := GetEnumName(TypeInfo(SDL_Bool), Ord(Self))
end;

{ TSDL_EventTypeHelper }

function TSDL_EventTypeHelper.Valid: Boolean;
begin
  Result := SDL_UInt32(Ord(Self)) <> SDL_UInt32(-1)
end;

function TSDL_EventTypeHelper.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TSDL_EventTypeHelper.ToString: String;
begin
  case Self of
    SDL_INVALID: Result := 'SDL_INVALID';
    SDL_FIRSTEVENT: Result := 'SDL_FIRSTEVENT';
    SDL_WINDOWEVENT_EV: Result := 'SDL_WINDOWEVENT';
    SDL_KEYDOWN: Result := 'SDL_KEYDOWN';
    SDL_KEYUP: Result := 'SDL_KEYUP';
    SDL_TEXTEDITING: Result := 'SDL_TEXTEDITING';
    SDL_TEXTINPUT: Result := 'SDL_TEXTINPUT';
    SDL_MOUSEMOTION: Result := 'SDL_MOUSEMOTION';
    SDL_MOUSEBUTTONDOWN: Result := 'SDL_MOUSEBUTTONDOWN';
    SDL_MOUSEBUTTONUP: Result := 'SDL_MOUSEBUTTONUP';
    SDL_MOUSEWHEEL: Result := 'SDL_MOUSEWHEEL';
    SDL_JOYAXISMOTION: Result := 'SDL_JOYAXISMOTION';
    SDL_JOYBALLMOTION: Result := 'SDL_JOYBALLMOTION';
    SDL_JOYHATMOTION: Result := 'SDL_JOYHATMOTION';
    SDL_JOYBUTTONDOWN: Result := 'SDL_JOYBUTTONDOWN';
    SDL_JOYBUTTONUP: Result := 'SDL_JOYBUTTONUP';
    SDL_JOYDEVICEADDED: Result := 'SDL_JOYDEVICEADDED';
    SDL_JOYDEVICEREMOVED: Result := 'SDL_JOYDEVICEREMOVED';
    SDL_CONTROLLERAXISMOTION: Result := 'SDL_CONTROLLERAXISMOTION';
    SDL_CONTROLLERBUTTONDOWN: Result := 'SDL_CONTROLLERBUTTONDOWN';
    SDL_CONTROLLERBUTTONUP: Result := 'SDL_CONTROLLERBUTTONUP';
    SDL_CONTROLLERDEVICEADDED: Result := 'SDL_CONTROLLERDEVICEADDED';
    SDL_CONTROLLERDEVICEREMOVED: Result := 'SDL_CONTROLLERDEVICEREMOVED';
    SDL_CONTROLLERDEVICEREMAPPED: Result := 'SDL_CONTROLLERDEVICEREMAPPED';
    SDL_QUITEV: Result := 'SDL_QUITEV';
    SDL_USEREVENT_EV: Result := 'SDL_USEREVENT';
    SDL_SYSWMEVENT_EV: Result := 'SDL_SYSWMEVENT';
    SDL_FINGERDOWN: Result := 'SDL_FINGERDOWN';
    SDL_FINGERUP: Result := 'SDL_FINGERUP';
    SDL_FINGERMOTION: Result := 'SDL_FINGERMOTION';
    SDL_DOLLARGESTURE: Result := 'SDL_DOLLARGESTURE';
    SDL_DOLLARRECORD: Result := 'SDL_DOLLARRECORD';
    SDL_MULTIGESTURE: Result := 'SDL_MULTIGESTURE';
    SDL_DROPFILE: Result := 'SDL_DROPFILE';
    SDL_LASTEVENT: Result := 'SDL_LASTEVENT'
  else
    Result := 'SDL_USEREVENT_EXT'
  end;
end;

function TSDL_EventTypeHelper.Next: SDL_EventType;
begin
  Result := SDL_EventType(Ord(Self) + 1)
end;

function TSDL_EventTypeHelper.Prior: SDL_EventType;
begin
  Result := SDL_EventType(Ord(Self) - 1)
end;

{ TSDL_EV_STATUSS_EnumHelper }

function TSDL_EventStatusHelper.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TSDL_EventStatusHelper.ToString: String;
begin
  case Self of
    SDL_QUERY: Result := 'SDL_QUERY';
    SDL_DISABLE: Result := 'SDL_DISABLE';
    SDL_ENABLE: Result := 'SDL_ENABLE';
  else
    Result := 'SDL_STATE_UNKNOWN';
  end;
end;

end.

