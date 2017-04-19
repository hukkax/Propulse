unit SDL.Api.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SDL.Api.Options, sysutils;

type
  SDL_SInt8  = Int8;
  SDL_SInt16 = Int16;
  SDL_SInt32 = Int32;
  SDL_SInt64 = Int64;

  PSDL_SInt8  = ^SDL_SInt8;
  PSDL_SInt16 = ^SDL_SInt16;
  PSDL_SInt32 = ^SDL_SInt32;
  PSDL_SInt64 = ^SDL_SInt64;

  SDL_UInt8  = UInt8;
  SDL_UInt16 = UInt16;
  SDL_UInt32 = UInt32;
  SDL_UInt64 = UInt64;

  PSDL_UInt8  = ^SDL_UInt8;
  PSDL_UInt16 = ^SDL_UInt16;
  PSDL_UInt32 = ^SDL_UInt32;
  PSDL_UInt64 = ^SDL_UInt64;

  SDL_Float = Single;
  SDL_Double = Double;

  PSDL_Float = ^SDL_Float;

  {$ifdef CPU64}
  SDL_Size_t = QWord;
  {$ENDIF}
  {$ifdef CPU32}
  SDL_Size_t = DWord;
  {$ENDIF}

  {$IFDEF WINDOWS}
  SDL_WideChar = WideChar;
  SDL_WideString = PWideChar;
  {$ELSE}
  SDL_WideChar = UCS4Char;
  SDL_WideString = PUCS4Char;
  {$ENDIF}
  SDL_String = PAnsiChar;
  PSDL_String = ^SDL_String;
  SDL_Data = Pointer;
  PSDL_Data = ^SDL_Data;
  SDL_Pointer = Pointer;
  SDL_LibHandle = Pointer;

  SDL_ErrorString = array [1..127] of AnsiChar;

  SDLException = class(Exception);
  SDLProviderException = class(SDLException);
  SDLWindowException = class(SDLException);
  SDL_ImageException = class(SDLException);
  SDLCriticalException = class(SDLException);

{$REGION 'Enums'}
  SDL_Bool = (
    SDL_FALSE := 0,
    SDL_TRUE := 1
  );
  SDL_InitFlag = (
    SDL_INIT_NOTHING := $00000000,       (** implies that nothing to initialize *)
    SDL_INIT_TIMER := $00000001,
    SDL_INIT_AUDIO := $00000010,
    SDL_INIT_VIDEO := $00000020,          (** SDL_INIT_VIDEO implies SDL_INIT_EVENTS *)
    SDL_INIT_JOYSTICK := $00000200,       (** SDL_INIT_JOYSTICK implies SDL_INIT_EVENTS *)
    SDL_INIT_HAPTIC := $00001000,
    SDL_INIT_GAMECONTROLLER := $00002000, (**< SDL_INIT_GAMECONTROLLER implies SDL_INIT_JOYSTICK *)
    SDL_INIT_EVENTS := $00004000,
    SDL_INIT_NOPARACHUTE := $00100000,    (**< compatibility; this flag is ignored. *)
    SDL_INIT_EVERYTHING = Integer(SDL_INIT_TIMER) or
                          Integer(SDL_INIT_AUDIO) or
                          Integer(SDL_INIT_VIDEO) or
                          Integer(SDL_INIT_JOYSTICK) or
                          Integer(SDL_INIT_HAPTIC) or
                          Integer(SDL_INIT_GAMECONTROLLER)
  {%H-});

  SDL_SwapInterval = (
    SDL_TEARING := -1 ,
    SDL_IMMEDIATE := 0,
    SDL_SYNCHRONIZED := 1
  );

{$ENDREGION}

  SDL_InitFlags = record Value: SDL_UInt32; end;

  SDL_ResultCode = record ResultCode: SDL_SInt32; end;

{$i sdl.api.assert.inc}
{$i sdl.api.atomic.inc}
{$i sdl.api.audio.inc}
{$i sdl.api.blendmode.inc}
{$i sdl.api.gesture.inc}
{$i sdl.api.haptic.inc}
{$i sdl.api.hints.inc}
{$i sdl.api.joystick.inc}
{$i sdl.api.gamecontroller.inc}
{$i sdl.api.scancode.inc}
{$i sdl.api.keycode.inc}
{$i sdl.api.keyboard.inc}
{$i sdl.api.log.inc}
{$i sdl.api.mouse.inc}
{$i sdl.api.pixels.inc}
{$i sdl.api.power.inc}
{$i sdl.api.rect.inc}
{$i sdl.api.render.inc}
{$i sdl.api.rwops.inc}
{$i sdl.api.shape.inc}
{$i sdl.api.timer.inc}
{$i sdl.api.touch.inc}
{$i sdl.api.version.inc}
{$i sdl.api.error.inc}
{$i sdl.api.threads.inc}
{$i sdl.api.mutex.inc}
{$i sdl.api.surface.inc}
{$i sdl.api.video.inc}
{$i sdl.api.messagebox.inc}
{$i sdl.api.events.inc}
{$i sdl.api.operators.h.inc}
{$i sdl.api.libsdl_image.inc}

{$REGION 'Operators'}

operator := (const Value: SDL_ResultCode): Boolean; inline;
operator not (const Value: SDL_ResultCode): Boolean; inline;
operator := (const Value: SDL_ResultCode): SDL_SInt32; inline;
operator = (const Left: SDL_ResultCode; Right: SDL_SInt32): Boolean; inline;
operator > (const Left: SDL_ResultCode; Right: SDL_SInt32): Boolean; inline;
operator < (const Left: SDL_ResultCode; Right: SDL_SInt32): Boolean; inline;
{$IFDEF UNIX}
operator := (const Value: String): SDL_WideString;
operator := (const Value: SDL_WideString): String;
{$ENDIF}

{$ENDREGION}

implementation

uses
{$IFDEF UNIX}
  math,
{$ENDIF}
  SDL.Api.Helpers;

{$i sdl.api.operators.b.inc}

{ SDL_Color }

class function SDL_Color.Make(const Rv, Gv, Bv, Av: Byte): SDL_Color;
begin
  Result.r := Rv;
  Result.g := Gv;
  Result.b := Bv;
  Result.a := Av;
end;

{$REGION 'SDL Types'}

operator:=(const Value: SDL_ResultCode): Boolean;
begin
  Result := Value.ResultCode = 0
end;

operator = (const Left: SDL_ResultCode; Right: SDL_SInt32): Boolean; inline;
begin
  Result := Left.ResultCode = Right;
end;

operator > (const Left: SDL_ResultCode; Right: SDL_SInt32): Boolean; inline;
begin
  Result := Left.ResultCode > Right;
end;

operator < (const Left: SDL_ResultCode; Right: SDL_SInt32): Boolean; inline;
begin
  Result := Left.ResultCode < Right;
end;

operator not (const Value: SDL_ResultCode): Boolean;
begin
  Result := not (Value.ResultCode = 0)
end;

operator := (const Value: SDL_ResultCode): SDL_SInt32;
begin
  Result := Value.ResultCode
end;

{$IFDEF UNIX}

operator := (const Value: String): SDL_WideString;
begin
  Result := PUCS4Char(UnicodeStringToUCS4String(UnicodeString(Value)))
end;

operator := (const Value: SDL_WideString): String;
var
  Str: array of UCS4Char;
  len: Integer;
  Iter: SDL_WideString;
begin
  SetLength(Str, 10);

  len := 0;
  Iter := Value;
  while (Iter^ <> 0) do
  begin
    if Length(Str) = len then
      SetLength(Str, Min(10, len + len div 4));
    Str[len] := Iter^;
    Inc(len);
    Inc(Iter, 1);
  end;

  SetLength(Str, len + 1);

  Result := String(UCS4StringToUnicodeString(Str))
end;

{$ENDIF}

{ SDLKeyStatesArr }

function SDLKeyStatesArr.GetItem(Index: SDL_Scancode): ByteBool;
begin
  Result := Data[Ord(Index)]
end;

{$ENDREGION}

end.

