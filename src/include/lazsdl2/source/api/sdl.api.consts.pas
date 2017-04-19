unit SDL.APi.Consts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  LIB_SDL = 'SDL2';
  LIB_SDL_IMAGE = 'SDL_Image';
  LIB_OPENGL = 'OPENGL';

type

  SDLConsts = object
  public const
    (** SDL_Touch *)
    SDL_TOUCH_MOUSEID = UInt32(-1);
    CPU: String =
      {$IFDEF CPU32}
      'i368'
      {$ELSE}
        {$IFDEF CPU64}
        'x86_64'
        {$ELSE}
          {$IFDEF CPUARM}
          'arm'
          {$ELSE}
          'unsupported'
          {$ENDIF}
        {$IFEND}
      {$IFEND};
    OS: String =
      {$IFDEF WINDOWS}
        {$IFDEF CPU32}
        'win32'
        {$ELSE}
        'win64'
        {$ENDIF}
      {$ELSE}
        {$IFDEF ANDROID}
        'android'
        {$ELSE}
          {$IFDEF UNIX}
          'linux'
          {$ELSE}
          'unsupported'
          {$ENDIF}
        {$IFEND}
      {$IFEND};
  end;

  { SDLUtils }

  SDLUtils = class
  public
    class function GetLibName(LibName: String; TargetOS: String): String;
  end;

implementation

{ SDLUtils }

class function SDLUtils.GetLibName(LibName: String; TargetOS: String): String;

  function GetLibraryExt(TargetOS: string): string;
  begin
    if CompareText(copy(TargetOS,1,3), 'win') = 0 then
      Result:='.dll'
    else if CompareText(TargetOS, 'darwin') = 0 then
      Result:='.dylib'
    else if (CompareText(TargetOS, 'linux') = 0)
    or (CompareText(TargetOS, 'android') = 0)
    or (CompareText(TargetOS, 'freebsd') = 0)
    or (CompareText(TargetOS, 'openbsd') = 0)
    or (CompareText(TargetOS, 'netbsd') = 0)
    or (CompareText(TargetOS, 'haiku') = 0) then
      Result:='.so'
    else
      Result:=''
  end;

  function GetTargetOSPrefix(const TargetOS: string): string;
  begin
    Result:='';
    if (CompareText(TargetOS,'linux')=0)
    or (CompareText(TargetOS,'freebsd')=0)
    or (CompareText(TargetOS,'netbsd')=0)
    or (CompareText(TargetOS,'openbsd')=0)
    or (CompareText(TargetOS,'darwin')=0)
    or (CompareText(TargetOS,'solaris')=0)
    or (CompareText(TargetOS,'haiku')=0)
    or (CompareText(TargetOS,'android')=0)
    then
      Result:='lib'
    else
    if (CompareText(TargetOS,'win32')=0)
    or (CompareText(TargetOS,'win64')=0)
    or (CompareText(TargetOS,'wince')=0)
    then
      Result:=''
  end;

begin
  Result := GetTargetOSPrefix(TargetOS) + ChangeFileExt(LibName, GetLibraryExt(TargetOS))
end;

end.

