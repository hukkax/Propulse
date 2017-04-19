unit SDL.Android.Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Extended.Interfaces, SDL.Extended.Types, DLibrary;

type

  { TAndroidLogger }

  TAndroidLogger = class(TInterfacedObject, ILogger)
  private
    FTag: PAnsiChar;
    procedure SetTag(AValue: PAnsiChar);
  public
    procedure Log(Level: TLogLevel; Msg: String);

    property Tag: PAnsiChar read FTag write SetTag;
  end;

  { TAndroidLibLogger }

  TAndroidLibLogger = class(TAndroidLogger, ILibLogger)
  public
    procedure Log(Level: TLogPriority; Msg: String); overload;

    property Tag: PAnsiChar read FTag write SetTag;
  end;

implementation


function PrintLn(prio:TLogLevel; tag, text: PAnsiChar):longint; cdecl; varargs; external 'liblog.so' name '__android_log_print';

{ TAndroidLibLogger }

procedure TAndroidLibLogger.Log(Level: TLogPriority; Msg: String);
begin
  case Level of
    lgUnknown: Log(logUnknown, Msg);
    lgDefault: Log(logDefault, Msg);
    lgVerbose: Log(logVerbose, Msg);
    lgDebug: Log(logDebug, Msg);
    lgInfo: Log(logInfo, Msg);
    lgWarn: Log(logWarning, Msg);
    lgError: Log(logError, Msg);
    lgFatal: Log(logFatal, Msg);
    lgSilent: Log(logSilent, Msg);
  end;
end;

{ TAndroidLogger }

procedure TAndroidLogger.SetTag(AValue: PAnsiChar);
begin
  FTag:=AValue
end;

procedure TAndroidLogger.Log(Level: TLogLevel; Msg: String);
begin
  PrintLn(Level, FTag, PAnsiChar(Msg))
end;

end.

