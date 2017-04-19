unit Event.Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Extended.Interfaces, SDL.Extended.Types, eventlog;

type

  { TEventLogger }

  TEventLogger = class(TInterfacedObject, ILogger)
  private
    FEventLog: TEventLog;
    FOwnsLogger: Boolean;
  public
    constructor Create(EventLog: TEventLog; OwnsLogger: Boolean); reintroduce;
    procedure BeforeDestruction; override;

    procedure Log(Level: TLogLevel; Msg: String);
  end;

implementation

{ TEventLogger }

constructor TEventLogger.Create(EventLog: TEventLog; OwnsLogger: Boolean);
begin
  FEventLog := EventLog;
  FOwnsLogger := OwnsLogger
end;

procedure TEventLogger.BeforeDestruction;
begin
  if FOwnsLogger then
    FEventLog.Free;

  inherited BeforeDestruction
end;

procedure TEventLogger.Log(Level: TLogLevel; Msg: String);
begin
  case Level of
    logUnknown: FEventLog.Log(etInfo, Msg);
    logDefault: FEventLog.Log(etInfo, Msg);
    logVerbose: FEventLog.Log(etInfo, Msg);
    logDebug: FEventLog.Log(etDebug, Msg);
    logInfo: FEventLog.Log(etInfo, Msg);
    logWarning: FEventLog.Log(etWarning, Msg);
    logError: FEventLog.Log(etError, Msg);
    logFatal: FEventLog.Log(etError, Msg);
    logSilent:;
  end
end;

end.

