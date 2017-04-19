unit SDL.Extended.ConditionVariable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SDL.Extended.LibObject, SDL.Api.Mutex, SDL.Api.Types, SDL.Extended.Mutex;

type

  { TSDLCustomConditionVariable }

  TSDLCustomConditionVariable = class(TSDLObject)
  private
    FCond: PSDL_cond;
    FSDLMutexApi: TSDLMutexApi;
  protected
    procedure DoReload; override;
  public
    procedure Signal;
    procedure Broadcast;
    procedure Wait(const Mutex: TSDLMutex);
    function WaitTimeout(const Mutex: TSDLMutex; Time: SDL_SInt32): Boolean;
  end;

  TSDLConditionVariable = class(TSDLCustomConditionVariable)
  published
    property Provider;
  end;

implementation

{ TSDLCustomConditionVariable }

procedure TSDLCustomConditionVariable.DoReload;
begin
  if Assigned(Provider) and Provider.Active then
  begin
    FSDLMutexApi := TSDLMutexApi.Create(Provider.Libs['SDL2']);
    FSDLMutexApi.Init;

    if not FSDLMutexApi.Valid then
      RaiseSDLException;

    FCond := FSDLMutexApi.SDL_CreateCond();
  end
  else
  begin
    if Assigned(FCond) then
      FSDLMutexApi.SDL_DestroyCond(FCond);
    FCond := nil;
    FreeAndNil(FSDLMutexApi);
  end;
end;

procedure TSDLCustomConditionVariable.Signal;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_CondSignal(FCond);
    if Rez < 0 then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

procedure TSDLCustomConditionVariable.Broadcast;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_CondBroadcast(FCond);
    if Rez < 0 then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

procedure TSDLCustomConditionVariable.Wait(const Mutex: TSDLMutex);
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_CondWait(FCond, Mutex.Mutex);
    if Rez < 0 then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

function TSDLCustomConditionVariable.WaitTimeout(const Mutex: TSDLMutex; Time: SDL_SInt32): Boolean;
var
  Rez: SDL_ResultCode;
begin
  Result := False;

  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_CondWaitTimeout(FCond, Mutex.Mutex, Time);
    if Rez < 0 then
      RaiseSDLException
    else
      Result := Rez
  end
  else
    RaiseProviderException
end;

end.
