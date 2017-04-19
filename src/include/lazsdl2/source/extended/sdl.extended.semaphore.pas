unit SDL.Extended.Semaphore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SDL.Extended.LibObject, SDL.Api.Mutex, SDL.Api.Types;

type

  { TSDLCustomSemaphore }

  TSDLCustomSemaphore = class(TSDLObject)
  private
    FResources: SDL_SInt32;
    FSemaphore: PSDL_semaphore;
    FSDLMutexApi: TSDLMutexApi;
    function GetResources: SDL_SInt32;
    procedure SetResources(AValue: SDL_SInt32);
  protected
    procedure DoReload; override;
  public
    (**
      * \brief Try to take one resource in given time.
      *
      * \param Time Minimum value in miliseconds witch is alocated to try to get resource
      *
      * \return Returns true if resource is got and return false if semofore did not get resource in given time
      *)
    function Take(Time: SDL_SInt32): Boolean;
    (**
      * \brief Take resource. If resource is not available then wait until it is available.
      *)
    procedure Take;
    (**
      * \brief Give one resource to semaphore.
      *)
    procedure Give;

    property Resources: SDL_SInt32 read GetResources write SetResources; (** Ammount of avaible resources *)
  end;

  { TSDLSemaphore }

  TSDLSemaphore = class(TSDLCustomSemaphore)
  published
    property Provider;
    property Resources;
  end;

implementation

{ TSDLCustomSemaphore }

procedure TSDLCustomSemaphore.SetResources(AValue: SDL_SInt32);
begin
  if (FResources=AValue) or (FResources < 0) then
    Exit;

  if Assigned(FSDLMutexApi) then
  begin
    while FSDLMutexApi.SDL_SemValue(FSemaphore) <> AValue do
    begin
      if FSDLMutexApi.SDL_SemValue(FSemaphore) > AValue then
        FSDLMutexApi.SDL_SemWaitTimeout(FSemaphore, 0)
      else
        FSDLMutexApi.SDL_SemPost(FSemaphore)
    end
  end;

  FResources:=AValue;
end;

function TSDLCustomSemaphore.GetResources: SDL_SInt32;
begin
  if Assigned(FSDLMutexApi) then
    Result := FSDLMutexApi.SDL_SemValue(FSemaphore)
  else
    Result := FResources
end;

procedure TSDLCustomSemaphore.DoReload;
begin
  if Assigned(Provider) and Provider.Active then
  begin
    FSDLMutexApi := TSDLMutexApi.Create(Provider.Libs['SDL2']);
    FSDLMutexApi.Init;

    if not FSDLMutexApi.Valid then
      RaiseSDLException;

    FSemaphore := FSDLMutexApi.SDL_CreateSemaphore(FResources);
  end
  else
  begin
    if Assigned(FSemaphore) then
      FSDLMutexApi.SDL_DestroySemaphore(FSemaphore);
    FSemaphore := nil;

    FreeAndNil(FSDLMutexApi);
  end
end;

function TSDLCustomSemaphore.Take(Time: SDL_SInt32): Boolean;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    if Time = 0 then
      Rez := FSDLMutexApi.SDL_SemTryWait(FSemaphore)
    else
      Rez := FSDLMutexApi.SDL_SemWaitTimeout(FSemaphore, Time);

    if Rez < 0 then
      RaiseSDLException
    else
      Result := Rez
  end
  else
    RaiseProviderException;
end;

procedure TSDLCustomSemaphore.Take;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_SemWait(FSemaphore);

    if not Rez then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

procedure TSDLCustomSemaphore.Give;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_SemPost(FSemaphore);

    if not Rez then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

end.
