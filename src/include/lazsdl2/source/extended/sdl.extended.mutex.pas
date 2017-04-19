unit SDL.Extended.Mutex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SDL.Extended.LibObject, SDL.Api.Mutex, SDL.Api.Types;

type

  { TSDLCustomMutex }

  TSDLCustomMutex = class(TSDLObject)
  private
    FMutex: PSDL_mutex;
    FSDLMutexApi: TSDLMutexApi;
  protected
    procedure DoReload; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    (**
      * \brief Try to lock mutex.
      *)
    function TryLock: Boolean;
    (**
      * \brief Lock mutex
      *)
    procedure Lock;
    (**
      * \brief Unlock mutex
      *)
    procedure Unlock;

    property Mutex: PSDL_mutex read FMutex; (** Access to SDL data structure *)
  end;

  { TSDLMutex }

  TSDLMutex = class(TSDLCustomMutex)
  published
    property Provider;
  end;

implementation

{ TSDLCustomMutex }

procedure TSDLCustomMutex.DoReload;
begin
  if Assigned(Provider) and Provider.Active then
  begin
    FSDLMutexApi := TSDLMutexApi.Create(Provider.Libs['SDL2']);
    FSDLMutexApi.Init;

    if not FSDLMutexApi.Valid then
      RaiseSDLException;

    FMutex := FSDLMutexApi.SDL_CreateMutex();
  end
  else
  begin
    if Assigned(FMutex) then
      FSDLMutexApi.SDL_DestroyMutex(FMutex);
    FMutex := nil;

    FreeAndNil(FSDLMutexApi);
  end;
end;

procedure TSDLCustomMutex.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TSDLCustomMutex.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

function TSDLCustomMutex.TryLock: Boolean;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_TryLockMutex(FMutex);
    if Rez = -1 then
      RaiseSDLException
    else
      Result := Rez
  end
  else
    RaiseProviderException
end;

procedure TSDLCustomMutex.Lock;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_LockMutex(FMutex);
    if Rez < 0 then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

procedure TSDLCustomMutex.Unlock;
var
  Rez: SDL_ResultCode;
begin
  if Assigned(FSDLMutexApi) then
  begin
    Rez := FSDLMutexApi.SDL_UnlockMutex(FMutex);
    if Rez < 0 then
      RaiseSDLException
  end
  else
    RaiseProviderException
end;

end.
