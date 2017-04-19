unit SDL.Extended.AtomicLock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SDL.Extended.LibObject,
  SDL.Api.Atomic, SDL.Api.Types;

type

  { TSDLCustomAtomicLock }

  TSDLCustomAtomicLock = class(TSDLObject)
  private
    FLock: SDL_SpinLock;
    FSDLAtomicApi: TSDLAtomicApi;
  protected
    procedure DoReload; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function TryLock: Boolean;
    procedure Lock;
    procedure Unlock;
  end;

  TSDLAtomicLock = class(TSDLCustomAtomicLock)
  published
    property Provider;
  end;

implementation

{ TSDLCustomAtomicLock }

procedure TSDLCustomAtomicLock.DoReload;
begin
  if Assigned(Provider) and Provider.Active then
  begin
    FSDLAtomicApi := TSDLAtomicApi.Create(Provider.Libs['SDL2']);
    FSDLAtomicApi.Init;

    if not FSDLAtomicApi.Valid then
      RaiseSDLException;
  end
  else
  begin
    FreeAndNil(FSDLAtomicApi);
  end;
end;

procedure TSDLCustomAtomicLock.AfterConstruction;
begin
  inherited AfterConstruction;
  FLock := 0;
end;

procedure TSDLCustomAtomicLock.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

function TSDLCustomAtomicLock.TryLock: Boolean;
begin
  if Assigned(FSDLAtomicApi) then
    Result := FSDLAtomicApi.SDL_AtomicTryLock(@FLock) = SDL_TRUE
  else
    RaiseProviderException
end;

procedure TSDLCustomAtomicLock.Lock;
begin
  if Assigned(FSDLAtomicApi) then
    FSDLAtomicApi.SDL_AtomicLock(@FLock)
  else
    RaiseProviderException
end;

procedure TSDLCustomAtomicLock.Unlock;
begin
  if Assigned(FSDLAtomicApi) then
    FSDLAtomicApi.SDL_AtomicUnlock(@FLock)
  else
    RaiseProviderException
end;

end.
