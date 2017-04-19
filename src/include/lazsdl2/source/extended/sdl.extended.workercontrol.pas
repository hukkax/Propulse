unit SDL.Extended.WorkerControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SDL.Extended.LibObject, SDL.Api.Timer, SDL.Api.Thread, SDL.Api.Types, SDL.Api.Mutex,
  SDL.Extended.Interfaces, SDL.Api.Video;

type

  TSDLWorkerEvent = procedure(const Sender: TObject; const JobName: String; const Worker: IWorker; const Data: Pointer) of object;

  { TSDLCustomWorkerControl }

  TSDLCustomWorkerControl = class(TSDLWindowedObject, IControl)
  private
    FActive: Boolean;
    FActiveWorkers: Integer;
    FMutex: PSDL_mutex;
    FMutexApi: TSDLMutexApi;
    FOnWorkFinish: TSDLWorkerEvent;
    FOnWorkStart: TSDLWorkerEvent;
    FSDLVideoApi: TSDLVideoApi;
    FSharedContext: SDL_SInt32;
    FThread: PSDL_Thread;
    FThreadApi: TSDLThreadApi;
    FTimerApi: TSDLTimerApi;
    FWorkerCount: Integer;
    FWorkers: TStringList;
    procedure SetActive(AValue: Boolean);
    procedure SetOnWorkFinish(AValue: TSDLWorkerEvent);
    procedure SetOnWorkStart(AValue: TSDLWorkerEvent);
    procedure SetSharedContext(AValue: SDL_SInt32);
    procedure SetWorkerCount(AValue: Integer);
    function GetControlProvider: TComponent;
    function GetControlLogger: ILogger;
    function IControl.GetProvider = GetControlProvider;
    function IControl.GetLogger = GetControlLogger;
  protected
    procedure DoReload; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function GetSharedContext(out Context: SDL_GLContext): Boolean;
    function PutSharedContext(const Context: SDL_GLContext): Boolean;
    function GetSDLWindow: PSDL_Window;
    procedure AddWorker(const JobName: String; Worker: IWorker; Data: Pointer);

    property Active: Boolean read FActive write SetActive; (** Activate/deactivate worker control. When deactivateing worker control try to finish all works and deactivate all runing threads *)
    property WorkerCount: Integer read FWorkerCount write SetWorkerCount; (** Maximums alowed paralel threads count *)

    property OnWorkStart: TSDLWorkerEvent read FOnWorkStart write SetOnWorkStart; (** Worker start to process work *)
    property OnWorkFinish: TSDLWorkerEvent read FOnWorkFinish write SetOnWorkFinish; (** Worker finished processing work *)
  end;

  { TSDLWorkerControl }

  TSDLWorkerControl = class(TSDLCustomWorkerControl)
  published
    property Active;
    property Provider;
    property Window; (** Window with is used to create shared contexts *)
    property WorkerCount;

    property OnWorkStart;
    property OnWorkFinish;
  end;

implementation

uses
  SDL.Extended.Strings;

type

  PThreadData = ^TThreadData;
  TThreadData = record
    Name: String;
    Worker: IWorker;
    Data: Pointer;
    OnWorkStart: TSDLWorkerEvent;
    OnWorkFinish: TSDLWorkerEvent;
    Logger: ILogger;
    Control: TSDLCustomWorkerControl;
  end;

{ TSDLCustomWorkerControl }

procedure TSDLCustomWorkerControl.SetWorkerCount(AValue: Integer);
begin
  if FWorkerCount=AValue then Exit;
  FWorkerCount:=AValue;
end;

function TSDLCustomWorkerControl.GetControlProvider: TComponent;
begin
  Result := Provider
end;

function TSDLCustomWorkerControl.GetControlLogger: ILogger;
begin
  Result := nil
end;

procedure TSDLCustomWorkerControl.DoReload;
begin
  if Assigned(Provider) and Provider.Active then
  begin
    FTimerApi := TSDLTimerApi.Create(Provider.Libs['SDL2']);
    InitLibrary(FTimerApi);

    FThreadApi := TSDLThreadApi.Create(Provider.Libs['SDL2']);
    InitLibrary(FThreadApi);

    FMutexApi := TSDLMutexApi.Create(Provider.Libs['SDL2']);
    InitLibrary(FMutexApi);

    FSDLVideoApi := TSDLVideoApi.Create(Provider.Libs['SDL2']);
    InitLibrary(FSDLVideoApi);

    FMutex := FMutexApi.SDL_CreateMutex();
  end
  else
  if Assigned(FMutex) then
  begin
    if Active then
    begin
      FActive := False;
      FThreadApi.SDL_WaitThread(FThread, nil);
    end;

    FMutexApi.SDL_DestroyMutex(FMutex);
    FMutex := nil;

    FreeAndNil(FSDLVideoApi);
    FreeAndNil(FTimerApi);
    FreeAndNil(FThreadApi);
    FreeAndNil(FMutexApi);
  end
end;

procedure TSDLCustomWorkerControl.AfterConstruction;
begin
  FWorkerCount := 5;
  FSharedContext := 5;
  inherited AfterConstruction;

  FWorkers := TStringList.Create;
end;

procedure TSDLCustomWorkerControl.BeforeDestruction;
begin
  FWorkers.Free;

  inherited BeforeDestruction;
end;

procedure TSDLCustomWorkerControl.AddWorker(const JobName: String; Worker: IWorker; Data: Pointer);
var
  NewWorker: PThreadData;
begin
  New(NewWorker);
  NewWorker^.Worker := Worker;
  NewWorker^.Data := Data;

  if FActive then
  begin
    FMutexApi.SDL_LockMutex(FMutex);
    try
      FWorkers.AddObject(JobName, TObject(NewWorker))
    finally
      FMutexApi.SDL_UnlockMutex(FMutex)
    end
  end
  else
    FWorkers.AddObject(JobName, TObject(NewWorker))
end;

function TSDLCustomWorkerControl.GetSharedContext(out Context: SDL_GLContext): Boolean;
begin
  Result := Window.GetSharedContext(Context)
end;

function TSDLCustomWorkerControl.PutSharedContext(const Context: SDL_GLContext): Boolean;
begin
  Result := Window.PutSharedContext(Context)
end;

function TSDLCustomWorkerControl.GetSDLWindow: PSDL_Window;
begin
  if Assigned(Window) then
    Result := Window.Wnd
  else
    RaiseWindowException
end;

procedure TSDLCustomWorkerControl.SetOnWorkStart(AValue: TSDLWorkerEvent);
begin
  FOnWorkStart:=AValue;
end;

procedure TSDLCustomWorkerControl.SetSharedContext(AValue: SDL_SInt32);
begin
  if FSharedContext=AValue then
    Exit;

  if not (csDesigning in ComponentState) and Active then
    raise SDLException.Create(rsWorkerControlIsActive);

  FSharedContext:=AValue;
end;

procedure TSDLCustomWorkerControl.SetOnWorkFinish(AValue: TSDLWorkerEvent);
begin
  FOnWorkFinish:=AValue;
end;

function WorkerThread(data: SDL_Data): SDL_SInt32; cdecl;
var
  ThreadData: PThreadData absolute data;
begin
  with ThreadData^ do
  begin
    if Assigned(OnWorkStart) then
      OnWorkStart(Control, Name, Worker, Data);

    Worker.Execute(Control, Data);

    if Assigned(OnWorkFinish) then
      OnWorkFinish(Control, Name, Worker, Data);

    Freemem(Data);
  end;

  Dispose(ThreadData);
  Result := 0;
end;

function ProcessThread(data: SDL_Data): SDL_SInt32; cdecl;
var
  Control: TSDLCustomWorkerControl;
  ThreadData: PThreadData;
begin
  Control := TSDLCustomWorkerControl(data);

  while Control.Active do
  begin
    while (Control.FWorkers.Count > 0) and (Control.FActiveWorkers < Control.WorkerCount) do
    begin
      Control.FMutexApi.SDL_LockMutex(Control.FMutex);
      try
        ThreadData := Pointer(Control.FWorkers.Objects[0]);
        ThreadData^.Name := Control.FWorkers[0];
        ThreadData^.OnWorkStart := Control.OnWorkStart;
        ThreadData^.OnWorkFinish := Control.OnWorkFinish;
        ThreadData^.Logger := nil;
        ThreadData^.Control := Control;
        Control.FWorkers.Delete(0);
        Control.FThreadApi.SDL_CreateThreadEx(@WorkerThread, nil, ThreadData);
      finally
        Control.FMutexApi.SDL_UnlockMutex(Control.FMutex);
      end
    end;

    Control.FTimerApi.SDL_Delay(1);
  end;

  Result := 0;
end;

procedure TSDLCustomWorkerControl.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;

  if not (csDesigning in ComponentState) then
  begin
    if not Assigned(Window) then
      RaiseWindowException;

    if not Assigned(Provider) or not Provider.Active then
      RaiseProviderException;

    if AValue then
      FThread := FThreadApi.SDL_CreateThreadEx(@ProcessThread, nil, Self)
    else
      FThreadApi.SDL_WaitThread(FThread, nil);
  end;
end;

end.
