unit SDL.Extended.Thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SDL.Api.Types,
  SDL.Extended.LibraryProvider, SDL.Api.Thread, SDL.Extended.LibObject,
  SDL.Api.Timer;

type

  { TSDLCustomThread }

  TSDLCustomThread = class(TSDLObject)
  private
    FData: Pointer;
    FDeatached: Boolean;
    FFreeOnTerminate: Boolean;
    FOnExecute: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FPriority: SDL_ThreadPriority;
    FRunning: Boolean;
    FSDLThreadApi: TSDLThreadApi;
    FSDLTimerApi: TSDLTimerApi;
    FTerminated: Boolean;
    FThread: PSDL_Thread;
    FThreadName: String;
    function GetThreadID: SDL_ThreadID;
    procedure SetData(AValue: Pointer);
    procedure SetFreeOnTerminate(AValue: Boolean);
    procedure SetOnExecute(AValue: TNotifyEvent);
    procedure SetOnFinish(AValue: TNotifyEvent);
    procedure SetPriority(AValue: SDL_ThreadPriority);
    procedure SetThreadName(AValue: String);
    procedure InternalDetach;
  protected
    procedure Cleanup;
    (**
      * \brief Override this method to write your own custom execution code
      *)
    procedure DoExecute; virtual;
    procedure DoReload; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    (**
      * \brief Shows if thread is running
      *
      * \return If thread is running then returns true otherwise false.
      *)
    function Running: Boolean;
    (**
      * \brief Shows if thread is terminated
      *
      * \return If thread is terminated then returns true otherwise false.
      *
      * \remark Terminated threads can still be running for quite a while.
      *)
    function Terminated: Boolean;
    (**
      * \brief Wait for thread to be terminated
      *
      * This method wait for thread to stop running. If time is specified then
      *   then after time runs out process will continue if time is specified as negative
      *   function will wait indefinatly for thread to finish. If time is specified and it runs out
      *   function will return with false otherwise it will return as true. It is adviced to
      *   send message processign method witch wait can execute while waiting (see remark for more).
      *   It is not advised to use wait on threads which have enabled FreeOnTerminate.
      *
      * \param MessageProcessing Method witch to execute while waitnig
      * \param Time time to wait until exit. If negative then wait indefinatly
      *
      * \return Returns true if thread finished running while waiting otherwise returns false
      *
      * \remark For form applications it is adviced to send Application.ProcessMessages as
      *   MessageProcessing variable so that application would not hang and message processing wont stop
      *   witch would nake your application stop responding wor a time.
      *)
    function Wait(MessageProcessing: TProcedureOfObject = nil; Time: SDL_SInt32 = -1): Boolean;

    (**
      * \brief Detach thread making it like daemon.
      *
      * This function detaches thread making it deamon like. Do not use TSDLThread object after detaching.
      *   Use this if you need continuous background process.
      *   Do not try to free detached threads they will do it themselves after finishing process.
      *
      * It is not safe to call Wait for detached threads either.
      *)
    procedure Detach;
    (**
      * \brief Use this function to wait a specified number of milliseconds before returning.
      *
      * \param Interval Amount of minimum milliseconds to wait until return
      *)
    procedure Sleep(const Interval: SDL_SInt32);
    (**
      * \brief Star thread execution.
      *)
    procedure Start;
    (**
      * \brief Sets internal thread flag as terminated. User must process it themselves.
      *
      * <code>
      * procedure OnThreadExecute(Sender: TObject);
      * begin
      *   while not TSDLThread(Sender).Terminated do
      *   begin
      *     TSDLThread(Sender).Sleep(10);
      *   end;
      * end;
      * </code>
      *)
    procedure Terminate;

    property Data: Pointer read FData write SetData; (** Data witch to attach to thread and use in onExecute *)
    property FreeOnTerminate: Boolean read FFreeOnTerminate write SetFreeOnTerminate; (** Free thread after it finished execution *)
    property Priority: SDL_ThreadPriority read FPriority write SetPriority; (** Thread priority default value SDL_THREAD_PRIORITY_NORMAL*)
    property ThreadName: String read FThreadName write SetThreadName; (** Name of thread *)
    property ThreadID: SDL_ThreadID read GetThreadID; (** Thread id *)

    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute; (** Contains main actions witch must be executed *)
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish; (** Performs after OnExecution is finished *)
  end;

  TSDLThread = class(TSDLCustomThread)
  published
    property FreeOnTerminate;
    property Priority;
    property Provider;
    property ThreadName;

    property OnExecute;
    property OnFinish;
  end;

implementation

uses
  SDL.Extended.Strings;

function ThreadMethod(data: SDL_Data): SDL_SInt32 cdecl;
var
  Thread: TSDLCustomThread;
begin
  Thread := TSDLCustomThread(data);
  Thread.FSDLThreadApi.SDL_SetThreadPriority(Thread.Priority);
  Thread.FTerminated := False;
  Thread.FRunning := True;

  Thread.DoExecute;

  if not Thread.FDeatached then
      Thread.InternalDetach; //Thread is alredy finished when it will exit this method let sistem to handle it

  if Assigned(Thread.OnFinish) then
    Thread.OnFinish(Thread);

  if Thread.FreeOnTerminate or Thread.FDeatached then
    Thread.Free;

  Thread.FRunning := False;
  Result := 0;
end;

{ TSDLCustomThread }

procedure TSDLCustomThread.SetPriority(AValue: SDL_ThreadPriority);
begin
  if FPriority=AValue then Exit;
  FPriority:=AValue;
end;

procedure TSDLCustomThread.SetOnExecute(AValue: TNotifyEvent);
begin
  FOnExecute:=AValue;
end;

procedure TSDLCustomThread.SetFreeOnTerminate(AValue: Boolean);
begin
  FFreeOnTerminate:=AValue;
end;

procedure TSDLCustomThread.SetData(AValue: Pointer);
begin
  if FData=AValue then Exit;
  FData:=AValue;
end;

function TSDLCustomThread.GetThreadID: SDL_ThreadID;
begin
  if Assigned(FThread) then
    Result := FSDLThreadApi.SDL_GetThreadID(FThread)
  else
    Result := 0
end;

procedure TSDLCustomThread.SetOnFinish(AValue: TNotifyEvent);
begin
  FOnFinish:=AValue;
end;

procedure TSDLCustomThread.SetThreadName(AValue: String);
begin
  if FThreadName=AValue then Exit;
  FThreadName:=AValue;
end;

procedure TSDLCustomThread.InternalDetach;
begin
  if Assigned(FSDLThreadApi) then
  begin
    FSDLThreadApi.SDL_DetachThread(FThread);
    FThread := nil;
  end
  else
    RaiseProviderException;
end;

procedure TSDLCustomThread.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TSDLCustomThread.DoReload;
begin
  Terminate;

  if Assigned(Provider) and Provider.Active then
  begin
    FSDLThreadApi := TSDLThreadApi.Create(Provider.Libs['SDL2']);
    FSDLThreadApi.Init;

    if not FSDLThreadApi.Valid then
      RaiseLibraryException(FSDLThreadApi);

    FSDLTimerApi := TSDLTimerApi.Create(Provider.Libs['SDL2']);
    FSDLTimerApi.Init;

    if not FSDLTimerApi.Valid then
      RaiseLibraryException(FSDLTimerApi);
  end
  else
  begin
    if Assigned(FThread) then
    begin
      if not Running then
        Cleanup
      else
        Detach
    end;

    FreeAndNil(FSDLTimerApi);
    FreeAndNil(FSDLThreadApi)
  end;
end;

procedure TSDLCustomThread.Cleanup;
begin
  if not FDeatached and Assigned(FThread) then
    FSDLThreadApi.SDL_WaitThread(FThread, nil)
end;

procedure TSDLCustomThread.AfterConstruction;
begin
  FPriority := SDL_THREAD_PRIORITY_NORMAL;

  inherited AfterConstruction;
end;

procedure TSDLCustomThread.BeforeDestruction;
begin
  if Assigned(FThread) then
  begin
    Terminate;
    Detach
  end;

  inherited BeforeDestruction;
end;

procedure TSDLCustomThread.Start;
begin
  if not Assigned(Provider) then
    raise SDLException.Create(rsProviderNotSet);

  if not Provider.Active then
    raise SDLException.Create(rsProviderNotActive);

  FThread := FSDLThreadApi.SDL_CreateThreadEx(@ThreadMethod, SDL_String(FThreadName), Self);
end;

procedure TSDLCustomThread.Detach;
begin
  if Assigned(FThread) then
  begin
    if not (csDestroying in ComponentState) and Assigned(Owner) then
      Owner.RemoveComponent(Self);  //Do not free when owner is freed
    FDeatached := True;
    InternalDetach;
  end;
end;

procedure TSDLCustomThread.Sleep(const Interval: SDL_SInt32);
begin
  if Assigned(FSDLTimerApi) then
    FSDLTimerApi.SDL_Delay(Interval)
  else
    RaiseProviderException
end;

procedure TSDLCustomThread.Terminate;
begin
  FTerminated := True
end;

function TSDLCustomThread.Wait(MessageProcessing: TProcedureOfObject;
  Time: SDL_SInt32): Boolean;
var
  StartTime: QWord;
begin
  StartTime := GetTickCount64;

  while FRunning do
  begin
    if Assigned(MessageProcessing) then
      MessageProcessing() //For form applications if posible then better to call Application.ProcessMessages
    else
      FSDLTimerApi.SDL_Delay(1); //Next best option

    if Time > -1 then
    begin
      if GetTickCount64 - StartTime > Time then
        Break
    end
  end;

  Result := Running
end;

function TSDLCustomThread.Terminated: Boolean;
begin
  Result := FTerminated
end;

function TSDLCustomThread.Running: Boolean;
begin
  Result := FRunning
end;

end.
