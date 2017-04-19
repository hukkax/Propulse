unit mainfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SDL.Extended.Window,
  SDL.Extended.Thread, SDL.Extended.LibraryProvider, SDL.Extended.Mutex,
  SDL.Extended.Semaphore, SDL.Extended.AtomicLock;

type

  { TSDLWindow1 }

  TSDLWindow1 = class(TSDLWindow)
    atm_01: TSDLAtomicLock;
    sdlLib: TSDLLibraryProvider;
    mtx_01: TSDLMutex;
    thr_resources: TSDLThread;
    sem_01: TSDLSemaphore;
    thr_01: TSDLThread;
    thr_02: TSDLThread;
    thr_03: TSDLThread;
    thr_04: TSDLThread;
    thr_05: TSDLThread;
    thr_06: TSDLThread;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure thr_04Execute(Sender: TObject);
    procedure thr_04Finish(Sender: TObject);
    procedure thr_resourcesExecute(Sender: TObject);
  private
    FResources: TStringList;
    MaxSleep: Integer;
    MinSleep: Integer;
    FTotalThreds: Integer;
  public
    procedure WriteLog(const Msg: String);
  end;

implementation

uses
  SDL.Extended.Application;

{$R *.lfm}

{ TSDLWindow1 }

procedure TSDLWindow1.thr_04Execute(Sender: TObject);
begin
  InterLockedIncrement(FTotalThreds);
  with Sender as TSDLThread do
  begin
    WriteLog('Thread started: ' + Name);

    while not Terminated do
    begin
      //Try to take one resource
      if sem_01.Take(10) then
      begin
        mtx_01.Lock;
        try
          //Process resource
          WriteLog(Name + ': ' + FResources[0]);
          FResources.Delete(0);
        finally
          mtx_01.Unlock;
        end;
      end;

      //Do noething
      Sleep(MinSleep + Random(MaxSleep - MinSleep));
    end;

    WriteLog('Thread finished: ' + Name)
  end
end;

procedure TSDLWindow1.thr_04Finish(Sender: TObject);
begin
  InterLockedDecrement(FTotalThreds);
end;

procedure TSDLWindow1.thr_resourcesExecute(Sender: TObject);
var
  Guid: TGuid;
begin
  InterLockedIncrement(FTotalThreds);
  while not TSDLThread(Sender).Terminated do
  begin
    mtx_01.Lock;
    try
      //Create new resource
      CreateGUID(Guid);
      FResources.Add(GUIDToString(Guid));
      //Tels semaphore that one resource created
      sem_01.Give;
    finally
      mtx_01.Unlock;
    end;

    TSDLThread(Sender).Sleep(10);
  end;
end;

procedure TSDLWindow1.DataModuleCreate(Sender: TObject);
begin
  sdlLib.Libs['SDL2'] := Application.SDL2Lib;
  sdlLib.Active := True;

  FResources := TStringList.Create;
  MinSleep := 0;
  MaxSleep := 100;

  //This thread will generate resources
  thr_resources.Start;

  //These 6 threds will try to process resources which thr_resources will generate
  thr_01.Start;
  thr_02.Start;
  thr_03.Start;
  thr_04.Start;
  thr_05.Start;
  thr_06.Start;
end;

procedure TSDLWindow1.DataModuleDestroy(Sender: TObject);
begin
  thr_resources.Terminate;
  thr_01.Terminate;
  thr_02.Terminate;
  thr_03.Terminate;
  thr_04.Terminate;
  thr_05.Terminate;
  thr_06.Terminate;

  thr_01.Wait;
  thr_02.Wait;
  thr_03.Wait;
  thr_04.Wait;
  thr_05.Wait;
  thr_06.Wait;
  thr_resources.Wait;

  FResources.Free;
end;

procedure TSDLWindow1.WriteLog(const Msg: String);
begin
  atm_01.Lock;
  try
    WriteLn(Msg);
  finally
    atm_01.Unlock
  end;
end;


initialization
  Application.RegisterWindow(TSDLWindow1, True);
end.

