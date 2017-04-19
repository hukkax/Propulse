unit SDL.Extended.Application;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF ANDROID}
  jni, SDL.Android.NativeActivity,
  {$ENDIF}
  Classes, SysUtils, CustApp, SDL.Api.libSDL2, SDL.Api.libSDL_Image,
  SDL.Api.Video, fgl, SDL.Extended.Mutex, SDL.Api.OpenGL, SDL.Extended.Interfaces,
  SDL.Extended.Types;

type

  { TSDL2Application }

  TSDL2Application = class(TCustomApplication)
  private type

    TSDLWinReg = class
      WndClass: TComponentClass;
      AutoCreate: Boolean;
    end;

    { TSDLPaths }

    TSDLPaths = class(TPersistent)
    private
      FSDL2: String;
      FSDL_Image: String;
    published
      property SDL2: String read FSDL2 write FSDL2;
      property SDL_Image: String read FSDL_Image write FSDL_Image;
    end;

    TSDLRegWindows = specialize TFPGObjectList<TSDLWinReg>;
    TSDLReleaseList = specialize TFPGObjectList<TObject>;
    TSDL2ApplicationClass = class of TSDL2Application;
  private
    class var FApplication: TSDL2Application;
    class var FRegWnd: TSDLRegWindows;
    class constructor _Create;
    class destructor _Destroy;
  private
    FLogger: ILogger;
    FMainWnd: TComponent;
    FMutex: TSDLMutex;
    FOpenGL: TOpenGLLibrary;
    FPaths: TSDLPaths;
    FReleaseList: TSDLReleaseList;
    FSDL2: TSDL2Library;
    FSDL_Image: TSDL2_ImageLibrary;
    {$IFDEF ANDROID}
    FSDL_Activity: PANativeActivity;
    FSDL_env: PJNIEnv;
    FSDL_clazz: Pointer;
    {$ENDIF}
    procedure SetLogger(AValue: ILogger);
  protected
    procedure AfterInitialization; virtual;
    procedure DoInitialize; virtual;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure DoRun; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure CreateWnd;
    procedure HandleException(Sender: TObject); override;
    procedure RegisterWindow(WndClass: TComponentClass; AutoCreate: Boolean);
    procedure ReleaseComponent(ACmp: TObject);
    procedure Lock;
    procedure Unlock;
    procedure Initialize; override; final;
    class procedure Init(const AppClass: TSDL2ApplicationClass);

    property Paths: TSDLPaths read FPaths;
    property SDL2Lib: TSDL2Library read FSDL2;
    property SDL_Image: TSDL2_ImageLibrary read FSDL_Image;
    property OpenGL: TOpenGLLibrary read FOpenGL;
    property Logger: ILogger read FLogger write SetLogger;
    {$IFDEF ANDROID}
    property SDL_env: PJNIEnv read FSDL_env write FSDL_env;
    property SDL_clazz: Pointer read FSDL_clazz write FSDL_clazz;
    property SDL_Activity: PANativeActivity read FSDL_Activity write FSDL_Activity;
    {$ENDIF}
  end;

function Application: TSDL2Application;

implementation

uses
  SDL.Api.Types, SDL.Extended.Window, LMessages, SDL.Extended.LibObject, SDL.Extended.LibraryProvider,
  DLibrary;

function Application: TSDL2Application;
begin
  Result := TSDL2Application.FApplication
end;

{ TSDL2Application }

class constructor TSDL2Application._Create;
begin
  FRegWnd := TSDLRegWindows.Create(True);
end;

class destructor TSDL2Application._Destroy;
begin
  FRegWnd.Free;
end;

procedure TSDL2Application.SetLogger(AValue: ILogger);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

procedure TSDL2Application.CreateWnd;
var
  i: Integer;
begin
  if Assigned(FMainWnd) then
    FMainWnd.Free;

  FMainWnd := nil;
  Log(etDebug, 'Find window class');
  for i := 0 to FRegWnd.Count - 1 do
  begin
    if FRegWnd[i].AutoCreate then
    begin
      if not Assigned(FMainWnd) then
      begin
        FMainWnd := TSDLWindowClass(FRegWnd[i].WndClass).Create(FSDL2, Self);
        Log(etDebug, Format('Main window created (%s)', [FMainWnd.ClassName]));
      end
      else
        TSDLWindowClass(FRegWnd[i].WndClass).Create(FSDL2, Self)
    end
  end;
end;

procedure TSDL2Application.DoLog(EventType: TEventType; const Msg: String);
begin
  inherited DoLog(EventType, Msg);

  if Assigned(FLogger) then
  begin
    case EventType of
      etInfo: FLogger.Log(logInfo, Msg);
      etWarning: FLogger.Log(logWarning, Msg);
      etError: FLogger.Log(logError, Msg);
      etDebug: FLogger.Log(logDebug, Msg);
    else
      FLogger.Log(logVerbose, Msg);
    end
  end
end;

procedure TSDL2Application.DoInitialize;
begin

end;

procedure TSDL2Application.AfterInitialization;
var
  Ver: SDL_Version;
begin
  Log(etDebug, '-->Application.AfterInitialization');

  {$IFDEF ANDROID}
  //SDL_env^^.GetObjectClass(getContext().getApplicationInfo().nativeLibraryDir;);
  //TLibraryLoader.SearchPaths.Add('');
  {$ENDIF}

  FSDL2 := TSDL2Library.Create(FPaths.SDL2);
  FSDL2.Logger := TSDLLibProxyLogger.Create(Logger);
  Log(etInfo, 'SDL2: Initialiazing');
  FSDL2.Init;

  if not FSDL2.Valid then
  begin
    Log(etError, 'Exception: ' + FSDL2.LastError);
    raise SDLCriticalException.Create(FSDL2.LastError);
  end;
  Log(etInfo, 'SDL2: Initialized(' + IntToStr(FSDL2.Handle) + ')');

  FSDL2.Version.SDL_GetVersion(Ver);
  Log(etInfo, Format('SDL Version: %d.%d.%d', [Ver.major, Ver.minor, Ver.patch]));

  FSDL2.Video.SDL_GL_LoadLibrary(nil);

  FOpenGL := TOpenGLLibrary.Create(FSDL2);
  FOpenGL.Logger := TSDLLibProxyLogger.Create(Logger);

  Log(etInfo, 'SDL_Image: Loading');
  FSDL_Image := TSDL2_ImageLibrary.Create(FPaths.SDL_Image, FSDL2);
  FSDL_Image.Logger := TSDLLibProxyLogger.Create(Logger);
  Log(etInfo, 'SDL_Image: Initialiazing');
  FSDL_Image.Init;

  if not FSDL_Image.Valid then
  begin
    Log(etError, 'SDL_Image: Failed');
    FreeAndNil(FSDL_Image); //SDL_Image is not mandatory library
  end
  else
    Log(etInfo, 'SDL_Image: Initialized');

{$IFDEF ANDROID}
  Log(etDebug, '-->SDL_SetMainReady()');
  FSDL2.Main.SDL_SetMainReady();
  Log(etDebug, '<--SDL_SetMainReady()');

  Log(etDebug, '-->SDL_Android_Init()');
  FSDL2.SDL_Android_Init(SDL_env, SDL_clazz);
  Log(etDebug, '<--SDL_Android_Init()');
{$ENDIF}

  Log(etDebug, '-->SDL_Init()');
  if not FSDL2.SDL_Init(SDL_INIT_EVERYTHING) then
    Log(etError, FSDL2.LastError);
  Log(etDebug, '<--SDL_Init()');

  Log(etDebug, '<--Application.AfterInitialization');
end;

procedure TSDL2Application.DoRun;
var
  Event: SDL_Event;
  Wnd: PSDL_Window;
  Data: SDL_Data;
begin
  inherited DoRun;

  CreateWnd;

  FMutex := TSDLMutex.Create(Self);
  try
    FMutex.Provider := TSDLLibraryProvider.Create(FMutex);
    FMutex.Provider.Name := 'Mutex_Internal_provider';
    FMutex.Provider.Libs['SDL2'] := SDL2Lib;
    FMutex.Provider.Libs['SDL_Image'] := SDL_Image;
    FMutex.Provider.Libs['OpenGL'] := OpenGL;
    FMutex.Provider.Active := True;

    Log(etInfo, 'Starting main loop');
    while not Terminated do
    begin
      if FSDL2.Events.SDL_PollEvent(@Event) = SDL_TRUE then
      begin
        Data := nil;
        if Event._type = SDL_USEREVENT_EV then
        begin
          Event._type := SDL_EventType(Integer(Event._type) + LM_USER);
          Wnd := SDL2Lib.Video.SDL_GetWindowFromID(Event.user.windowID);
          Data := SDL2Lib.Video.SDL_GetWindowData(Wnd, 'window')
        end
        else
        if (Event._type = SDL_WINDOWEVENT_EV) and (Event.window.event = SDL_WINDOWEVENT_CLOSE) then
        begin
          Event._type := SDL_EventType(Integer(Event._type) + LM_USER);
          Wnd := SDL2Lib.Video.SDL_GetWindowFromID(Event.window.windowID);
          Data := SDL2Lib.Video.SDL_GetWindowData(Wnd, 'window');
        end;

        if Assigned(Data) then
        begin
          FMutex.Lock;
          try
            TSDLWindow(Data).Dispatch(Event)
          finally
            FMutex.Unlock
          end
        end
      end;

      while FReleaseList.Count > 0 do
      begin
        if (FReleaseList[0] = FMainWnd) then
          Terminate;

        FReleaseList.Delete(0)
      end;

      FSDL2.Timer.SDL_Delay(1);
    end;

    Log(etInfo, 'Main loop finished')
  finally
    FMutex.Free;
    FSDL2.SDL_Quit()
  end
end;

procedure TSDL2Application.AfterConstruction;
begin
  inherited AfterConstruction;
  FReleaseList := TSDLReleaseList.Create(True);
  FPaths := TSDLPaths.Create;
end;

procedure TSDL2Application.BeforeDestruction;
begin
  FReleaseList.Free;
  FPaths.Free;

  FSDL_Image.Free;
  FSDL2.Free;

  inherited BeforeDestruction;
end;

procedure TSDL2Application.HandleException(Sender: TObject);
begin
  if ExceptObject is SDLCriticalException then
  begin
    Log(etError, SDLCriticalException(ExceptObject).Message);
    Terminate
  end
  else
    inherited HandleException(Sender)
end;

procedure TSDL2Application.RegisterWindow(WndClass: TComponentClass;
  AutoCreate: Boolean);
var
  New: TSDLWinReg;
begin
  New := TSDLWinReg.Create;
  New.WndClass := WndClass;
  New.AutoCreate := AutoCreate;
  FRegWnd.Add(New)
end;

procedure TSDL2Application.ReleaseComponent(ACmp: TObject);
begin
  FMutex.Lock;
  try
    if FReleaseList.IndexOf(ACmp) = -1 then
      FReleaseList.Add(ACmp)
  finally
    FMutex.Unlock
  end;
end;

procedure TSDL2Application.Lock;
begin
  FMutex.Lock;
end;

procedure TSDL2Application.Unlock;
begin
  FMutex.Unlock;
end;

procedure TSDL2Application.Initialize;
begin
  Log(etInfo, '-->Application.Initialize');
  inherited Initialize;
  DoInitialize;
  AfterInitialization;
  Log(etInfo, '<--Application.Initialize');
end;

class procedure TSDL2Application.Init(const AppClass: TSDL2ApplicationClass);
begin
  if not Assigned(FApplication) then
    FApplication := AppClass.Create(nil)
  else
  if FApplication.ClassName <> AppClass.ClassName then
    raise SDLException.Create('Application alredy assigned')
end;

end.

