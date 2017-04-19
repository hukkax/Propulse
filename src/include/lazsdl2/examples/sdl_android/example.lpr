{$IFDEF ANDROID} library {$ELSE} program {$ENDIF} example;

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
{$IFDEF ANDROID}
  jni, ctypes, SDL.Android.NativeActivity, SDL.Android.Logger,
{$ELSE}
  Event.Logger, eventlog,
{$ENDIF}
  interfaces, SDL.Init,
  SDL.Extended.Application, MainWnd, SDL.Extended.Interfaces,
  SDL.Extended.Types
  { add your units here };

type

  { TAndroidApplication }

  TAndroidApplication = class(TSDL2Application)
  protected
    procedure DoInitialize; override;
  end;

procedure TAndroidApplication.DoInitialize;
begin
  inherited DoInitialize;
  Paths.SDL2 := 'SDL2';
  Paths.SDL_Image := 'SDL2_image';
end;

{$IFDEF ANDROID}
exports
  JNI_OnLoad,
  JNI_OnUnload,
  SDL_GetEnv name 'Android_JNI_GetEnv',
  SDL_Init name 'Java_org_libsdl_app_SDLActivity_nativeInit',
  Native_Create name 'ANativeActivity_onCreate';
{$ENDIF}

{$IFDEF APP_LOGGING}
var
  Logger: ILogger;
{$ENDIF}

{$R *.res}

begin
  {$IFDEF APP_LOGGING}
  Logger := GetLogger;
  Logger.Log(logDebug, '-->AppMain');
  {$ENDIF}
  TSDL2Application.Init(TAndroidApplication);
  try
    Application.Title:='SDL Android Application';
    {$IFDEF APP_LOGGING}
    Application.Logger := Logger;
    {$ENDIF}
    {$IFNDEF ANDROID}
    Application.Initialize;
    Application.CreateWnd;
    Application.Run;
    {$ENDIF}
  finally
    {$IFNDEF ANDROID}
    Application.Free;
    {$ENDIF}
  end;
  {$IFDEF APP_LOGGING}
  Logger.Log(logDebug, '<--AppMain');
  {$ENDIF}
end.
