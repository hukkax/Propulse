unit SDL.Init;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF ANDROID}
    jni, SDL.Android.Logger, SDL.Android.NativeActivity,
  {$ELSE}
    Event.Logger, eventlog,
  {$ENDIF}
  {$IFDEF ANDROID}
  Classes,
  {$ENDIF}
  SDL.Extended.Interfaces, SDL.Extended.Application, DLibrary, FileUtil, LazFileUtils, sysutils;

{$IFDEF ANDROID}
function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; cdecl;
function JNI_OnUnload(vm: PJavaVM; reserved: pointer): jint; cdecl;
function SDL_GetEnv(): PJNIEnv; cdecl;
procedure SDL_Init(env: PJNIEnv; cls: jclass; obj: jobject); cdecl;
procedure Native_Create(activity: PANativeActivity; savedState: Pointer; savedStateSize: DWord);
{$ENDIF}

function GetLogger: ILogger;

implementation

function GetLogger: ILogger;
var
  {$IFDEF ANDROID}
  AndroidLogger: TAndroidLogger;
  {$ELSE}
  Events: TEventLog;
  {$ENDIF}
begin
  {$IFDEF ANDROID}
  AndroidLogger := TAndroidLogger.Create;
  AndroidLogger.Tag := 'ANDROID';
  Result := AndroidLogger;
  {$ELSE}
  Events := TEventLog.Create(nil);
  Events.LogType := ltFile;
  Events.FileName := 'log.txt';
  Events.Active := True;
  Result := TEventLogger.Create(Events, True);
  {$ENDIF}
end;

{$IFDEF ANDROID}
function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; cdecl;
begin
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '-->JNI_OnLoad');
  {$ENDIF}
  Result := JNI_VERSION_1_6;
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '<--JNI_OnLoad(Ok)');
  {$ENDIF}
end;

function JNI_OnUnload(vm: PJavaVM; reserved: pointer): jint; cdecl;
begin
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '-->JNI_OnUnload');
  {$ENDIF}
  Result := 0;
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '<--JNI_OnUnload(Ok)');
  {$ENDIF}
end;

function SDL_GetEnv(): PJNIEnv; cdecl;
begin
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '-->SDL_GetEnv');
  {$ENDIF}
  Result := Application.SDL_env;
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '<--SDL_GetEnv(Ok)');
  {$ENDIF}
end;

procedure SDL_Init(env: PJNIEnv; cls: jclass; obj: jobject); cdecl;
begin
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '-->SDL_Init()');
  {$ENDIF}

  Application.SDL_env := env;
  Application.SDL_clazz := cls;
  Application.Initialize;
  Application.Run;

  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '<--SDL_Init()');
  {$ENDIF}
end;

procedure Native_Create(activity: PANativeActivity; savedState: Pointer;
  savedStateSize: DWord);
var
  LibPath: String;
  Files: TStringList;
  i: Integer;
begin
  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '-->Native_Create()');
  {$ENDIF}

  LibPath := AppendPathDelim(ExtractFilePath(activity^.internalDataPath)) + 'lib';
  TLibraryLoader.SearchPaths.Add(LibPath);

  Application.SDL_Activity := activity;
  Application.Log(etInfo, 'Internal data path: ' + activity^.internalDataPath);
  Application.Log(etInfo, 'External data path: ' + activity^.externalDataPath);
  Application.Log(etInfo, 'Native library path: ' + LibPath);

  Files := FindAllFiles(LibPath, '*.*', True);
  try
    for i := 0 to Files.Count - 1 do
      Application.Log(etInfo, Files[i]);
  finally
    Files.Free;
  end;

  {$IFDEF APP_LOGGING}
  Application.Log(etDebug, '<--Native_Create()');
  {$ENDIF}
end;

{$ENDIF}

end.

