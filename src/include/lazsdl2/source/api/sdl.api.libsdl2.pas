(**
 *  \file SDL.h
 *
 *  Main include header for the SDL library
 *)
unit SDL.Api.libSDL2;

{$mode objfpc}{$H+}

interface

(**
 *  \mainpage Simple DirectMedia Layer (SDL)
 *
 *  http://www.libsdl.org/
 *
 *  \section intro_sec Introduction
 *
 *  Simple DirectMedia Layer is a cross-platform development library designed
 *  to provide low level access to audio, keyboard, mouse, joystick, and
 *  graphics hardware via OpenGL and Direct3D. It is used by video playback
 *  software, emulators, and popular games including Valve's award winning
 *  catalog and many Humble Bundle games.
 *
 *  SDL officially supports Windows, Mac OS X, Linux, iOS, and Android.
 *  Support for other platforms may be found in the source code.
 *
 *  SDL is written in C, works natively with C++, and there are bindings
 *  available for several other languages, including C# and Python.
 *
 *  This library is distributed under the zlib license, which can be found
 *  in the file "COPYING.txt".
 *
 *  The best way to learn how to use SDL is to check out the header files in
 *  the "include" subdirectory and the programs in the "test" subdirectory.
 *  The header files and test programs are well commented and always up to date.
 *  More documentation and FAQs are available online at:
 *      http://wiki.libsdl.org/
 *
 *  If you need help with the library, or just want to discuss SDL related
 *  issues, you can join the developers mailing list:
 *      http://www.libsdl.org/mailing-list.php
 *
 *  Enjoy!
 *      Sam Lantinga                (slouken@libsdl.org)
 *)

uses
  Classes,
  DLibrary,
  SDL.Api.Consts,
  SDL.Api.Assert,
  SDL.Api.Atomic,
  SDL.Api.Audio,
  SDL.Api.Clipboard,
  SDL.APi.CPUInfo,
  SDL.Api.Error,
  SDL.Api.Events,
  SDL.APi.FileSystem,
  SDL.Api.GameController,
  SDL.Api.Gesture,
  SDL.Api.Haptic,
  SDL.Api.Hints,
  {$IFNDEF ANDROID}
  SDL.Api.JoyStick,
  {$ENDIF}
  SDL.Api.Keyboard,
  SDL.Api.LoadSO,
  SDL.Api.Log,
  SDL.Api.Main,
  SDL.Api.MessageBox,
  SDL.Api.Mouse,
  SDL.Api.Mutex,
  SDL.Api.Pixels,
  SDL.Api.Platform,
  SDL.Api.Power,
  SDL.Api.Rect,
  SDL.Api.Render,
  SDL.Api.Rwops,
  SDL.Api.StdInc,
  SDL.Api.Surface,
  SDL.Api.Thread,
  SDL.Api.Timer,
  SDL.Api.Touch,
  SDL.Api.Types,
  SDL.Api.Version,
  SDL.Api.Video,
  SysUtils;

type

  { TSDL2Library }

  TSDL2Library = class(TLibrary)
  private type
    TSDL_Init = function(flags: SDL_InitFlags): SDL_ResultCode cdecl;
    TSDL_InitSubSystem = function(flags: SDL_InitFlags): SDL_ResultCode cdecl;
    TSDL_QuitSubSystem = procedure(flags: SDL_InitFlags) cdecl;
    TSDL_WasInit = function(flags: SDL_InitFlags): SDL_InitFlags cdecl;
    TSDL_Quit = procedure() cdecl;
    {$IFDEF ANDROID}
    TSDL_Android_Init = procedure(env : Pointer; clazz: Pointer); cdecl;
    {$ENDIF}
  public
    (**
     *  This function initializes  the subsystems specified by \c flags
     *)
    SDL_Init: TSDL_Init;
    (**
     *  This function initializes specific SDL subsystems
     *
     *  Subsystem initialization is ref-counted, you must call
     *  SDL_QuitSubSystem for each SDL_InitSubSystem to correctly
     *  shutdown a subsystem manually (or call SDL_Quit to force shutdown).
     *  If a subsystem is already loaded then this call will
     *  increase the ref-count and return.
     *)
    SDL_InitSubSystem: TSDL_InitSubSystem;
    (**
     *  This function cleans up specific SDL subsystems
     *)
    SDL_QuitSubSystem: TSDL_QuitSubSystem;
    (**
     *  This function returns a mask of the specified subsystems which have
     *  previously been initialized.
     *
     *  If \c flags is 0, it returns a mask of all initialized subsystems.
     *)
    SDL_WasInit: TSDL_WasInit;
    (**
     *  This function cleans up all initialized subsystems. You should
     *  call it upon all exit conditions.
     *)
    SDL_Quit: TSDL_Quit;
    {$IFDEF ANDROID}
    SDL_Android_Init: TSDL_Android_Init;
    {$ENDIF}
  private
    FAssert: TSDLAssertApi;
    FAtomic: TSDLAtomicApi;
    FAudio: TSDLAudioApi;
    FClipboard: TSDLClipboardApi;
    FCPUInfo: TSDLCPUInfoApi;
    FError: TSDLErrorApi;
    FEvents: TSDLEventsApi;
    FFileSystem: TSDLFileSystemApi;
    FGameControler: TSDLGameControllerApi;
    FGesture: TSDLGestureApi;
    FHaptic: TSDLHapticApi;
    FHints: TSDLHintsApi;
    {$IFNDEF ANDROID}
    FJoyStick: TSDLJoyStickApi;
    {$ENDIF}
    FKeyboard: TSDLKeyboardApi;
    FLoadSO: TSDLLoadSOApi;
    FLog: TSDLLogApi;
    FMain: TSDLMainApi;
    FMessageBox: TSDLMessageBoxApi;
    FMouse: TSDLMouseApi;
    FMutex: TSDLMutexApi;
    FPixels: TSDLPixelsApi;
    FPlatform: TSDLPlatformApi;
    FPower: TSDLPowerApi;
    FRect: TSDLRectApi;
    FRender: TSDLRenderApi;
    FRWops: TSDLRwopsApi;
    FStdInc: TSDLStrIncApi;
    FSurface: TSDLSurfaceApi;
    FThreads: TSDLThreadApi;
    FTimer: TSDLTimerApi;
    FTouch: TSDLTouchApi;
    FVersion: TSDLVersionApi;
    FVideo: TSDLVideoApi;
  protected
    procedure DoGetFileVersion(out VersionStr: String); override;
    procedure DoGetLastError(out ErrorStr: String); override;
    procedure DoInit; override;
    procedure DoSetLastError(const ErrorStr: String); override;
    procedure GetRequiredMethods(const List: TMethodList); override;
    procedure InitApi(ApiClass: TAbstractLibraryClass; out Reference);
  public
    constructor Create(const LibraryFile: String = ''); override;
    procedure BeforeDestruction; override;

    function GetLibProcAddress(const ProcName : AnsiString): Pointer; override;

    property Assert: TSDLAssertApi read FAssert; (** Header file redefinition for SDL_Assert.h *)
    property Atomic: TSDLAtomicApi read FAtomic; (** Header file redefinition for SDL_Atomic.h *)
    property Audio: TSDLAudioApi read FAudio; (** Header file redefinition for SDL_Audio.h *)
    property Clipboard: TSDLClipboardApi read FClipboard; (** Header file redefinition for SDL_Clipboard.h *)
    property CPUInfo: TSDLCPUInfoApi read FCPUInfo; (** Header file redefinition for SDL_CPUInfo.h *)
    property Error: TSDLErrorApi read FError; (** Header file redefinition for SDL_Error.h *)
    property Events: TSDLEventsApi read FEvents; (** Header file redefinition for SDL_Events.h *)
    property FileSystem: TSDLFileSystemApi read FFileSystem; (** Header file redefinition for SDL_FileSystem.h *)
    property GameControler: TSDLGameControllerApi read FGameControler; (** Header file redefinition for SDL_GameControler.h *)
    property Gesture: TSDLGestureApi read FGesture; (** Header file redefinition for SDL_Gesture.h *)
    property Haptic: TSDLHapticApi read FHaptic; (** Header file redefinition for SDL_Haptic.h *)
    property Hints: TSDLHintsApi read FHints; (** Header file redefinition for SDL_Hints.h *)
    {$IFNDEF ANDROID}
    property JoyStick: TSDLJoyStickApi read FJoyStick; (** Header file redefinition for SDL_JoyStick.h *)
    {$ENDIF}
    property Keyboard: TSDLKeyboardApi read FKeyboard; (** Header file redefinition for SDL_Keyboard.h *)
    property LoadSO: TSDLLoadSOApi read FLoadSO; (** Header file redefinition for SDL_LoadSO.h *)
    property Log: TSDLLogApi read FLog; (** Header file redefinition for SDL_Log.h *)
    property Main: TSDLMainApi read FMain; (** Header file redefinition for SDL_Main.h *)
    property MessageBox: TSDLMessageBoxApi read FMessageBox; (** Header file redefinition for SDL_MessageBox.h *)
    property Mouse: TSDLMouseApi read FMouse; (** Header file redefinition for SDL_Mouse.h *)
    property Mutex: TSDLMutexApi read FMutex; (** Header file redefinition for SDL_Mutex.h *)
    property Pixels: TSDLPixelsApi read FPixels; (** Header file redefinition for SDL_Pixels.h *)
    property Platform: TSDLPlatformApi read FPlatform; (** Header file redefinition for SDL_Platform.h *)
    property Power: TSDLPowerApi read FPower; (** Header file redefinition for SDL_Power.h *)
    property Rect: TSDLRectApi read FRect; (** Header file redefinition for SDL_Rect.h *)
    property Render: TSDLRenderApi read FRender; (** Header file redefinition for SDL_Render.h *)
    property RWops: TSDLRwopsApi read FRWops; (** Header file redefinition for SDL_Rwops.h *)
    property StdInc: TSDLStrIncApi read FStdInc; (** Header file redefinition for SDL_StdInc.h *)
    property Surface: TSDLSurfaceApi read FSurface; (** Header file redefinition for SDL_Surface.h *)
    property Threads: TSDLThreadApi read FThreads; (** Header file redefinition for SDL_Threds.h *)
    property Timer: TSDLTimerApi read FTimer; (** Header file redefinition for SDL_Timer.h *)
    property Touch: TSDLTouchApi read FTouch; (** Header file redefinition for SDL_Touch.h *)
    property Version: TSDLVersionApi read FVersion; (** Header file redefinition for SDL_Version.h *)
    property Video: TSDLVideoApi read FVideo; (** Header file redefinition for SDL_Video.h *)
  end;

implementation

uses
  LazFileUtils;

{ TSDL2Library }

procedure TSDL2Library.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  Logger.Log(lgDebug, 'GetRequiredMethods');

  List.Add('SDL_Init', @SDL_Init);
  List.Add('SDL_InitSubSystem', @SDL_InitSubSystem);
  List.Add('SDL_QuitSubSystem', @SDL_QuitSubSystem);
  List.Add('SDL_WasInit', @SDL_WasInit);
  List.Add('SDL_Quit', @SDL_Quit);
  {$IFDEF ANDROID}
  List.Add('SDL_Android_Init', @SDL_Android_Init);
  {$ENDIF}
end;

procedure TSDL2Library.InitApi(ApiClass: TAbstractLibraryClass; out Reference);
var
  Lib: TAbstractLibrary;
begin
  if ApiClass.InheritsFrom(TSubLibrary) then
    Lib := TSubLibraryClass(ApiClass).Create(Self)
  else
    Lib := ApiClass.Create;

  Lib.Logger := Logger;
  Lib.Init;

  if not Lib.Valid then
  begin
    Invalid;
    Logger.Log(lgError, ApiClass.ClassName + ': Initialization failure');
    Logger.Log(lgError, ApiClass.ClassName + ': ' + Lib.LastError);
    LastError := LastError + Lib.LastError + LineEnding;
  end
  else
    Logger.Log(lgVerbose, ApiClass.ClassName + ': Initialization success');

  TAbstractLibrary(Reference) := Lib;
end;

constructor TSDL2Library.Create(const LibraryFile: String);
var
  RezFileName: String;
  LibDir: String;
begin
  if LibraryFile = '' then
  begin
    {$IFDEF WINDOWS}
      RezFileName := 'SDL2.dll';
    {$ELSE}
      {$IFDEF MACOS}
        RezFileName := 'SDL2';
        {$linklib libSDL2}
      {$ELSE}
        {$IFDEF UNIX}
          {$IFDEF DARWIN}
            RezFileName := 'libSDL2.dylib';
          {$ELSE}
            {$IFDEF ANDROID}
            RezFileName := 'libSDL2.so';
            {$ELSE}
            RezFileName := 'libSDL2-2.0.so.0';
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}

    LibDir := AppendPathDelim(ExtractFilePath(ParamStr(0))); // + 'lib' + PathDelim + SDLConsts.CPU + '-' + SDLConsts.OS;
    if DirectoryExists(LibDir) then
      RezFileName := AppendPathDelim(LibDir) + RezFileName;
  end
  else
    RezFileName := LibraryFile;

  inherited Create(RezFileName);
end;

procedure TSDL2Library.BeforeDestruction;
begin
  FAssert.Free;
  FAtomic.Free;
  FAudio.Free;
  FClipboard.Free;
  FCPUInfo.Free;
  FError.Free;
  FEvents.Free;
  FFileSystem.Free;
  FGameControler.Free;
  FGesture.Free;
  FHaptic.Free;
  FHints.Free;
  {$IFNDEF ANDROID}
  FJoyStick.Free;
  {$ENDIF}
  FKeyboard.Free;
  FLoadSO.Free;
  FLog.Free;
  FMain.Free;
  FMessageBox.Free;
  FMouse.Free;
  FMutex.Free;
  FPixels.Free;
  FPlatform.Free;
  FPower.Free;
  FRect.Free;
  FRender.Free;
  FRWops.Free;
  FStdInc.Free;
  FSurface.Free;
  FThreads.Free;
  FTimer.Free;
  FTouch.Free;
  FVersion.Free;
  FVideo.Free;

  inherited BeforeDestruction;
end;

function TSDL2Library.GetLibProcAddress(const ProcName: AnsiString): Pointer;
begin
  Result := inherited GetLibProcAddress(ProcName);

  if not Assigned(Result) and Assigned(FVideo) and FVideo.Valid then
    Result := FVideo.SDL_GL_GetProcAddress(SDL_String(ProcName))
end;

procedure TSDL2Library.DoGetFileVersion(out VersionStr: String);
var
  Ver: SDL_Version;
  Revision: SDL_SInt32;
begin
  if not Version.Valid then
  begin
    Ver.major := 0;
    Ver.Minor := 0;
    Ver.Patch := 0;
    Revision := 0;
  end
  else
  begin
    Version.SDL_GetVersion(Ver);
    Revision := Version.SDL_GetRevisionNumber();
  end;

  VersionStr := Format('%d.%d.%d.%d', [Ver.Major, Ver.Minor, Ver.Patch, Revision]);
end;

procedure TSDL2Library.DoGetLastError(out ErrorStr: String);
begin
  if Assigned(FError) and FError.Valid then
    ErrorStr := FError.SDL_GetError()
  else
    inherited DoGetLastError(ErrorStr)
end;

procedure TSDL2Library.DoInit;
begin
  inherited DoInit;

  if Valid then
  begin
	InitApi(TSDLErrorApi, FError);
	InitApi(TSDLVersionApi, FVersion);
	InitApi(TSDLAssertApi, FAssert);
	InitApi(TSDLAtomicApi, FAtomic);
//	InitApi(TSDLAudioApi, FAudio);
//	InitApi(TSDLClipboardApi, FClipboard);
	InitApi(TSDLCPUInfoApi, FCPUInfo);
	InitApi(TSDLEventsApi, FEvents);
	InitApi(TSDLFileSystemApi, FFileSystem);
//	InitApi(TSDLGameControllerApi, FGameControler);
//	InitApi(TSDLGestureApi, FGesture);
//	InitApi(TSDLHapticApi, FHaptic);
	InitApi(TSDLHintsApi, FHints);
	{$IFNDEF ANDROID}
//	InitApi(TSDLJoyStickApi, FJoyStick);
	{$ENDIF}
	InitApi(TSDLKeyboardApi, FKeyboard);
//	InitApi(TSDLLoadSOApi, FLoadSO);
//	InitApi(TSDLMessageBoxApi, FMessageBox);
	InitApi(TSDLLogApi, FLog);
	InitApi(TSDLMainApi, FMain);
	InitApi(TSDLMouseApi, FMouse);
	InitApi(TSDLMutexApi, FMutex);
	InitApi(TSDLPixelsApi, FPixels);
	InitApi(TSDLPlatformApi, FPlatform);
//	InitApi(TSDLPowerApi, FPower);
	InitApi(TSDLRectApi, FRect);
	InitApi(TSDLRenderApi, FRender);
//	InitApi(TSDLRwopsApi, FRWops);
//	InitApi(TSDLStrIncApi, FStdInc);
	InitApi(TSDLSurfaceApi, FSurface);
//	InitApi(TSDLTouchApi, FTouch);
	InitApi(TSDLTimerApi, FTimer);
	InitApi(TSDLThreadApi, FThreads);
	InitApi(TSDLVideoApi, FVideo)
  end
end;

procedure TSDL2Library.DoSetLastError(const ErrorStr: String);
begin
  if Assigned(FError) and FError.Valid then
  begin
    if ErrorStr = '' then
      FError.SDL_ClearError()
    else
      FError.SDL_SetError(SDL_String(ErrorStr))
  end
  else
    inherited DoSetLastError(ErrorStr)
end;

end.
