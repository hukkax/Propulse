unit SDL.Extended.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, SDL.Api.Types, SDL.Api.libSDL2, SDL.Api.Video,
  SDL.Api.Events, SDL.Api.Keyboard, LMessages, Forms, SDL.Extended.Interfaces,
  SDL.Extended.LibraryProvider, SDL.Api.libSDL_Image, SDL.Api.OpenGL, SDL.Api.OpenGL.Types;

type

  TMouseButtons = set of TMouseButton;

  TSDLWindowState = (wsSDLNormal, wsSDLMinimized, wsSDLMaximized, wsSDLFullScreen);

  TSDLPaintEvent = procedure(const Sender: TObject) of object;
  TSDLResizeEvent = procedure(const Sender: TObject; const NewWidth, NewHeight: SDL_SInt32) of object;
  TSDLMoveEvent = procedure(const Sender: TObject; const NewLeft, NewTop: SDL_SInt32) of object;
  TSDLMouseMoveEvent = procedure(const Sender: TObject; Buttons: TMouseButtons; Shift: TShiftState; X, Y, XRel, YRel: SDL_SInt32) of object;
  TSDLMouseWheelEvent = procedure(const Sender: TObject; DeltaX, DeltaY: SDL_SInt32; Direction: SDL_MouseWheelDirection) of object;
  TSDLMouseEvent = procedure(const Sender: TObject; Button: SDL_ButtonEnum; Clicks: SDL_UInt8; X, Y: SDL_SInt32) of object;
  TSDLKeyEvent = procedure(const Sender: TObject; Key: SDL_Keycode; Shift: TShiftState) of object;
  TSDLTextInputEvent = procedure(const Sender: TObject; const Text: String) of object;

  { TSDLWindow }

  TSDLWindow = class(TDataModule, IInternalProviderContainer)
  public const
    SDL_USER_PROCESS = 100; //Call DoProcess method
    SDL_USER_RENDER = 200; //Call DoRender method

    LM_WINDOWEVENT = Integer(SDL_WINDOWEVENT_EV) + LM_USER;
    LM_USEREVENT = Integer(SDL_USEREVENT_EV) + LM_USER;
    LM_MOUSE_MOTION = Integer(SDL_MOUSEMOTION) + LM_USER;
    LM_MOUSE_WHEEL = Integer(SDL_MOUSEWHEEL) + LM_USER;
    LM_MOUSE_BUTTONUP = Integer(SDL_MOUSEBUTTONUP) + LM_USER;
    LM_MOUSE_BUTTONDOWN = Integer(SDL_MOUSEBUTTONDOWN) + LM_USER;
    LM_KEYDOWN = Integer(SDL_KEYDOWN) + LM_USER;
    LM_KEYUP = Integer(SDL_KEYUP) + LM_USER;
    LM_TEXTINPUT = Integer(SDL_TEXTINPUT) + LM_USER;
  public type

    PDesignData = ^TDesignData;
    TDesignData = record
      Caption: String;
      Height: SDL_SInt32;
      Left: SDL_SInt32;
      SDL2: TSDL2Library;
      Top: SDL_SInt32;
      Visible: Boolean;
      Width: SDL_SInt32;
      WindowState: TSDLWindowState;
    end;

    { TSDLWindowSize }

    TSDLWindowSize = class(TPersistent)
    private
      FOwner: TSDLWindow;
      function GetHeight: SDL_SInt32;
      function GetWidth: SDL_SInt32;
      procedure SetHeight(AValue: SDL_SInt32);
      procedure SetWidth(AValue: SDL_SInt32);
    public
      constructor Create(const AOwner: TSDLWindow); reintroduce;
    published
      property Width: SDL_SInt32 read GetWidth write SetWidth; (** SDL window width *)
      property Height: SDL_SInt32 read GetHeight write SetHeight; (** SDL window height *)
    end;

    { TSDLWindowPosition }

    TSDLWindowPosition = class(TPersistent)
    private
      FOwner: TSDLWindow;
      function GetLeft: SDL_SInt32;
      function GetTop: SDL_SInt32;
      procedure SetLeft(AValue: SDL_SInt32);
      procedure SetTop(AValue: SDL_SInt32);
    public
      constructor Create(const AOwner: TSDLWindow); reintroduce;
    published
      property Left: SDL_SInt32 read GetLeft write SetLeft; (** SDL window x coorinate on screen *)
      property Top: SDL_SInt32 read GetTop write SetTop; (** SDL window y coorinate on screen *)
    end;

  strict private
    class var FWndCount: Integer;
  private
    FAPS: SDL_SInt32;
    FAPSTimer: SDL_TimerID;
    FClearColor: SDL_Color;
    FContextCount: SDL_SInt32;
    FContexts: array of SDL_GLContext;
    FDesignData: PDesignData;
    FFPS: SDL_SInt32;
    FFPSTimer: SDL_TimerID;
    FInternalProvider: TSDLCustomLibraryProvider;
    FMutex: PSDL_mutex;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnFocusGained: TNotifyEvent;
    FOnFocusLost: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnInitOpenGL: TSDLPaintEvent;
    FOnKeyDown: TSDLKeyEvent;
    FOnKeyPress: TSDLKeyEvent;
    FOnKeyUp: TSDLKeyEvent;
    FOnMaximize: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnMouseDown: TSDLMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseMove: TSDLMouseMoveEvent;
    FOnMouseUp: TSDLMouseEvent;
    FOnMouseWheel: TSDLMouseWheelEvent;
    FOnMove: TSDLMoveEvent;
    FOnProcess: TNotifyEvent;
    FOnRender: TSDLPaintEvent;
    FOnResize: TSDLResizeEvent;
    FOnRestore: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnTextInput: TSDLTextInputEvent;
    FPosition: TSDLWindowPosition;
    FSemaphore: PSDL_semaphore;
    FSharedContext: SDL_SInt32;
    FSize: TSDLWindowSize;
    FWnd: PSDL_Window;
    function GetCaption: String;
    function GetContexts: SDL_GLContext;
    function GetInternalProvider: TComponent;
    function GetOpenGL: TOpenGLLibrary;
    function GetSDL2: TSDL2Library;
    function GetSDL_Image: TSDL2_ImageLibrary;
    function GetVisible: Boolean;
    function GetWindowState: TSDLWindowState;
    function InternalConvertSDLButton(const SDLButtons: SDL_SInt32): TMouseButtons;
    function InternalConvertSDLModState(const KeyMods: SDL_Keymods): TShiftState;
    procedure Init;
    procedure MSG_KEYDOWN(var Msg: SDL_KeyboardEvent); message LM_KEYDOWN;
    procedure MSG_KEYUP(var Msg: SDL_KeyboardEvent); message LM_KEYUP;
    procedure MSG_MOUSE_BUTTONDOWN(var Msg: SDL_MouseButtonEvent); message LM_MOUSE_BUTTONDOWN;
    procedure MSG_MOUSE_BUTTONUP(var Msg: SDL_MouseButtonEvent); message LM_MOUSE_BUTTONUP;
    procedure MSG_MOUSE_MOTION(var Msg: SDL_MouseMotionEvent); message LM_MOUSE_MOTION;
    procedure MSG_MOUSE_WHEEL(var Msg: SDL_MouseWheelEvent); message LM_MOUSE_WHEEL;
    procedure MSG_TEXTINPUT(var Msg: SDL_TextInputEvent); message LM_TEXTINPUT;
    procedure MSG_USEREVENT(var Msg: SDL_UserEvent); message LM_USEREVENT;
    procedure MSG_WINDOWEVENT(var Msg: SDL_WindowEvent); message LM_WINDOWEVENT;
    procedure SetAPS(AValue: SDL_SInt32);
    procedure SetCaption(AValue: String);
    procedure SetClearColor(AValue: SDL_Color);
    procedure SetFPS(AValue: SDL_SInt32);
    procedure SetOnClose(AValue: TCloseEvent);
    procedure SetOnCloseQuery(AValue: TCloseQueryEvent);
    procedure SetOnFocusGained(AValue: TNotifyEvent);
    procedure SetOnFocusLost(AValue: TNotifyEvent);
    procedure SetOnHide(AValue: TNotifyEvent);
    procedure SetOnInitOpenGL(AValue: TSDLPaintEvent);
    procedure SetOnKeyDown(AValue: TSDLKeyEvent);
    procedure SetOnKeyPress(AValue: TSDLKeyEvent);
    procedure SetOnKeyUp(AValue: TSDLKeyEvent);
    procedure SetOnMaximize(AValue: TNotifyEvent);
    procedure SetOnMinimize(AValue: TNotifyEvent);
    procedure SetOnMouseDown(AValue: TSDLMouseEvent);
    procedure SetOnMouseEnter(AValue: TNotifyEvent);
    procedure SetOnMouseLeave(AValue: TNotifyEvent);
    procedure SetOnMouseMove(AValue: TSDLMouseMoveEvent);
    procedure SetOnMouseUp(AValue: TSDLMouseEvent);
    procedure SetOnMouseWheel(AValue: TSDLMouseWheelEvent);
    procedure SetOnMove(AValue: TSDLMoveEvent);
    procedure SetOnProcess(AValue: TNotifyEvent);
    procedure SetOnRender(AValue: TSDLPaintEvent);
    procedure SetOnResize(AValue: TSDLResizeEvent);
    procedure SetOnRestore(AValue: TNotifyEvent);
    procedure SetOnShow(AValue: TNotifyEvent);
    procedure SetOnTextInput(AValue: TSDLTextInputEvent);
    procedure SetSharedContext(AValue: SDL_SInt32);
    procedure SetVisible(AValue: Boolean);
    procedure SetWindowState(AValue: TSDLWindowState);
  protected
    procedure DoClose; virtual;
    procedure DoFocusGained; virtual;
    procedure DoFocusLost; virtual;
    procedure DoHide; virtual;
    procedure DoInitOpenGL; virtual;
    procedure DoKeyDown(Key: SDL_Keycode; Shift: TShiftState); virtual;
    procedure DoKeyPress(Key: SDL_Keycode; Shift: TShiftState); virtual;
    procedure DoKeyUp(Key: SDL_Keycode; Shift: TShiftState); virtual;
    procedure DoMaximize; virtual;
    procedure DoMinimize; virtual;
    procedure DoMouseDown(Button: SDL_ButtonEnum; Clicks: SDL_UInt8; X, Y: SDL_SInt32); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove(Buttons: TMouseButtons; Shift: TShiftState; X, Y, XRel, YRel: Integer); virtual;
    procedure DoMouseUp(Button: SDL_ButtonEnum; Clicks: SDL_UInt8; X, Y: SDL_SInt32); virtual;
    procedure DoMouseWheel(DeltaX, DeltaY: SDL_SInt32; Direction: SDL_MouseWheelDirection); virtual;
    procedure DoMove(const NewWidth, NewHeight: SDL_SInt32);
    procedure DoProcess; virtual;
    procedure DoRender; virtual;
    procedure DoResize(const NewWidth, NewHeight: SDL_SInt32);
    procedure DoShow; virtual;
    procedure DoTextInput(const Text: String); virtual;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(ASDL2: TSDL2Library; AOwner: TComponent); reintroduce; overload;
    constructor Create(ASDL2: TSDL2Library; const DesignData: TDesignData; AOwner: TComponent); reintroduce; overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    function GetSharedContext(out Context: SDL_GLContext): Boolean;
    function PutSharedContext(const Context: SDL_GLContext): Boolean;

    procedure RegisterPropEdits;

    (**
      * \brief Show SDL window
      *)
    procedure Show;
    (**
      * \brief Hide SDL window
      *)
    procedure Hide;
    function DesignTime: Boolean;

    property SDL2: TSDL2Library read GetSDL2;
    property SDL_Image: TSDL2_ImageLibrary read GetSDL_Image;
    property OpenGL: TOpenGLLibrary read GetOpenGL;
    property Wnd: PSDL_Window read FWnd;
    property Context: SDL_GLContext read GetContexts;
    property ClearColor: SDL_Color read FClearColor write SetClearColor; (** Color witch is used to clear window *)
  published
    property APS: SDL_SInt32 read FAPS write SetAPS; (** Actions per second used to process something *)
    property Caption: String read GetCaption write SetCaption; (** SDL window title *)
    property FPS: SDL_SInt32 read FFPS write SetFPS; (** Frames per second used to redraw window *)
    property Position: TSDLWindowPosition read FPosition write FPosition; (** SDL window X and Y coordinate on screen *)
    property Size: TSDLWindowSize read FSize write FSize; (** SDL window width and height *)
    property Visible: Boolean read GetVisible write SetVisible; (** Hide or show SDL window *)
    property WindowState: TSDLWindowState read GetWindowState write SetWindowState; (** Maximized, Minimized, FullScreen, Normal *)
    property SharedContext: SDL_SInt32 read FSharedContext write SetSharedContext; (** Tottal avaible shared context count. By default if value is 0 system will take procesor count. Do not change when worker is active. *)

    property OnClose: TCloseEvent read FOnClose write SetOnClose; (** System closes window *)
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write SetOnCloseQuery; (** System checks if window can be closed *)
    property OnFocusGained: TNotifyEvent read FOnFocusGained write SetOnFocusGained; (** window has gained keyboard focus *)
    property OnFocusLost: TNotifyEvent read FOnFocusLost write SetOnFocusLost; (** window has lost keyboard focus *)
    property OnHide: TNotifyEvent read FOnHide write SetOnHide; (**window has been hidden *)
    property OnInitOpenGL: TSDLPaintEvent read FOnInitOpenGL write SetOnInitOpenGL; (** Runs after OpenGL context and renderer is created *)
    property OnKeyDown: TSDLKeyEvent read FOnKeyDown write SetOnKeyDown; (** Key presed on focused window *)
    property OnKeyPress: TSDLKeyEvent read FOnKeyPress write SetOnKeyPress; (** Key is pressed on focused window *)
    property OnKeyUp: TSDLKeyEvent read FOnKeyUp write SetOnKeyUp; (** Key presed on released window *)
    property OnMaximize: TNotifyEvent read FOnMaximize write SetOnMaximize; (** window has been maximized *)
    property OnMinimize: TNotifyEvent read FOnMinimize write SetOnMinimize; (** window has been minimized *)
    property OnMouseDown: TSDLMouseEvent read FOnMouseDown write SetOnMouseDown; (** Someone pushed mouse button up on window *)
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter; (** window has gained mouse focus *)
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave; (** window has lost mouse focus *)
    property OnMouseMove: TSDLMouseMoveEvent read FOnMouseMove write SetOnMouseMove; (** Mouse moves on window surface *)
    property OnMouseUp: TSDLMouseEvent read FOnMouseUp write SetOnMouseUp; (** Someone pushed mouse button down on window *)
    property OnMouseWheel: TSDLMouseWheelEvent read FOnMouseWheel write SetOnMouseWheel; (** Someone scroled mouse wheel on window *)
    property OnMove: TSDLMoveEvent read FOnMove write SetOnMove; (** window has been moved *)
    property OnProcess: TNotifyEvent read FOnProcess write SetOnProcess; (** window user procesess *)
    property OnRender: TSDLPaintEvent read FOnRender write SetOnRender; (** window is rendering scene *)
    property OnResize: TSDLResizeEvent read FOnResize write SetOnResize; (** window has been resized  *)
    property OnRestore: TNotifyEvent read FOnRestore write SetOnRestore; (** window has been restored to normal size and position *)
    property OnShow: TNotifyEvent read FOnShow write SetOnShow; (** window has been shown *)
    property OnTextInput: TSDLTextInputEvent read FOnTextInput write SetOnTextInput; (** Text input from Os *)
  end;

  TSDLWindowClass = class of TSDLWindow;

implementation

uses
  SDL.Extended.Strings, SDL.Extended.Application, Graphics, math;

function EventWatch( userdata: Pointer; event: PSDL_Event ): SDL_SInt32 cdecl;
var
  Wnd: PSDL_Window;
  WndObj: TSDLWindow;
  WndMsg: SDL_Event;
begin
  WndObj := TSDLWindow(userdata);
  Wnd := WndObj.FWnd;

  with WndObj.SDL2.Video do
  begin
    WndMsg := event^;
    WndMsg._type := SDL_EventType(Integer(WndMsg._type) + LM_USER);

    if (event^._type = SDL_WINDOWEVENT_EV) and (event^.window.event <> SDL_WINDOWEVENT_CLOSE) then
    begin
      if (Wnd = SDL_GetWindowFromID(event^.window.windowID)) then
        WndObj.Dispatch(WndMsg)
    end
    else
    if event^._type = SDL_MOUSEMOTION then
    begin
      if Wnd = SDL_GetWindowFromID(event^.motion.windowID) then
        WndObj.Dispatch(WndMsg)
    end
    else
    if event^._type = SDL_MOUSEWHEEL then
    begin
      if Wnd = SDL_GetWindowFromID(event^.wheel.windowID) then
        WndObj.Dispatch(WndMsg)
    end
    else
    if event^._type in (SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP) then
    begin
      if Wnd = SDL_GetWindowFromID(event^.button.windowID) then
        WndObj.Dispatch(WndMsg)
    end
    else
    if event^._type in (SDL_KEYDOWN or SDL_KEYUP) then
    begin
      if Wnd = SDL_GetWindowFromID(event^.key.windowID) then
        WndObj.Dispatch(WndMsg)
    end
    else
    if event^._type = SDL_TEXTEDITING then
    begin
      if Wnd = SDL_GetWindowFromID(event^.edit.windowID) then
        WndObj.Dispatch(WndMsg)
    end
    else
    if event^._type = SDL_TEXTINPUT then
    begin
      if Wnd = SDL_GetWindowFromID(event^.text.windowID) then
        WndObj.Dispatch(WndMsg)
    end
  end;

  Result := 1;
end;

function RedrawWnd(interval: SDL_UInt32; param: SDL_Data): SDL_UInt32 cdecl;
var
  WndObj: TSDLWindow;
  Event: SDL_Event;
begin
  WndObj := TSDLWindow(param);

  Event._type := SDL_USEREVENT_EV;
  Event.user.code := TSDLWindow.SDL_USER_RENDER;
  Event.user.windowID := WndObj.SDL2.Video.SDL_GetWindowID(WndObj.Wnd);
  WndObj.SDL2.Events.SDL_PushEvent(@Event);

  if QWord(interval) <> 1000 div WndObj.FPS then
    Result := 1000 div WndObj.FPS
  else
    Result := interval
end;

function ProcessWnd(interval: SDL_UInt32; param: SDL_Data): SDL_UInt32 cdecl;
var
  WndObj: TSDLWindow;
  Event: SDL_Event;
begin
  WndObj := TSDLWindow(param);

  Event._type := SDL_USEREVENT_EV;
  Event.user.code := TSDLWindow.SDL_USER_PROCESS;
  Event.user.windowID := WndObj.SDL2.Video.SDL_GetWindowID(WndObj.Wnd);
  WndObj.SDL2.Events.SDL_PushEvent(@Event);

  if QWord(interval) <> 1000 div WndObj.APS then
    Result := 1000 div WndObj.APS
  else
    Result := interval
end;

{ TSDLWindow.TSDLWindowPosition }

function TSDLWindow.TSDLWindowPosition.GetLeft: SDL_SInt32;
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_GetWindowPosition(FOwner.FWnd, @Result, nil)
  else
    Result := FOwner.FDesignData^.Left
end;

function TSDLWindow.TSDLWindowPosition.GetTop: SDL_SInt32;
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_GetWindowPosition(FOwner.FWnd, nil, @Result)
  else
    Result := FOwner.FDesignData^.Left
end;

procedure TSDLWindow.TSDLWindowPosition.SetLeft(AValue: SDL_SInt32);
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_SetWindowPosition(FOwner.FWnd, AValue, Top)
  else
    FOwner.FDesignData^.Left := AValue
end;

procedure TSDLWindow.TSDLWindowPosition.SetTop(AValue: SDL_SInt32);
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_SetWindowPosition(FOwner.FWnd, Left, AValue)
  else
    FOwner.FDesignData^.Top := AValue
end;

constructor TSDLWindow.TSDLWindowPosition.Create(const AOwner: TSDLWindow);
begin
  FOwner := AOwner
end;

{ TSDLWindow.TSDLWindowSize }

procedure TSDLWindow.TSDLWindowSize.SetHeight(AValue: SDL_SInt32);
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_SetWindowSize(FOwner.FWnd, Width, AValue)
  else
    FOwner.FDesignData^.Height := AValue
end;

function TSDLWindow.TSDLWindowSize.GetHeight: SDL_SInt32;
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_GetWindowSize(FOwner.FWnd, nil, @Result)
  else
    Result := FOwner.FDesignData^.Height;
end;

function TSDLWindow.TSDLWindowSize.GetWidth: SDL_SInt32;
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_GetWindowSize(FOwner.FWnd, @Result, nil)
  else
    Result := FOwner.FDesignData^.Width
end;

procedure TSDLWindow.TSDLWindowSize.SetWidth(AValue: SDL_SInt32);
begin
  if Assigned(FOwner.FWnd) then
    FOwner.SDL2.Video.SDL_SetWindowSize(FOwner.FWnd, AValue, Height)
  else
    FOwner.FDesignData^.Width := AValue
end;

constructor TSDLWindow.TSDLWindowSize.Create(const AOwner: TSDLWindow);
begin
  FOwner := AOwner
end;

{ TSDLWindow }

procedure TSDLWindow.SetFPS(AValue: SDL_SInt32);
var
  NewValue: SDL_SInt32;
begin
  if AValue < 1 then
    NewValue := 1
  else
  if AValue > 1000 then
    NewValue := 1000
  else
    NewValue := AValue;

  FFPS := NewValue;
end;

procedure TSDLWindow.SetOnClose(AValue: TCloseEvent);
begin
  FOnClose:=AValue;
end;

procedure TSDLWindow.SetOnCloseQuery(AValue: TCloseQueryEvent);
begin
  FOnCloseQuery:=AValue;
end;

procedure TSDLWindow.SetOnFocusGained(AValue: TNotifyEvent);
begin
  FOnFocusGained:=AValue;
end;

procedure TSDLWindow.SetOnFocusLost(AValue: TNotifyEvent);
begin
  FOnFocusLost:=AValue;
end;

procedure TSDLWindow.SetOnHide(AValue: TNotifyEvent);
begin
  FOnHide:=AValue;
end;

procedure TSDLWindow.SetOnInitOpenGL(AValue: TSDLPaintEvent);
begin
  FOnInitOpenGL:=AValue;
end;

procedure TSDLWindow.SetOnKeyDown(AValue: TSDLKeyEvent);
begin
  FOnKeyDown:=AValue;
end;

procedure TSDLWindow.SetOnKeyPress(AValue: TSDLKeyEvent);
begin
  FOnKeyPress:=AValue;
end;

procedure TSDLWindow.SetOnKeyUp(AValue: TSDLKeyEvent);
begin
  FOnKeyUp:=AValue;
end;

procedure TSDLWindow.SetOnMaximize(AValue: TNotifyEvent);
begin
  FOnMaximize:=AValue;
end;

procedure TSDLWindow.SetOnMinimize(AValue: TNotifyEvent);
begin
  FOnMinimize:=AValue;
end;

procedure TSDLWindow.SetOnMouseDown(AValue: TSDLMouseEvent);
begin
  FOnMouseDown:=AValue;
end;

procedure TSDLWindow.SetOnMouseEnter(AValue: TNotifyEvent);
begin
  FOnMouseEnter:=AValue;
end;

procedure TSDLWindow.SetOnMouseLeave(AValue: TNotifyEvent);
begin
  FOnMouseLeave:=AValue;
end;

procedure TSDLWindow.SetOnMouseMove(AValue: TSDLMouseMoveEvent);
begin
  FOnMouseMove:=AValue;
end;

procedure TSDLWindow.SetOnMouseUp(AValue: TSDLMouseEvent);
begin
  FOnMouseUp:=AValue;
end;

procedure TSDLWindow.SetOnMouseWheel(AValue: TSDLMouseWheelEvent);
begin
  FOnMouseWheel:=AValue;
end;

procedure TSDLWindow.SetOnMove(AValue: TSDLMoveEvent);
begin
  FOnMove:=AValue;
end;

procedure TSDLWindow.SetOnRender(AValue: TSDLPaintEvent);
begin
  FOnRender:=AValue;
end;

procedure TSDLWindow.SetOnProcess(AValue: TNotifyEvent);
begin
  FOnProcess:=AValue;
end;

procedure TSDLWindow.SetOnResize(AValue: TSDLResizeEvent);
begin
  FOnResize:=AValue;
end;

procedure TSDLWindow.SetOnRestore(AValue: TNotifyEvent);
begin
  FOnRestore:=AValue;
end;

procedure TSDLWindow.SetOnShow(AValue: TNotifyEvent);
begin
  FOnShow:=AValue;
end;

procedure TSDLWindow.SetOnTextInput(AValue: TSDLTextInputEvent);
begin
  FOnTextInput:=AValue;
end;

procedure TSDLWindow.SetSharedContext(AValue: SDL_SInt32);
begin
  if AValue < 0 then
    FSharedContext := 0
  else
    FSharedContext:=AValue
end;

procedure TSDLWindow.SetCaption(AValue: String);
begin
  if Assigned(FWnd) then
    SDL2.Video.SDL_SetWindowTitle(FWnd, SDL_String(AValue))
  else
    FDesignData^.Caption := AValue
end;

procedure TSDLWindow.SetClearColor(AValue: SDL_Color);
begin
  FClearColor:=AValue;

  OpenGL.glClearColor(AValue.r / 255, AValue.g / 255, AValue.b / 255, AValue.a / 255);
end;

function TSDLWindow.GetCaption: String;
begin
  if Assigned(FWnd) then
    Result := SDL2.Video.SDL_GetWindowTitle(FWnd)
  else
    Result := FDesignData^.Caption;
end;

function TSDLWindow.GetContexts: SDL_GLContext;
begin
  if Length(FContexts) > 0 then
    Result := FContexts[High(FContexts)]
  else
    Result := nil
end;

function TSDLWindow.GetInternalProvider: TComponent;
begin
  if not Assigned(FInternalProvider) then
  begin
    FInternalProvider := TSDLLibraryProvider.Create(nil);
    FInternalProvider.Name := 'Internal';
    if not DesignTime then
    begin
      FInternalProvider.Libs['SDL2'] := SDL2;
      FInternalProvider.Libs['SDL_Image'] := SDL_Image;
      FInternalProvider.Libs['OpenGL'] := OpenGL;
      FInternalProvider.Active := True;
    end
  end;

  Result := FInternalProvider
end;

function TSDLWindow.GetOpenGL: TOpenGLLibrary;
begin
  Result := Application.OpenGL;
end;

function TSDLWindow.GetSDL2: TSDL2Library;
begin
  Result := Application.SDL2Lib;
end;

function TSDLWindow.GetSDL_Image: TSDL2_ImageLibrary;
begin
  Result := Application.SDL_Image
end;

function TSDLWindow.GetVisible: Boolean;
begin
  if Assigned(FWnd) then
    Result := SDL_WINDOW_SHOWN in SDL2.Video.SDL_GetWindowFlags(FWnd)
  else
    Result := FDesignData^.Visible
end;

function TSDLWindow.GetWindowState: TSDLWindowState;
var
  Flags: SDL_WindowFlags;
begin
  if Assigned(FWnd) then
  begin
    Flags := SDL2.Video.SDL_GetWindowFlags(FWnd);
    if SDL_WINDOW_MINIMIZED in Flags then
      Exit(wsSDLMinimized)
    else
    if (SDL_WINDOW_FULLSCREEN in Flags) then
      Exit(wsSDLFullScreen)
    else
    if (SDL_WINDOW_FULLSCREEN_DESKTOP) in Flags then
      Exit(wsSDLFullScreen)
    else
    if (SDL_WINDOW_MAXIMIZED in Flags) then
      Exit(wsSDLMaximized)
    else
      Exit(wsSDLNormal)
  end
  else
    Result := FDesignData^.WindowState
end;

procedure TSDLWindow.SetAPS(AValue: SDL_SInt32);
var
  NewValue: SDL_SInt32;
begin
  if AValue < 1 then
    NewValue := 1
  else
  if AValue > 1000 then
    NewValue := 1000
  else
    NewValue := AValue;

  FAPS:=NewValue;
end;

procedure TSDLWindow.SetVisible(AValue: Boolean);
begin
  if Visible then
    Exit;

  if Assigned(FWnd) then
  begin
    if AValue then
      SDL2.Video.SDL_ShowWindow(FWnd)
    else
      SDL2.Video.SDL_HideWindow(FWnd)
  end
  else
    FDesignData^.Visible := AValue
end;

procedure TSDLWindow.SetWindowState(AValue: TSDLWindowState);
var
  OldState: TSDLWindowState;
begin
  OldState := WindowState;
  if AValue = OldState then
    Exit;

  if Assigned(FWnd) then
  begin
    case AValue of
      wsSDLMinimized: SDL2.Video.SDL_MinimizeWindow(FWnd);
      wsSDLMaximized: SDL2.Video.SDL_MaximizeWindow(FWnd);
      wsSDLFullScreen: SDL2.Video.SDL_SetWindowFullscreen(FWnd, SDL_WINDOW_FULLSCREEN);
      wsSDLNormal:
        begin
          if OldState = wsSDLFullScreen then
            SDL2.Video.SDL_SetWindowFullscreen(FWnd, SDL_WINDOW_WINDOWED)
          else
            SDL2.Video.SDL_RestoreWindow(FWnd);
        end;
    end;
  end
  else
    FDesignData^.WindowState := AValue
end;

procedure TSDLWindow.Init;
begin
  New(FDesignData);
  FSize := TSDLWindowSize.Create(Self);
  FSize.Width := 800;
  FSize.Height := 600;

  FPosition := TSDLWindowPosition.Create(Self);
  FPosition.Left := 0;
  FPosition.Top := 0;

  FDesignData^.WindowState := wsSDLNormal;

  FPS := 60;
  APS := 60;

  FSharedContext := 0;
  FClearColor := SDL_Color.Make(0, 0, 0, 0);
end;

function TSDLWindow.InternalConvertSDLButton(const SDLButtons: SDL_SInt32): TMouseButtons;
begin
  Result := [];

  with SDL2.Mouse do
  begin
    if (SDLButtons and Integer(SDL_Button(SDL_BUTTON_LEFT))) = Integer(SDL_BUTTON_LMASK) then
      Result := Result + [mbLeft];

    if (SDLButtons and Integer(SDL_Button(SDL_BUTTON_RIGHT))) = Integer(SDL_BUTTON_RMASK) then
      Result := Result + [mbRight];

    if (SDLButtons and Integer(SDL_Button(SDL_BUTTON_MIDDLE))) = Integer(SDL_BUTTON_MMASK) then
      Result := Result + [mbMiddle];

    if (SDLButtons and Integer(SDL_Button(SDL_BUTTON_X1))) = Integer(SDL_BUTTON_X1MASK) then
      Result := Result + [mbExtra1];

    if (SDLButtons and Integer(SDL_Button(SDL_BUTTON_X2))) = Integer(SDL_BUTTON_X2MASK) then
      Result := Result + [mbExtra2];
  end
end;

function TSDLWindow.InternalConvertSDLModState(const KeyMods: SDL_Keymods
  ): TShiftState;
begin
  Result := [];

  with SDL2.Mouse do
  begin
    if KMOD_LSHIFT in KeyMods then
      Result := Result + [ssShift, ssLeft];

    if KMOD_RSHIFT in KeyMods then
      Result := Result + [ssShift, ssRight];

    if KMOD_LCTRL in KeyMods then
      Result := Result + [ssCtrl, ssLeft];

    if KMOD_RCTRL in KeyMods then
      Result := Result + [ssCtrl, ssRight];

    if KMOD_LALT in KeyMods then
      Result := Result + [ssAlt, ssLeft];

    if KMOD_RALT in KeyMods then
      Result := Result + [ssAlt, ssRight];

    if KMOD_LGUI in KeyMods then
      Result := Result + [ssHyper, ssLeft];

    if KMOD_RGUI in KeyMods then
      Result := Result + [ssHyper, ssRight];

    if KMOD_NUM in KeyMods then
      Result := Result + [ssNum];

    if KMOD_CAPS in KeyMods then
      Result := Result + [ssCaps];

    if KMOD_MODE in KeyMods then
      Result := Result + [ssAltGr];
  end;
end;

procedure TSDLWindow.MSG_WINDOWEVENT(var Msg: SDL_WindowEvent);
begin
  case Msg.event of
    SDL_WINDOWEVENT_CLOSE: DoClose;
    SDL_WINDOWEVENT_SHOWN: DoShow;
    SDL_WINDOWEVENT_HIDDEN: DoHide;
    SDL_WINDOWEVENT_MOVED: DoMove(Msg.data1, Msg.data2);
    SDL_WINDOWEVENT_RESIZED: DoResize(Msg.data1, Msg.data2);
    SDL_WINDOWEVENT_MINIMIZED: DoMinimize;
    SDL_WINDOWEVENT_MAXIMIZED: DoMaximize;
    SDL_WINDOWEVENT_ENTER: DoMouseEnter;
    SDL_WINDOWEVENT_LEAVE: DoMouseLeave;
    SDL_WINDOWEVENT_FOCUS_GAINED: DoFocusGained;
    SDL_WINDOWEVENT_FOCUS_LOST: DoFocusLost;
  end;
end;

procedure TSDLWindow.MSG_KEYDOWN(var Msg: SDL_KeyboardEvent);
begin
  if Msg._repeat = 0 then
    DoKeyDown(Msg.keysym.sym, InternalConvertSDLModState(Msg.keysym.amod))
  else
    DoKeyPress(Msg.keysym.sym, InternalConvertSDLModState(Msg.keysym.amod))
end;

procedure TSDLWindow.MSG_KEYUP(var Msg: SDL_KeyboardEvent);
begin
  DoKeyUp(Msg.keysym.sym, InternalConvertSDLModState(Msg.keysym.amod))
end;

procedure TSDLWindow.MSG_TEXTINPUT(var Msg: SDL_TextInputEvent);
begin
  DoTextInput(Msg.text)
end;

procedure TSDLWindow.MSG_USEREVENT(var Msg: SDL_UserEvent);
begin
  case Msg.code of
    SDL_USER_PROCESS: DoProcess;
    SDL_USER_RENDER: DoRender;
  end;
end;

procedure TSDLWindow.MSG_MOUSE_MOTION(var Msg: SDL_MouseMotionEvent);
begin
  DoMouseMove(
    InternalConvertSDLButton(Msg.state),
    InternalConvertSDLModState(SDL2.Keyboard.SDL_GetModState()),
    Msg.x,
    Msg.y,
    Msg.xrel,
    Msg.yrel
  )
end;

procedure TSDLWindow.MSG_MOUSE_WHEEL(var Msg: SDL_MouseWheelEvent);
begin
  DoMouseWheel(Msg.x, Msg.y, Msg.direction)
end;

procedure TSDLWindow.MSG_MOUSE_BUTTONUP(var Msg: SDL_MouseButtonEvent);
begin
  DoMouseUp(Msg.button, Msg.clicks, Msg.x, Msg.y)
end;

procedure TSDLWindow.MSG_MOUSE_BUTTONDOWN(var Msg: SDL_MouseButtonEvent);
begin
  DoMouseDown(Msg.button, Msg.clicks, Msg.x, Msg.y)
end;

procedure TSDLWindow.DoClose;
var
  CanClose: Boolean;
  CloseAction: TCloseAction;
begin
  CanClose := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, CanClose);

  if CanClose then
  begin
    CloseAction := caFree;
    if Assigned(FOnClose) then
      FOnClose(Self, CloseAction);

    case CloseAction of
      caFree:
        begin
          SDL2.Timer.SDL_RemoveTimer(FFPSTimer);
          SDL2.Timer.SDL_RemoveTimer(FAPSTimer);
          FFPSTimer := 0;
          FAPSTimer := 0;
          SDL2.Events.SDL_DelEventWatch(@EventWatch, Self);
          Application.ReleaseComponent(Self);
        end;
      caHide: Hide;
      caMinimize: WindowState := wsSDLMinimized;
    end
  end
end;

procedure TSDLWindow.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self)
end;

procedure TSDLWindow.DoTextInput(const Text: String);
begin
  if Assigned(FOnTextInput) then
    FOnTextInput(Self, Text)
end;

procedure TSDLWindow.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self)
end;

procedure TSDLWindow.DoRender;
begin
  OpenGL.glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if Assigned(FOnRender) then
    FOnRender(Self);

  FPONotifyObservers(Self, ooCustom, Pointer(LM_PAINT));

  SDL2.Video.SDL_GL_SwapWindow(FWnd);
end;

procedure TSDLWindow.DoInitOpenGL;
begin
  OpenGL.glClearColor(FClearColor.r / 255, FClearColor.g / 255, FClearColor.b / 255, FClearColor.a / 255);

  if Assigned(FOnInitOpenGL) then
    FOnInitOpenGL(Self)
end;

procedure TSDLWindow.DoResize(const NewWidth, NewHeight: SDL_SInt32);
begin
  if Assigned(FOnResize) then
    FOnResize(Self, NewWidth, NewHeight)
end;

procedure TSDLWindow.DoMouseWheel(DeltaX, DeltaY: SDL_SInt32; Direction: SDL_MouseWheelDirection);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, DeltaX, DeltaY, Direction)
end;

procedure TSDLWindow.DoKeyUp(Key: SDL_Keycode; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift)
end;

procedure TSDLWindow.DoKeyDown(Key: SDL_Keycode; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift)
end;

procedure TSDLWindow.DoKeyPress(Key: SDL_Keycode; Shift: TShiftState);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key, Shift)
end;

procedure TSDLWindow.DoMove(const NewWidth, NewHeight: SDL_SInt32);
begin
  if Assigned(FOnMove) then
    FOnMove(Self, NewWidth, NewHeight)
end;

procedure TSDLWindow.DoMinimize;
begin
  if Assigned(FOnMinimize) then
    FOnMinimize(Self)
end;

procedure TSDLWindow.DoMaximize;
begin
  if Assigned(FOnMaximize) then
    FOnMaximize(Self)
end;

procedure TSDLWindow.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self)
end;

procedure TSDLWindow.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self)
end;

procedure TSDLWindow.DoMouseMove(Buttons: TMouseButtons; Shift: TShiftState; X, Y, XRel, YRel: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Buttons, Shift, X, Y, XRel, YRel)
end;

procedure TSDLWindow.DoFocusGained;
begin
  if Assigned(FOnFocusGained) then
    FOnFocusGained(Self)
end;

procedure TSDLWindow.DoFocusLost;
begin
  if Assigned(FOnFocusLost) then
    FOnFocusLost(Self)
end;

procedure TSDLWindow.DoProcess;
begin
  if Assigned(FOnProcess) then
    FOnProcess(Self)
end;

procedure TSDLWindow.DoMouseDown(Button: SDL_ButtonEnum; Clicks: SDL_UInt8; X, Y: SDL_SInt32);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Clicks, X, Y)
end;

procedure TSDLWindow.DoMouseUp(Button: SDL_ButtonEnum; Clicks: SDL_UInt8; X, Y: SDL_SInt32);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Clicks, X, Y)
end;

constructor TSDLWindow.Create(AOwner: TComponent);
begin
  Init;
  FWnd := nil;
  inherited Create(AOwner);
  if not DesignTime then
    raise SDLException.Create(rsErrDessignTImeCreate);
end;

constructor TSDLWindow.Create(ASDL2: TSDL2Library; AOwner: TComponent);
begin
  Init;
  inherited Create(AOwner);
end;

constructor TSDLWindow.Create(ASDL2: TSDL2Library;
  const DesignData: TDesignData; AOwner: TComponent);
begin
  Init;
  Create(ASDL2, AOwner);

  New(FDesignData);
  FDesignData^ := DesignData
end;

procedure TSDLWindow.AfterConstruction;

  procedure PrintVideoDisplays;
  var
    i: Integer;
    Displays: TStringList;
  begin
    Displays := TStringList.Create;
    try
      for i := 0 to SDL2.Video.SDL_GetNumVideoDisplays() -1 do
        Displays.Add('Display(' + IntToStr(i) + '): ' + SDL2.Video.SDL_GetDisplayName(i));

      Application.Log(etInfo, 'Video Displays(' + IntToStr(Displays.Count) + '): ');
      for i := 0 to Displays.Count - 1 do
        Application.Log(etInfo, '    ' + Displays[i]);
    finally
      Displays.Free;
    end;
  end;

  procedure PrintVideoDrivers;
  var
    i: Integer;
    Drivers: TStringList;
  begin
    Drivers := TStringList.Create;
    try
      for i := 0 to SDL2.Video.SDL_GetNumVideoDrivers() -1 do
        Drivers.Add('Driver(' + IntToStr(i) + '): ' + SDL2.Video.SDL_GetVideoDriver(i));

      Application.Log(etInfo, 'Video drivers(' + IntToStr(Drivers.Count) + '): ');
      for i := 0 to Drivers.Count - 1 do
        Application.Log(etInfo, '    ' + Drivers[i]);
    finally
      Drivers.Free;
    end;
  end;

  procedure PrintDisplayModes;
  var
    DispMode: SDL_DisplayMode;
    i: Integer;
    Modes: TStringList;
  begin
    Modes := TStringList.Create;
    try
      for i := 0 to SDL2.Video.SDL_GetNumDisplayModes(0) - 1 do
      begin
        SDL2.Video.SDL_GetDisplayMode(0, i, DispMode);
        Modes.Add(Format('Mode(%d): %dx%d %d', [i, DispMode.h, DispMode.w, DispMode.refresh_rate]));
      end;

      Application.Log(etInfo, 'Display modes(' + IntToStr(Modes.Count) + '): ');
      for i := 0 to Modes.Count - 1 do
        Application.Log(etInfo, '    ' + Modes[i]);
    finally
      Modes.Free;
    end;
  end;

var
  i: Integer;
  Desired, Closest: SDL_DisplayMode;
  Event: SDL_Event;
begin
  if not DesignTime then with SDL2, SDL2.Video do
  begin
    if not SDL2.Valid then
      raise SDLCriticalException.Create(rsErrUnitializedLibrary);

    if not (SDL_INIT_VIDEO in SDL_WasInit(SDL_INIT_VIDEO)) then
    begin
      Application.Log(etDebug, 'SDL_InitSubSystem(SDL_INIT_VIDEO)');
      SDL_InitSubSystem(SDL_INIT_VIDEO);
    end;

    PrintVideoDisplays;
    PrintVideoDrivers;
    PrintDisplayModes;

    Application.Log(etDebug, 'SDL_VideoInit');
    if not SDL_VideoInit(nil) then
      Application.Log(etError, SDL2.LastError);

    SDL2.Video.SDL_GL_LoadLibrary(nil);

    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);

    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
    SDL_GL_SetAttribute(SDL_GL_BUFFER_SIZE, 32);

    SDL_GL_SetAttribute(SDL_GL_ACCUM_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_ALPHA_SIZE, 8);

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    {$IFNDEF ANDROID}
    SDL_GL_SetAttribute(SDL_GL_SHARE_WITH_CURRENT_CONTEXT, Ord(SDL_TRUE));
    {$ELSE}
    SDL_GL_SetAttribute(SDL_GL_SHARE_WITH_CURRENT_CONTEXT, Ord(SDL_TRUE));
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, Ord(SDL_GL_CONTEXT_PROFILE_ES));
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
    {$ENDIF}

    Desired.w := Size.Width;
    Desired.h := Size.Height;
    Desired.refresh_rate := 60;
    Desired.format := SDL_PIXELFORMAT_UNKNOWN;
    SDL2.Video.SDL_GetClosestDisplayMode(0, @Desired, Closest);

    Application.Log(etInfo, 'Closest display mode');
    Application.Log(etInfo, '{');
    Application.Log(etInfo, '  Width: ' + IntToStr(Closest.w));
    Application.Log(etInfo, '  Height: ' + IntToStr(Closest.h));
    Application.Log(etInfo, '  Refresh Rate: ' + IntToStr(Closest.refresh_rate));
    Application.Log(etInfo, '}');

    Size.Width := Closest.w;
    Size.Height := Closest.h;

    FWnd := SDL_CreateWindow(
      SDL_String(Caption),
      SDL_WINDOWPOS_UNDEFINED_DISPLAY(FPosition.Left),
      SDL_WINDOWPOS_UNDEFINED_DISPLAY(FPosition.Top),
      Size.Width,
      Size.Height,
      {$IFDEF ANDROID}SDL_WINDOW_ALLOW_HIGHDPI or {$ENDIF}SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN
    );

    if not Assigned(FWnd) then
      raise SDLCriticalException.Create(SDL2.Error.SDL_GetError())
    else
      Application.Log(etDebug, 'SDL window created!');

    while SDL2.Events.SDL_PollEvent(@Event) = SDL_TRUE do;

    SDL2.Video.SDL_SetWindowDisplayMode(FWnd, @Closest);

    if FSharedContext = 0 then
      FSharedContext := 1;

    SDL2.Video.SDL_GL_LoadLibrary(nil);
    FContextCount := SharedContext + 1;
    SetLength(FContexts, FContextCount);
    FSemaphore := SDL2.Mutex.SDL_CreateSemaphore(0);
    FMutex := SDL2.Mutex.SDL_CreateMutex();

    Application.Log(etInfo, 'Context count: ' + IntToStr(SharedContext));

    //Create shared contexts
    for i := 0 to SharedContext do
    begin
      FContexts[i] := SDL2.Video.SDL_GL_CreateContext(FWnd);
      Application.Log(etInfo, 'GL context created(' + IntToStr(i) + ')');

      if not Assigned(FContexts[i]) then
        Application.Log(etError, SDL2.LastError);

      if i <> SharedContext then
      begin
        //Last context is main context so it is no avaible for sharing with others
        Mutex.SDL_SemPost(FSemaphore);
      end
    end;
    Application.Log(etError, SDL2.LastError);
    SDL_SetWindowData(FWnd, 'window', Self);

    Application.Log(etInfo, 'OpenGL: Loading');
    Application.Log(etInfo, 'OpenGL: Initialiazing');
    OpenGL.Init;

    if not OpenGL.Valid then
      raise SDLCriticalException.Create(OpenGL.LastError);
    Application.Log(etInfo, 'OpenGL: Initialized');

    Events.SDL_AddEventWatch(@EventWatch, Self);
    FFPSTimer := Timer.SDL_AddTimer(1000 div FPS, @RedrawWnd, Self);

    if FFPSTimer = 0 then
      Application.Log(etError, LastError);

    FAPSTimer := Timer.SDL_AddTimer(1000 div APS, @ProcessWnd, Self);

    if FAPSTimer = 0 then
      Application.Log(etError, LastError);

    Dispose(FDesignData);
    FDesignData := nil;
  end;

  inherited AfterConstruction;

  InterLockedIncrement(FWndCount);

  if not DesignTime then
  begin
    FPONotifyObservers(Self, ooChange, Pointer(LM_CREATE));
    DoInitOpenGL;
    Application.Log(etDebug, 'SDL window initialized')
  end
end;

procedure TSDLWindow.BeforeDestruction;
begin
  if not DesignTime then
  begin
    SDL2.Mutex.SDL_DestroySemaphore(FSemaphore);
    SDL2.Mutex.SDL_DestroyMutex(FMutex);
  end;

  if Assigned(FWnd) then
    SDL2.Video.SDL_DestroyWindow(FWnd);

  FreeAndNil(FInternalProvider);

  inherited BeforeDestruction;
end;

destructor TSDLWindow.Destroy;
var
  i: Integer;
begin
  if not DesignTime then with SDL2 do
  begin
    if FFPSTimer <> 0 then
      Timer.SDL_RemoveTimer(FFPSTimer);

    if FAPSTimer <> 0 then
      Timer.SDL_RemoveTimer(FAPSTimer);

    for i := Low(FContexts) to High(FContexts) do
      Video.SDL_GL_DeleteContext(FContexts[i]);
    Video.SDL_DestroyWindow(FWnd);

    if InterLockedDecrement(FWndCount) = 0 then
    begin
      Video.SDL_VideoQuit();
      SDL_QuitSubSystem(SDL_INIT_VIDEO);
    end;
  end
  else
    Dispose(FDesignData);

  FPosition.Free;
  FSize.Free;

  inherited Destroy;
end;

function TSDLWindow.GetSharedContext(out Context: SDL_GLContext): Boolean;
begin
  Result := False;
  Context := nil;

  with SDL2.Mutex do
  begin
    SDL_SemWait(FSemaphore);

    SDL2.Mutex.SDL_LockMutex(FMutex);
    try
      Context := FContexts[Length(FContexts) - FContextCount];
      Dec(FContextCount)
    finally
      SDL2.Mutex.SDL_UnlockMutex(FMutex)
    end
  end;
end;

function TSDLWindow.PutSharedContext(const Context: SDL_GLContext): Boolean;
begin
  Result := False;

  with SDL2.Mutex do
  begin
    SDL_LockMutex(FMutex);
    try
      Inc(FContextCount);
    finally
      SDL_UnlockMutex(FMutex)
    end;

    SDL_SemPost(FSemaphore);
  end
end;

procedure TSDLWindow.RegisterPropEdits;
begin

end;

procedure TSDLWindow.Show;
begin
  Visible := True
end;

procedure TSDLWindow.Hide;
begin
  Visible := False
end;

function TSDLWindow.DesignTime: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

end.

