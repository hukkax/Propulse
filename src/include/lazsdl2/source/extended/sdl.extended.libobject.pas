unit SDL.Extended.LibObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Extended.LibraryProvider, SDL.Extended.Interfaces, DLibrary,
  SDL.Extended.Window;

type

  { TSDLObject }

  TSDLObject = class(TComponent, IFPObserver)
  private
    FProvider: TSDLCustomLibraryProvider;
    function IsStoredProvider: Boolean;
    procedure SetProvider(AValue: TSDLCustomLibraryProvider);
    procedure InternalRaiseProviderException;
    procedure InternalReload;
  protected
    procedure DoReload; virtual; abstract;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; {%H-}Data: Pointer); virtual;
    procedure InitLibrary(const Lib: TAbstractLibrary);
    procedure RaiseLibraryException(const Lib: TAbstractLibrary);
    procedure RaiseProviderException;
    procedure RaiseSDLException;
    procedure RaiseOpenGLException;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Provider: TSDLCustomLibraryProvider read FProvider write SetProvider stored IsStoredProvider; (** Library methods provider *)
  end;

  { TSDLWindowedObject }

  TSDLWindowedObject = class(TSDLObject)
  private
    FWindow: TSDLWindow;
    function IsStoredWnd: Boolean;
    procedure SetWindow(AValue: TSDLWindow);
  protected
    procedure RaiseWindowException;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Window: TSDLWindow read FWindow write SetWindow;
  end;

  { TSDLLibProxyLogger }

  TSDLLibProxyLogger = class(TInterfacedObject, ILibLogger)
  private
    FLogger: ILogger;
  public
    constructor Create(Logger: ILogger); reintroduce;

    procedure Log(Level: TLogPriority; Msg: String);
  end;

implementation

uses
  SDL.Api.Types, SDL.Extended.Strings, SDL.Api.Consts, SDL.Extended.Types;

{ TSDLLibProxyLogger }

constructor TSDLLibProxyLogger.Create(Logger: ILogger);
begin
  FLogger := Logger
end;

procedure TSDLLibProxyLogger.Log(Level: TLogPriority; Msg: String);
begin
  if Assigned(FLogger) then
  begin
    case Level of
      lgUnknown: FLogger.Log(logUnknown, Msg);
      lgDefault: FLogger.Log(logDefault, Msg);
      lgVerbose: FLogger.Log(logVerbose, Msg);
      lgDebug: FLogger.Log(logDebug, Msg);
      lgInfo: FLogger.Log(logInfo, Msg);
      lgWarn: FLogger.Log(logWarning, Msg);
      lgError: FLogger.Log(logError, Msg);
      lgFatal: FLogger.Log(logFatal, Msg);
      lgSilent: FLogger.Log(logSilent, Msg);
    end
  end
end;

{ TSDLWindowedObject }

procedure TSDLWindowedObject.SetWindow(AValue: TSDLWindow);
var
  NewValue: TSDLWindow;
begin
  if FWindow=AValue then
    Exit;

  if not Assigned(AValue) and (Owner is TSDLWindow) then
    NewValue := Owner as TSDLWindow
  else
    NewValue := AValue;

  if Assigned(FWindow) then
    FWindow.FPODetachObserver(Self);

  if Assigned(NewValue) then
    NewValue.FPOAttachObserver(Self);

  FWindow:=NewValue;
end;

procedure TSDLWindowedObject.AfterConstruction;
begin
  inherited AfterConstruction;

  if not Assigned(Window) and (Owner is TSDLWindow) then
    Window := Owner as TSDLWindow;
end;

procedure TSDLWindowedObject.BeforeDestruction;
begin
  Window := nil;

  inherited BeforeDestruction;
end;

function TSDLWindowedObject.IsStoredWnd: Boolean;
begin
  Result := Owner <> FWindow
end;

procedure TSDLWindowedObject.RaiseWindowException;
begin
  if not Assigned(FWindow) then
    raise SDLWindowException.Create(rsWindowNotAssigned);
end;

{ TSDLObject }

function TSDLObject.IsStoredProvider: Boolean;
begin
  if Owner is IInternalProviderContainer then
    Result := FProvider <> (Owner as IInternalProviderContainer).GetInternalProvider
  else
    Result := True
end;

procedure TSDLObject.SetProvider(AValue: TSDLCustomLibraryProvider);
var
  NewProvider: TSDLCustomLibraryProvider;
begin
  if FProvider=AValue then
    Exit;

  if (Owner is IInternalProviderContainer) and not Assigned(AValue) then
    NewProvider := (Owner as IInternalProviderContainer).GetInternalProvider as TSDLCustomLibraryProvider
  else
    NewProvider := AValue;

  if Assigned(FProvider) then
    FProvider.FPODetachObserver(Self);

  if Assigned(NewProvider) then
    NewProvider.FPOAttachObserver(Self);

  FProvider:=NewProvider;
  FPOObservedChanged(NewProvider, ooChange, nil)
end;

procedure TSDLObject.InternalRaiseProviderException;
begin
  if not Assigned(FProvider) then
    raise SDLProviderException.Create(rsProviderNotSet)
  else
  if not FProvider.Active then
    raise SDLProviderException.Create(rsProviderNotActive)
  else
  if FProvider.Libs[LIB_SDL] = nil then
    raise SDLProviderException.Create(rsSDL2LibraryNotLoaded)
end;

procedure TSDLObject.InternalReload;
begin
  if not (csDesigning in ComponentState) then
    DoReload
end;

procedure TSDLObject.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if FProvider = ASender then
  begin
    if Operation = ooChange then
      InternalReload
    else
    if Operation = ooFree then
    begin
      FProvider := nil;
      InternalReload
    end
  end
end;

procedure TSDLObject.InitLibrary(const Lib: TAbstractLibrary);
begin
  Lib.Init;
  if not Lib.Valid then
    RaiseLibraryException(Lib);
end;

procedure TSDLObject.RaiseProviderException;
begin
  InternalRaiseProviderException;

  raise SDLProviderException.Create(''); //I do not see any problem but we need to raise something
end;

procedure TSDLObject.RaiseSDLException;
begin
  //First check if provider is valid
  InternalRaiseProviderException;

  raise SDLException.Create(Provider.Libs[LIB_SDL].LastError);
end;

procedure TSDLObject.RaiseOpenGLException;
begin
  if not Assigned(Provider.Libs[LIB_OPENGL]) then
    raise SDLException.Create(rsErrOpenGLLibraryNotFound);
end;

procedure TSDLObject.RaiseLibraryException(const Lib: TAbstractLibrary);
var
  Err: String;
begin
  Err := Lib.LastError;

  if Err <> '' then
    raise SDLException.Create(Err);

  RaiseSDLException;
end;

procedure TSDLObject.AfterConstruction;
begin
  inherited AfterConstruction;

  if (Owner is IInternalProviderContainer) and not Assigned(Provider) then
    Provider := (Owner as IInternalProviderContainer).GetInternalProvider as TSDLCustomLibraryProvider;
end;

procedure TSDLObject.BeforeDestruction;
begin
  Provider := nil;

  inherited BeforeDestruction;
end;

end.

