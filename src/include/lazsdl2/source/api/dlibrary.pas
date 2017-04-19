unit DLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, SDL.Api.Consts,
  lazlogger;

resourcestring
  rsErrMethodNotFound = 'Method "%s" not found!';
  rsParentLibrarNotAssigned = 'Parent library not assigned!';

type

  TLogPriority = (lgUnknown, lgDefault, lgVerbose, lgDebug, lgInfo, lgWarn, lgError, lgFatal, lgSilent);

  ILibLogger = interface
    ['{36DC6E3A-961B-4ED1-A437-716FE6D3928E}']
    procedure Log(Level: TLogPriority; Msg: String);
  end;

  { TNULLLoger }

  TNULLLoger = class(TInterfacedObject, ILibLogger)
  public
    procedure Log(Priority: TLogPriority; LogMsg: String); virtual;
    procedure LogLn(Priority: TLogPriority; LogMsg: String); virtual;
  end;

  { TMethodList }

  TMethodList = class
  private type

    { TMethodDefinition }

    PMethodDefinition = ^TMethodDefinition;
    TMethodDefinition = record
      Name: String;
      Method: PPointer;
      Mandatory: Boolean;
    end;

  private
    FData: array of PMethodDefinition;
    FCount: Integer;
    function GetItem(Index: Integer): PMethodDefinition;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Add(const Name: String; Method: PPointer; Mandatory: Boolean = True);
    procedure Clear;

    property Count: Integer read FCount;
    property Item[Index: Integer]: PMethodDefinition read GetItem; default;
  end;

  { TLibraryLoader }

  TLibraryLoader = class
  private
    class constructor _Create;
    class destructor _Destroy;
  public
    class var SearchPaths: TStringList;

    class function LoadLibrary(const Libname: String; out Handle: TLibHandle): Boolean;
  end;

  { TAbstractLibrary }

  TAbstractLibrary = class abstract(TPersistent, IFPObserver)
  private
    FLastError: String;
    FLogger: ILibLogger;
    FVersion: String;
    FValid: Boolean;
    function GetLastError: String;
    procedure SetLastError(const AValue: String);
    procedure SetLogger(AValue: ILibLogger);
  protected
    procedure DoGetFileVersion(out VersionStr: String); virtual;
    procedure DoGetLastError(out ErrorStr: String); virtual;
    procedure DoInit; virtual; abstract;
    procedure DoSetLastError(const ErrorStr: String); virtual;
    procedure GetRequiredMethods(const List: TMethodList); virtual;
    procedure Invalid;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  public
    procedure Init;

    function GetLibProcAddress(const ProcName : AnsiString): Pointer; virtual; abstract;
    function Valid: Boolean; virtual;

    property LastError: String read GetLastError write SetLastError;
    property Logger: ILibLogger read FLogger write SetLogger;
    property Version: String read FVersion;
  end;
  TAbstractLibraryClass = class of TAbstractLibrary;

  { TLibrary }

  TLibrary = class(TAbstractLibrary)
  private
    FFileName: String;
    FHandle: TLibHandle;
  protected
    procedure DoInit; override;
  public
    constructor Create(const LibraryFile: String); virtual; reintroduce;
    procedure BeforeDestruction; override;

    function GetLibProcAddress(const ProcName : AnsiString): Pointer; override;
    function Valid: Boolean; override;

    property FileName: String read FFileName write FFileName;
    property Handle: TLibHandle read FHandle;
  end;

  { TSubLibrary }

  TSubLibrary = class(TAbstractLibrary)
  private
    FOwner: TAbstractLibrary;
  protected
    procedure DoInit; override;
  public
    constructor Create(const AOwner: TAbstractLibrary); reintroduce;

    function GetLibProcAddress(const ProcName : AnsiString): Pointer; override;
    function Valid: Boolean; override;

    property Owner: TAbstractLibrary read FOwner;
  end;
  TSubLibraryClass = class of TSubLibrary;

  { TMultiPartLibrary }

  TMultiPartLibrary = class(TAbstractLibrary, IFPObserved)
  private
    FLibraries: array of TAbstractLibrary;
    FCount: Integer;
  protected
    procedure DoInit; override;
  public
    procedure AfterConstruction; override;

    procedure AddLib(const LibFileName: String);
    procedure AddLib(const Lib: TAbstractLibrary);

    function GetLibProcAddress(const ProcName : AnsiString): Pointer; override;
    function Valid: Boolean; override;
  end;

implementation

uses
  LazFileUtils;

{ TLibraryLoader }

class constructor TLibraryLoader._Create;
begin
  SearchPaths := TStringList.Create;
  SearchPaths.Add('lib');
  SearchPaths.Add('lib' + PathDelim + SDLConsts.CPU + '-' + SDLConsts.OS);
end;

class destructor TLibraryLoader._Destroy;
begin
  FreeAndNil(SearchPaths)
end;

class function TLibraryLoader.LoadLibrary(const Libname: String; out Handle: TLibHandle): Boolean;
var
  Path: String;
begin
  Handle := 0;

  //Fist try to find library in search paths
  for Path in SearchPaths do
  begin
    if FileExistsUTF8(AppendPathDelim(Path) + SDLUtils.GetLibName(Libname, SDLConsts.OS)) then
      Handle := dynlibs.LoadLibrary(AppendPathDelim(Path) + SDLUtils.GetLibName(Libname, SDLConsts.OS));

    if Handle <> 0 then
      Exit(True);
  end;

  //Last chance try to load library from system folders
  Handle := dynlibs.LoadLibrary(Libname);
  Result := Handle <> 0
end;

{ TMultiPartLibrary }

procedure TMultiPartLibrary.DoInit;
begin
end;

procedure TMultiPartLibrary.AfterConstruction;
begin
  inherited AfterConstruction;

  FCount := 0;
  SetLength(FLibraries, 10)
end;

procedure TMultiPartLibrary.AddLib(const LibFileName: String);
begin
  FLibraries[FCount] := TLibrary.Create(LibFileName);
  FLibraries[FCount].Init;
  FLibraries[FCount].FPOAttachObserver(Self);
  Inc(FCount)
end;

procedure TMultiPartLibrary.AddLib(const Lib: TAbstractLibrary);
begin
  FLibraries[FCount] := Lib;
  Inc(FCount)
end;

function TMultiPartLibrary.GetLibProcAddress(const ProcName: AnsiString): Pointer;
var
  i: Integer;
begin
  for i := 0 to FCount -1 do
  begin
    Result := FLibraries[i].GetLibProcAddress(ProcName);
    if Assigned(Result) then
      Exit;
  end;

  Result := nil
end;

function TMultiPartLibrary.Valid: Boolean;
var
  i: Integer;
begin
  for i := 0 to FCount -1 do
  begin
    if not FLibraries[i].Valid then
      Exit(False);
  end;

  Result := inherited Valid;
end;

{ TNULLLoger }

procedure TNULLLoger.Log(Priority: TLogPriority; LogMsg: String);
begin
	{$IFDEF DEBUG}
	DebugLn(LogMsg);
	writeln(LogMsg);
	{$ELSE}
	if Priority in [lgWarn, lgError, lgFatal] then
		DebugLn(LogMsg);
	{$ENDIF}
end;

procedure TNULLLoger.LogLn(Priority: TLogPriority; LogMsg: String);
begin
	Log(Priority, LogMsg);
end;

{ TSubLibrary }

procedure TSubLibrary.DoInit;
begin
  if not Assigned(FOwner) then
  begin
    Invalid;
    LastError := rsParentLibrarNotAssigned;
    Logger.Log(lgWarn, ClassName + ': Owner not assigned');
  end
  else
  if not Owner.Valid then
  begin
    Invalid;
    LastError := Format('Owner "%s" not valid', [FOwner.ClassName]);
    Logger.Log(lgWarn, ClassName + LastError);
  end
end;

constructor TSubLibrary.Create(const AOwner: TAbstractLibrary);
begin
  FOwner := AOwner;
end;

function TSubLibrary.GetLibProcAddress(const ProcName: AnsiString): Pointer;
begin
  Result := FOwner.GetLibProcAddress(ProcName)
end;

function TSubLibrary.Valid: Boolean;
begin
  Result := inherited Valid and FOwner.Valid
end;

{ TLibrary }

procedure TLibrary.DoInit;
{$IFDEF WINDOWS}
var
  OldDir: String;
{$ENDIF}
begin
  //We change current directory because SDL_Image and SDL_Mixer neads to load
  //some additional libraryes witch moust likely will be located in same folder
  {$IFDEF WINDOWS}
  OldDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FFileName));
  try
  {$ENDIF}
    if not TLibraryLoader.LoadLibrary(FFileName, FHandle) then
    begin
      LastError := Format('Could not load "%s" library', [FFileName]);
      Logger.Log(lgError, ClassName + ': ' + LastError);
      Invalid;
    end
    else
    begin
      Logger.Log(lgVerbose, ClassName + Format(': Loaded "%s"', [FFileName]));
      Logger.Log(lgVerbose, ClassName + '.Handle: ' + IntToStr(FHandle));
    end;
  {$IFDEF WINDOWS}
  finally
    SetCurrentDirUTF8(OldDir);
  end;
  {$ENDIF}
end;

constructor TLibrary.Create(const LibraryFile: String);
begin
  FFileName := LibraryFile;
end;

procedure TLibrary.BeforeDestruction;
begin
  if FHandle > 0 then
    UnloadLibrary(FHandle);
  inherited BeforeDestruction;
end;

function TLibrary.Valid: Boolean;
begin
  Result:= inherited Valid and (FHandle <> 0)
end;

function TLibrary.GetLibProcAddress(const ProcName: AnsiString): Pointer;
begin
  Result := GetProcAddress(FHandle, ProcName);
  //Assert(Assigned(Result), Format('Method "%s" not found!', [ProcName]));
end;

{ TMethodList }

function TMethodList.GetItem(Index: Integer): PMethodDefinition;
begin
  Result := FData[Index]
end;

procedure TMethodList.AfterConstruction;
begin
  inherited AfterConstruction;
  SetLength(FData, 10);
  FCount := 0;
end;

procedure TMethodList.BeforeDestruction;
var
  i: Integer;
begin
  inherited BeforeDestruction;

  for i := 0 to FCount - 1 do
    Dispose(FData[i]);
  FData := nil;
end;

procedure TMethodList.Add(const Name: String; Method: PPointer;
  Mandatory: Boolean);
var
  NewItem: PMethodDefinition;
begin
  if FCount = Length(FData) then
    SetLength(FData, FCount + FCount div 4);

  New(NewItem);
  NewItem^.Name := Name;
  NewItem^.Method := Method;
  NewItem^.Mandatory := Mandatory;
  FData[FCount] := NewItem;
  Inc(FCount);
end;

procedure TMethodList.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    Dispose(FData[i]);

  FCount := 0;
  SetLength(FData, 10);
end;

{ TAbstractLibrary }

function TAbstractLibrary.GetLastError: String;
begin
  DoGetLastError(Result);
end;

procedure TAbstractLibrary.SetLastError(const AValue: String);
begin
  DoSetLastError(AValue);
end;

procedure TAbstractLibrary.SetLogger(AValue: ILibLogger);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

procedure TAbstractLibrary.DoGetFileVersion(out VersionStr: String);
begin
  VersionStr := '0.0.0.0'
end;

procedure TAbstractLibrary.DoGetLastError(out ErrorStr: String);
begin
  ErrorStr := Trim(FLastError)
end;

procedure TAbstractLibrary.DoSetLastError(const ErrorStr: String);
begin
  FLastError := ErrorStr
end;

procedure TAbstractLibrary.GetRequiredMethods(const List: TMethodList);
begin
  List.Clear;
end;

procedure TAbstractLibrary.Invalid;
begin
  FValid := False
end;

procedure TAbstractLibrary.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation = ooFree then
    Free;
end;

procedure TAbstractLibrary.Init;
var
  Required: TMethodList;
  Method: Pointer;
  i: Integer;
begin
  if not Assigned(FLogger) then
    FLogger := TNULLLoger.Create;

  Logger.Log(lgVerbose, ClassName + ': Init started');
  FVersion := '0.0.0.0';

  FValid := True;
  DoInit;

  if Valid then
  begin
    Logger.Log(lgVerbose, ClassName + ': Loading required methods');
    Required := TMethodList.Create;
    try
      GetRequiredMethods(Required);
      Logger.Log(lgVerbose, ClassName + '.MethodCount: ' + IntToStr(Required.Count));
      for i := 0 to Required.Count - 1 do
      begin
        Method := GetLibProcAddress(Required[i]^.Name);
        if Assigned(Method) then
        begin
          if Assigned(Required[i]^.Method) then
            Required[i]^.Method^ := Method;
        end
        else
        begin
          if Required[i]^.Mandatory then
            Invalid;

          Logger.Log(lgError, ClassName + Format(': Method "%s" not found.', [Required[i]^.Name]));
          LastError := LastError + Format(rsErrMethodNotFound, [Required[i]^.Name]) + LineEnding;
        end
      end
    finally
      Required.Free
    end;

    DoGetFileVersion(FVersion);
  end;
  Logger.Log(lgVerbose, ClassName + Format(': Init finished(%s)', [BoolToStr(Valid, 'Ok', 'Fail')]));
end;

function TAbstractLibrary.Valid: Boolean;
begin
  Result := FValid
end;

end.

