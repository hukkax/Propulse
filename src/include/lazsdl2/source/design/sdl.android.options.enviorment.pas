unit SDL.Android.Options.Enviorment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, ProjectIntf, Controls, Forms;

type

  ISourceAction = interface;

  { TSourceEntry }

  TSourceEntry = class
  private
    FAction: ISourceAction;
    FFileName: String;
    FSource: TStringList;
    procedure SetAction(AValue: ISourceAction);
    procedure SetFileName(AValue: String);
    procedure SetSource(AValue: TStringList);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Action: ISourceAction read FAction write SetAction;
    property FileName: String read FFileName write SetFileName;
    property Source: TStringList read FSource write SetSource;
  end;

  { ISourceAction }

  ISourceAction = interface
    ['{594C0F48-B054-46BF-9491-19E6773A4F23}']
    procedure Execute(Entry: TSourceEntry);
    function GetPriority: UInt8;
    function GetHidden: Boolean;
  end;

  { TSaveSourceAction }

  TSaveSourceAction = class(TInterfacedObject, ISourceAction)
  public
    procedure Execute(Entry: TSourceEntry);
    function GetPriority: UInt8;
    function GetHidden: Boolean;
  end;

  { TUpdateSourceAction }

  TUpdateSourceAction = class(TInterfacedObject, ISourceAction)
  public
    procedure Execute(Entry: TSourceEntry);
    function GetPriority: UInt8;
    function GetHidden: Boolean;
  end;

  { TDeleteSourceAction }

  TDeleteSourceAction = class(TInterfacedObject, ISourceAction)
  public
    procedure Execute(Entry: TSourceEntry);
    function GetPriority: UInt8;
    function GetHidden: Boolean;
  end;

  { TJavaSources }

  TJavaSources = class
  private const
    cTemplatesDir = 'java_templates';
  private
    FItems: TStringList;
    FInsert, FUpdate, FDelete: ISourceAction;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Load;
    procedure Save;

    procedure FillKeys(List:TStrings);
    procedure FillSources(const Key: String; List:TStrings);

    function AddSource(const Key: String; const FileName: TFilename): Boolean;
    function DeleteSource(const Key: String; const SourceName: String): Boolean;
    function UpdateSource(const Key: String; const SourceName: String; const Data: TStrings): Boolean;
    function ReadSource(const Key: String; const SourceName: String; out Data: TStringList): Boolean;
  end;

  { TAndroidEnviormentOptions }

  TAndroidEnviormentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FAnt: String;
    FSources: TJavaSources;
    FAndroidNDK: String;
    FAndroidSDK: String;
    FTargetMinSDK: String;
    FTargetSDK: String;
    procedure SetAnt(AValue: String);
    procedure SetTargetMinSDK(AValue: String);
    class constructor _Create;
    class destructor _Destroy;
    class var FInstance: TAbstractIDEOptions;
    procedure SetAndroidNDK(AValue: String);
    procedure SetAndroidSDK(AValue: String);
    procedure SetTargetSDK(AValue: String);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadOptions;
    procedure SaveOptions;

    procedure FillTargets(List: TStrings);
    function TargetVersion(const Target: String): String;
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function ExternalOnProjectOpened(Sender: TObject; {%H-}AProject: TLazProject): TModalResult;
    function ExternalOnProjectClosed(Sender: TObject; {%H-}AProject: TLazProject): TModalResult;
    function ExtenalAndroidMacro(const {%H-}Param: string; const {%H-}Data: PtrInt; var Abort: boolean): string;

    property AndroidNDK: String read FAndroidNDK write SetAndroidNDK;
    property AndroidSDK: String read FAndroidSDK write SetAndroidSDK;
    property Sources: TJavaSources read FSources;
    property TargetMinSDK: String read FTargetMinSDK write SetTargetMinSDK;
    property TargetSDK: String read FTargetSDK write SetTargetSDK;
    property Ant: String read FAnt write SetAnt;
  end;

implementation

uses
  IDEExternToolIntf, FileUtil, LazFileUtils, process, IniFiles, LazIDEIntf, MacroIntf, IDEMsgIntf;

{ TDeleteSourceAction }

procedure TDeleteSourceAction.Execute(Entry: TSourceEntry);
begin
  if FileExistsUTF8(Entry.FileName) then
    DeleteFileUTF8(Entry.FileName)
end;

function TDeleteSourceAction.GetPriority: UInt8;
begin
  Result := 1
end;

function TDeleteSourceAction.GetHidden: Boolean;
begin
  Result := False
end;

{ TUpdateSourceAction }

procedure TUpdateSourceAction.Execute(Entry: TSourceEntry);
begin
  if FileExistsUTF8(Entry.FileName) then
    Entry.Source.SaveToFile(Entry.FileName)
end;

function TUpdateSourceAction.GetPriority: UInt8;
begin
  Result := 2
end;

function TUpdateSourceAction.GetHidden: Boolean;
begin
  Result := False
end;

{ TSaveSourceAction }

procedure TSaveSourceAction.Execute(Entry: TSourceEntry);
begin
  ForceDirectoriesUTF8(ExtractFilePath(Entry.FileName));
  Entry.Source.SaveToFile(Entry.FileName)
end;

function TSaveSourceAction.GetPriority: UInt8;
begin
  Result := 3
end;

function TSaveSourceAction.GetHidden: Boolean;
begin
  Result := False
end;

{ TSourceEntry }

procedure TSourceEntry.SetAction(AValue: ISourceAction);
begin
  if FAction=AValue then Exit;
  FAction:=AValue;
end;

procedure TSourceEntry.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TSourceEntry.SetSource(AValue: TStringList);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

procedure TSourceEntry.AfterConstruction;
begin
  inherited AfterConstruction;
  FSource := TStringList.Create;
end;

procedure TSourceEntry.BeforeDestruction;
begin
  FreeAndNil(FSource);
  inherited BeforeDestruction;
end;

{ TJavaSources }

procedure TJavaSources.AfterConstruction;
begin
  inherited AfterConstruction;

  FInsert := TSaveSourceAction.Create;
  FUpdate := TUpdateSourceAction.Create;
  FDelete := TDeleteSourceAction.Create;

  FItems := TStringList.Create;
  FItems.OwnsObjects := True;
end;

procedure TJavaSources.BeforeDestruction;
begin
  FreeAndNil(FItems);
  inherited BeforeDestruction;
end;

procedure TJavaSources.Load;
var
  Templates, Sources: TStringList;
  SourceDir: TFilename;
  Template, Source: String;
  TemplateObj: TStringList;
  SourceObj: TSourceEntry;
begin
  SourceDir := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + cTemplatesDir;
  FItems.Clear;

  Templates := FindAllDirectories(SourceDir + PathDelim, False);
  try
    for Template in Templates do
    begin
      TemplateObj := TStringList.Create;
      TemplateObj.OwnsObjects := True;
      FItems.AddObject(ExtractFileName(Template), TemplateObj);

      Sources := FindAllFiles(AppendPathDelim(Template), '*.java', False);
      try
        for Source in Sources do
        begin
          SourceObj := TSourceEntry.Create;
          SourceObj.FileName := Source;
          SourceObj.Source.LoadFromFile(Source);
          TemplateObj.AddObject(ExtractFileName(Source), SourceObj);
        end;
      finally
        Sources.Free;
      end;
    end;
  finally
    Templates.Free;
  end;
end;

procedure TJavaSources.Save;
var
  i, j: Integer;
  Sources: TStringList;
  Action: ISourceAction;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    Sources := FItems.Objects[i] as TStringList;
    for j := 0 to Sources.Count - 1 do
    begin
      Action := TSourceEntry(Sources.Objects[j]).Action;

      if Assigned(Action) then
        Action.Execute(TSourceEntry(Sources.Objects[j]));
    end;
  end;
end;

procedure TJavaSources.FillKeys(List: TStrings);
var
  i: Integer;
begin
  List.Clear;

  for i := 0 to FItems.Count - 1 do
    List.AddObject(FItems[i], FItems.Objects[i]);
end;

procedure TJavaSources.FillSources(const Key: String; List: TStrings);
var
  Index, i: Integer;
  Sources: TStringList;
begin
  List.Clear;

  Index := FItems.IndexOf(Key);

  if Index > -1 then
  begin
    Sources := FItems.Objects[Index] as TStringList;
    for i := 0 to Sources.Count - 1 do
      List.AddObject(Sources[i], Sources.Objects[i]);
  end;
end;

function TJavaSources.AddSource(const Key: String; const FileName: TFilename): Boolean;
var
  SourceName: String;
  Index: Integer;
  Sources: TStringList;
  Source: TSourceEntry;
begin
  Index := FItems.IndexOf(Key);
  if Index = -1 then
  begin
    Sources := TStringList.Create;
    Sources.OwnsObjects := True;
    FItems.AddObject(Key, Sources);
  end
  else
    Sources := FItems.Objects[Index] as TStringList;

  SourceName := ExtractFileName(FileName);
  Index := Sources.IndexOf(SourceName);

  if Index <> -1 then
  begin
    Source := TSourceEntry.Create;
    Source.FileName := Key + PathDelim + SourceName;
    Source.Source.LoadFromFile(FileName);
    Source.Action := FInsert;
    Sources.AddObject(SourceName, Source);
    Exit(True)
  end
  else
  begin
    Source := Sources.Objects[Index] as TSourceEntry;

    if Source.Action = FDelete then
    begin
      Source.Source.LoadFromFile(FileName);
      Source.Action := FUpdate;
      Exit(True)
    end
  end;

  Result := False
end;

function TJavaSources.DeleteSource(const Key: String; const SourceName: String): Boolean;
var
  Index: Integer;
  Sources: TStringList;
begin
  Index := FItems.IndexOf(Key);

  if Index > -1 then
  begin
    Sources := FItems.Objects[Index] as TStringList;
    Index := Sources.IndexOf(ExtractFileName(SourceName));

    if Index > -1 then
    begin
      TSourceEntry(Sources.Objects[Index]).Action := FDelete;
      Exit(True)
    end
  end;

  Result := False
end;

function TJavaSources.UpdateSource(const Key: String; const SourceName: String; const Data: TStrings): Boolean;
var
  Index: Integer;
  Sources: TStringList;
  Source: TSourceEntry;
begin
  Index := FItems.IndexOf(Key);

  if Index > -1 then
  begin
    Sources := FItems.Objects[Index] as TStringList;
    Index := Sources.IndexOf(ExtractFileName(SourceName));

    if Index > -1 then
    begin
      Source := Sources.Objects[Index] as TSourceEntry;
      Source.Action := FUpdate;
      Source.Source.Assign(Data);
      Exit(True)
    end
  end;

  Result := False
end;

function TJavaSources.ReadSource(const Key: String; const SourceName: String; out Data: TStringList): Boolean;
var
  Index: Integer;
  Sources: TStringList;
  Source: TSourceEntry;
begin
  Index := FItems.IndexOf(Key);
  Data := nil;

  if Index > -1 then
  begin
    Sources := FItems.Objects[Index] as TStringList;
    Index := Sources.IndexOf(ExtractFileName(SourceName));

    if Index > -1 then
    begin
      Source := Sources.Objects[Index] as TSourceEntry;
      Data := Source.Source
    end
  end;

  Result := Assigned(Data)
end;

{ TAndroidEnviormentOptions }

class constructor TAndroidEnviormentOptions._Create;
begin
  FInstance := TAndroidEnviormentOptions.Create
end;

procedure TAndroidEnviormentOptions.SetTargetMinSDK(AValue: String);
begin
  if FTargetMinSDK=AValue then Exit;
  FTargetMinSDK:=AValue;
end;

procedure TAndroidEnviormentOptions.SetAnt(AValue: String);
begin
  if FAnt=AValue then Exit;
  FAnt:=AValue;
end;

procedure TAndroidEnviormentOptions.SetAndroidNDK(AValue: String);
begin
  if FAndroidNDK=AValue then Exit;
  FAndroidNDK:=AValue;
end;

procedure TAndroidEnviormentOptions.SetAndroidSDK(AValue: String);
begin
  if FAndroidSDK=AValue then Exit;
  FAndroidSDK:=AValue;
end;

procedure TAndroidEnviormentOptions.SetTargetSDK(AValue: String);
begin
  if FTargetSDK=AValue then Exit;
  FTargetSDK:=AValue;
end;

procedure TAndroidEnviormentOptions.AfterConstruction;
begin
  inherited AfterConstruction;
  FSources := TJavaSources.Create;
end;

procedure TAndroidEnviormentOptions.BeforeDestruction;
begin
  FreeAndNil(FSources);
  inherited BeforeDestruction;
end;

class destructor TAndroidEnviormentOptions._Destroy;
begin
  FreeAndNil(FInstance)
end;

procedure TAndroidEnviormentOptions.LoadOptions;
var
  Filename: String;
  Options: TIniFile;
begin
  Filename := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'android.options.ini';

  if not FileExistsUTF8(Filename) then
    Exit;

  Options := TIniFile.Create(Filename);
  try
    AndroidSDK := Options.ReadString('Android', 'SDK', '');
    AndroidNDK := Options.ReadString('Android', 'NDK', '');
    TargetMinSDK := Options.ReadString('Android', 'TargetMinSDK', '');
    TargetSDK := Options.ReadString('Android', 'TargetSDK', '');
    Ant := Options.ReadString('Android', 'Ant', '');
  finally
    Options.Free
  end;

  FSources.Load;
end;

procedure TAndroidEnviormentOptions.SaveOptions;
var
  Filename: String;
  Options: TIniFile;
begin
  Filename := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'android.options.ini';

  Options := TIniFile.Create(Filename);
  try
    Options.WriteString('Android', 'SDK', AndroidSDK);
    Options.WriteString('Android', 'NDK', AndroidNDK);
    Options.WriteString('Android', 'TargetMinSDK', TargetMinSDK);
    Options.WriteString('Android', 'TargetSDK', TargetSDK);
    Options.WriteString('Android', 'Ant', Ant);
    Options.UpdateFile;
  finally
    Options.Free
  end;

  FSources.Save;
end;

procedure TAndroidEnviormentOptions.FillTargets(List: TStrings);
var
  i: Integer;
  Platforms: String;
  Targets: TStringList;
begin
  List.Clear;

  if DirectoryExistsUTF8(AndroidNDK) then
  begin
    Platforms := AppendPathDelim(AppendPathDelim(AndroidNDK) + 'platforms');
    Targets := FindAllDirectories(Platforms, False);
    try
      for i := 0 to Targets.Count - 1 do
        List.Add(ExtractRelativepath(Platforms, Targets[i]))
    finally
      Targets.Free
    end
  end;
end;

function TAndroidEnviormentOptions.TargetVersion(const Target: String): String;
var
  TrgNum: Integer;
begin
  TrgNum := StrToIntDef(Copy(Target, Pos('-', Target) + 1, Length(Target)), -1);

  case TrgNum of
    23: Result := 'Android 6.0';
    22: Result := 'Android 5.1';
    21: Result := 'Android 5.0';
    20: Result := 'Android 4.4W';
    19: Result := 'Android 4.4';
    18: Result := 'Android 4.3';
    17: Result := 'Android 4.2, 4.2.2';
    16: Result := 'Android 4.1, 4.1.1';
    15: Result := 'Android 4.0.3, 4.0.4';
    14: Result := 'Android 4.0, 4.0.1, 4.0.2';
    13: Result := 'Android 3.2';
    12: Result := 'Android 3.1.x';
    11: Result := 'Android 3.0.x';
    10: Result := 'Android 2.3.3;Android 2.3.4';
    9: Result := 'Android 2.3, 2.3.1, 2.3.2';
    8: Result := 'Android 2.2.x';
    7: Result := 'Android 2.1.x';
    6: Result := 'Android 2.0.1';
    5: Result := 'Android 2.0';
    4: Result := 'Android 1.6';
    3: Result := 'Android 1.5';
    2: Result := 'Android 1.1';
    1: Result := 'Android 1.0';
    else
      Result := ''
  end;
end;

class function TAndroidEnviormentOptions.GetGroupCaption: string;
begin
  Result := 'SDL2'
end;

class function TAndroidEnviormentOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := FInstance
end;

function TAndroidEnviormentOptions.ExternalOnProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
var
  SourceDir, BaseDir: String;
begin
  Result:=mrOk;
  LoadOptions;

  SourceDir := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + 'java_templates' + PathDelim;
  if not DirectoryExistsUTF8(SourceDir) then
  begin
    ForceDirectoriesUTF8(SourceDir);
    BaseDir := '$PkgDir(LazSDL2Design)java_templates';
    if IDEMacros.SubstituteMacros(BaseDir) then
      CopyDirTree(BaseDir, SourceDir, [cffCreateDestDirectory])
    else
      AddIDEMessage(mluError, 'Could not find package "LazSDL2Design"');

    FSources.Load;
  end;
end;

function TAndroidEnviormentOptions.ExternalOnProjectClosed(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result:=mrOk;
  SaveOptions;
end;

function TAndroidEnviormentOptions.ExtenalAndroidMacro(const Param: string; const Data: PtrInt; var Abort: boolean): string;
begin
  Abort := False;

  if Param = 'Ant' then
    Result := Ant
  else
  if Param = 'SDK' then
    Result := AndroidSDK
  else
  if Param = 'NDK' then
    Result := AndroidNDK
  else
  if Param = 'TargetSDK' then
    Result := TargetSDK
  else
  if Param = 'TargetMinSDK' then
    Result := TargetMinSDK
  else
  if Param = 'Libraries' then
  begin
    if (AndroidNDK <> '') and (TargetSDK <> '') then
      Result :=
        AppendPathDelim(AndroidNDK) + 'platforms' + PathDelim + TargetSDK +
        PathDelim + 'arch-arm' + PathDelim + 'usr' + PathDelim + 'lib'
    else
      Result := ''
  end
  else
  begin
    Abort := True;
    Result := ''
  end
end;

end.

