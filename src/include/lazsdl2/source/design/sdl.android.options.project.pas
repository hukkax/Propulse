unit SDL.Android.Options.Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, ProjectIntf, forms, Controls;

type

  { TAndroidProjectOptions }

  TAndroidProjectOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FAllPermissions: TStringList;
    FAndroidLibs: TStringList;
    FMainActivity: String;
    FNeedLoad: Boolean;
    FOpenGLES: String;
    FPackageName: String;
    FPermissions: TStringList;
    FProjectName: String;
    FScreensAnyDensity: Boolean;
    FScreensLarge: Boolean;
    FScreensNormal: Boolean;
    FScreensSmall: Boolean;
    FScreensxLarge: Boolean;
    FTargetMinSDK: String;
    FTargetSDK: String;
    procedure SetMainActivity(AValue: String);
    procedure SetNeedLoad(AValue: Boolean);
    procedure SetOpenGLES(AValue: String);
    procedure SetProjectName(AValue: String);
    procedure SetScreensAnyDensity(AValue: Boolean);
    procedure SetScreensLarge(AValue: Boolean);
    procedure SetScreensNormal(AValue: Boolean);
    procedure SetScreensSmall(AValue: Boolean);
    procedure SetScreensxLarge(AValue: Boolean);
    procedure SetTargetMinSDK(AValue: String);
    procedure SetTargetSDK(AValue: String);
    class destructor _Destroy;
    class var FInstance: TAndroidProjectOptions;
    procedure SetPackageName(AValue: String);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure LoadOptions;
    procedure SaveOptions;
    procedure Clear;

    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;

    function ExternalOnProjectOpened(Sender: TObject; {%H-}AProject: TLazProject): TModalResult;
    function ExternalOnProjectClosed(Sender: TObject; {%H-}AProject: TLazProject): TModalResult;
    function ExternalOnSaveAll(Sender: TObject): TModalResult;
    procedure ExternalAfterBuild(Sender: TObject; BuildSuccessful: Boolean);
    function ExtenalAndroidMacro(const {%H-}Param: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function ExtenalLibraryMacro(const {%H-}Param: string; const {%H-}Data: PtrInt; var Abort: boolean): string;
    function ExtenalTargetDirMacro(const {%H-}Param: string; const {%H-}Data: PtrInt; var Abort: boolean): string;

    property AllPermissions: TStringList read FAllPermissions;
    property AndroidLibs: TStringList read FAndroidLibs;
    property NeedLoad: Boolean read FNeedLoad write SetNeedLoad;
    property Permissions: TStringList read FPermissions;
    property TargetMinSDK: String read FTargetMinSDK write SetTargetMinSDK;
    property TargetSDK: String read FTargetSDK write SetTargetSDK;
    property ProjectName: String read FProjectName write SetProjectName;
  published
    property MainActivity: String read FMainActivity write SetMainActivity;
    property OpenGLES: String read FOpenGLES write SetOpenGLES;
    property PackageName: String read FPackageName write SetPackageName;
    property ScreensAnyDensity: Boolean read FScreensAnyDensity write SetScreensAnyDensity;
    property ScreensLarge: Boolean read FScreensLarge write SetScreensLarge;
    property ScreensNormal: Boolean read FScreensNormal write SetScreensNormal;
    property ScreensSmall: Boolean read FScreensSmall write SetScreensSmall;
    property ScreensxLarge: Boolean read FScreensxLarge write SetScreensxLarge;
  end;

implementation

uses
  MacroIntf, LazIDEIntf, FileUtil, LazFileUtils, process, IniFiles, DOM, XMLRead, SDL.Api.Consts,
  SDL.Android.ProjectBuilder, LCLProc, IDEExternToolIntf, SDL.ProjectBuilder, typinfo, Variants;

{ TAndroidProjectOptions }

class destructor TAndroidProjectOptions._Destroy;
begin
  FreeAndNil(FInstance)
end;

procedure TAndroidProjectOptions.SetOpenGLES(AValue: String);
begin
  if FOpenGLES=AValue then Exit;
  FOpenGLES:=AValue;
end;

procedure TAndroidProjectOptions.SetProjectName(AValue: String);
begin
  if FProjectName=AValue then Exit;
  FProjectName:=AValue;
end;

procedure TAndroidProjectOptions.SetScreensAnyDensity(AValue: Boolean);
begin
  if FScreensAnyDensity=AValue then Exit;
  FScreensAnyDensity:=AValue;
end;

procedure TAndroidProjectOptions.SetScreensLarge(AValue: Boolean);
begin
  if FScreensLarge=AValue then Exit;
  FScreensLarge:=AValue;
end;

procedure TAndroidProjectOptions.SetScreensNormal(AValue: Boolean);
begin
  if FScreensNormal=AValue then Exit;
  FScreensNormal:=AValue;
end;

procedure TAndroidProjectOptions.SetScreensSmall(AValue: Boolean);
begin
  if FScreensSmall=AValue then Exit;
  FScreensSmall:=AValue;
end;

procedure TAndroidProjectOptions.SetScreensxLarge(AValue: Boolean);
begin
  if FScreensxLarge=AValue then Exit;
  FScreensxLarge:=AValue;
end;

procedure TAndroidProjectOptions.SetTargetMinSDK(AValue: String);
begin
  if FTargetMinSDK=AValue then Exit;
  FTargetMinSDK:=AValue;
end;

procedure TAndroidProjectOptions.SetTargetSDK(AValue: String);
begin
  if FTargetSDK=AValue then Exit;
  FTargetSDK:=AValue;
end;

procedure TAndroidProjectOptions.SetMainActivity(AValue: String);
begin
  if FMainActivity=AValue then Exit;
  FMainActivity:=AValue;
end;

procedure TAndroidProjectOptions.SetNeedLoad(AValue: Boolean);
begin
  if FNeedLoad=AValue then Exit;
  FNeedLoad:=AValue;
end;

procedure TAndroidProjectOptions.SetPackageName(AValue: String);
begin
  if FPackageName=AValue then Exit;
  FPackageName:=AValue;
end;

procedure TAndroidProjectOptions.AfterConstruction;
begin
  inherited AfterConstruction;
  FNeedLoad := True;
  FAndroidLibs := TStringList.Create;
  FPermissions := TStringList.Create;
  FAllPermissions := TStringList.Create;
end;

procedure TAndroidProjectOptions.BeforeDestruction;
begin
  FreeAndNil(FAndroidLibs);
  FreeAndNil(FPermissions);
  FreeAndNil(FAllPermissions);
  inherited BeforeDestruction;
end;

procedure TAndroidProjectOptions.LoadOptions;
var
  Filename: String;
  Options: TIniFile;
begin
  if not NeedLoad then
    Exit;

  Clear;

  Filename := '$(ProjFile)';
  if not IDEMacros.SubstituteMacros(Filename) then
    Exit;

  if not FileExistsUTF8(Filename) then
    Exit;

  Filename := ChangeFileExt(Filename, '.android.ini');
  DebugLn('Project file(Load): ' + Filename);

  if not FileExistsUTF8(Filename) then
    Exit;

  Options := TIniFile.Create(Filename);
  try
    AndroidLibs.DelimitedText := Options.ReadString('Android', 'Libs', '');
    MainActivity := Options.ReadString('Android', 'Main', '');
    OpenGLES := Options.ReadString('Android', 'OpenGLES', '1.0');
    PackageName := Options.ReadString('Android', 'PackageName', '');
    Permissions.DelimitedText := Options.ReadString('Android', 'Permissions', '');
    ScreensAnyDensity := Options.ReadBool('Android', 'Density-AnyDensity', True);
    ScreensSmall := Options.ReadBool('Android', 'Screens-Small', True);
    ScreensNormal := Options.ReadBool('Android', 'Screens-Normal', True);
    ScreensLarge := Options.ReadBool('Android', 'Screens-Large', True);
    ScreensxLarge := Options.ReadBool('Android', 'Screens-xLarge', False);
    ProjectName := Options.ReadString('Android', 'ProjectName', '');
    TargetMinSDK := Options.ReadString('Android', 'TargetMinSDK', '');
    TargetSDK := Options.ReadString('Android', 'TargetSDK', '');
  finally
    Options.Free
  end;
end;

procedure TAndroidProjectOptions.SaveOptions;
var
  Filename: String;
  Options: TIniFile;
begin
  if not NeedLoad then
    Exit;

  Filename := '$(ProjFile)';
  if not IDEMacros.SubstituteMacros(Filename) then
    Exit;

  if not FileExistsUTF8(Filename) then
    Exit;

  Filename := ChangeFileExt(Filename, '.android.ini');
  DebugLn('Project file(Save): ' + Filename);

  Options := TIniFile.Create(Filename);
  try
    Options.WriteString('Android', 'Libs', AndroidLibs.DelimitedText);
    Options.WriteString('Android', 'Main', MainActivity);
    Options.WriteString('Android', 'OpenGLES', OpenGLES);
    Options.WriteString('Android', 'PackageName', PackageName);
    Options.WriteString('Android', 'Permissions', Permissions.DelimitedText);
    Options.WriteBool('Android', 'Density-AnyDensity', ScreensAnyDensity);
    Options.WriteBool('Android', 'Density-Small', ScreensSmall);
    Options.WriteBool('Android', 'Density-Normal', ScreensNormal);
    Options.WriteBool('Android', 'Density-xLarge', ScreensxLarge);
    Options.WriteString('Android', 'ProjectName', ProjectName);
    Options.WriteString('Android', 'TargetMinSDK', TargetMinSDK);
    Options.WriteString('Android', 'TargetSDK', TargetSDK);
    Options.UpdateFile;
  finally
    Options.Free
  end;
end;

procedure TAndroidProjectOptions.Clear;
begin
  AllPermissions.Clear;
  AndroidLibs.Clear;
  MainActivity := '';
  OpenGLES := '';
  PackageName := '';
  Permissions.Clear;
  ScreensAnyDensity := True;
  ScreensLarge := True;
  ScreensSmall := True;
  ScreensxLarge := True;
  TargetMinSDK := '';
  TargetSDK := '';
end;

class function TAndroidProjectOptions.GetGroupCaption: string;
begin
  Result := 'SDL2 (Project)'
end;

class function TAndroidProjectOptions.GetInstance: TAbstractIDEOptions;
begin
  if not Assigned(FInstance) then
    FInstance := TAndroidProjectOptions.Create;

  Result := FInstance
end;

function TAndroidProjectOptions.ExternalOnProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
var
  XmlPermissions: TXMLDocument;
  XmlPermission: TDOMElement;
  PermissionFile: String;
  i: Integer;
begin
  DebugLn('-->ExternalOnProjectOpened()');
  Result:=mrOk;
  LoadOptions;

  PermissionFile := '$PkgDir(LazSDL2Design)android_permissions.xml';
  if IDEMacros.SubstituteMacros(PermissionFile) then
  begin
    if FileExistsUTF8(PermissionFile) then
    begin
      ReadXMLFile(XmlPermissions, PermissionFile);
      for i := 0 to XmlPermissions.FirstChild.ChildNodes.Count - 1 do
      begin
        XmlPermission := XmlPermissions.FirstChild.ChildNodes[i] as TDOMElement;
        FAllPermissions.Values[String(XmlPermission.AttribStrings['Name'])] := String(XmlPermission.AttribStrings['Description']);
      end
    end
    else
      DebugLn('File not found: ' + PermissionFile);
  end
  else
    DebugLn('android_permissions.xml file not found. Imposible to load permissions.');

  if PackageName = '' then
    PackageName := 'com.pascal.' + ExtractFileNameOnly(LazarusIDE.ActiveProject.MainFile.GetFullFilename);

  if OpenGLES = '' then
    OpenGLES := '1.0';

  DebugLn('<--ExternalOnProjectOpened()');
end;

function TAndroidProjectOptions.ExternalOnProjectClosed(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result:=mrOk;
  SaveOptions;
end;

function TAndroidProjectOptions.ExternalOnSaveAll(Sender: TObject
  ): TModalResult;
begin
  Result := mrOK;
  with TAndroidProjectOptions.GetInstance as TAndroidProjectOptions do
    NeedLoad := True;
end;

procedure TAndroidProjectOptions.ExternalAfterBuild(Sender: TObject; BuildSuccessful: Boolean);
var
  TargetOS: String;
begin
  if BuildSuccessful then
  begin
    TargetOS := '$(TargetOS)';
    if IDEMacros.SubstituteMacros(TargetOS) then
    begin
      if (TargetOS = 'android') and (LazarusIDE.ActiveProject.MainFile.Filename <> '') then
      begin
        with TAndroidProjectBuilder.Create do
        try
          Build
        finally
          Free
        end;
      end
      else
      begin
        with TSDLDesktopProjectBuilder.Create do
        try
          Build
        finally
          Free
        end
      end
    end
  end
end;

function TAndroidProjectOptions.ExtenalAndroidMacro(const Param: string; const Data: PtrInt; var Abort: boolean): string;
begin
  Abort := False;

  if IsPublishedProp(Self, Param) then
  begin
    case GetPropInfo(Self, Param)^.PropType^.Kind of
      tkBool: Result := BoolToStr(Boolean(GetOrdProp(Self, Param)), 'true', 'false');
      else
        Result := VarToStr(GetPropValue(Self, Param, True))
    end;
  end
  else
  if Param = 'ProjectName' then
  begin
    if ProjectName <> '' then
      Result := ProjectName
    else
    begin
      Result := '$(TargetFile)';
      if not IDEMacros.SubstituteMacros(Result) then
        Result := ''
    end
  end
  else
  if Param = 'TargetMinSDK' then
  begin
    if TargetMinSDK <> '' then
      Result := TargetMinSDK
    else
    begin
      Result := '$Android(TargetMinSDK)';
      if not IDEMacros.SubstituteMacros(Result) then
        Result := '';
    end
  end
  else
  if Param = 'TargetSDK' then
  begin
    if TargetSDK <> '' then
      Result := TargetSDK
    else
    begin
      Result := '$Android(TargetSDK)';
      if not IDEMacros.SubstituteMacros(Result) then
        Result := '';
    end
  end
  else
  if Param = 'Permissions' then
    Result := Permissions.DelimitedText
  else
  if Param = 'Libraries' then
    Result := AndroidLibs.DelimitedText
  else
  begin
    Abort := True;
    Result := ''
  end
end;

function TAndroidProjectOptions.ExtenalLibraryMacro(const Param: string; const Data: PtrInt; var Abort: boolean): string;
var
  TargetOS: String;
begin
  TargetOS := '$(TargetOS)';

  if IDEMacros.SubstituteMacros(TargetOS) then
    Result := SDLUtils.GetLibName(Param, TargetOS)
  else
    Result := Param
end;

function TAndroidProjectOptions.ExtenalTargetDirMacro(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result := '$(TargetFile)';
  if not IDEMacros.SubstituteMacros(Result) then
    Exit('');

  Result := AppendPathDelim(ExtractFilePath(Result))
end;

end.

