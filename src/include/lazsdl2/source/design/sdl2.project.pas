unit SDL2.Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, ProjectIntf;

type

  { TSDL2ProjectDescriptor }

  TSDL2ProjectDescriptor = class(TProjectDescriptor)
  private
    FIncludeLibs: Boolean;
  public
    constructor create; override;

    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles(AProject: TLazProject) : TModalResult; override;
  end;

implementation

uses
  LazIDEIntf, SDL2.DesignStrings, SDL2.NewProject, MacroIntf, SDL.Android.Options.Project,
  LCLProc;

{ TSDL2ProjectDescriptor }

constructor TSDL2ProjectDescriptor.create;
begin
  inherited create;
  Flags := Flags - [pfMainUnitHasCreateFormStatements] + [pfRunnable];
  Name := SDL2Application;
end;

function TSDL2ProjectDescriptor.GetLocalizedName: string;
begin
  Result := SDLProject
end;

function TSDL2ProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SDLProjectDescription
end;

function TSDL2ProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  PFile : TLazProjectFile;
  PFileName: String;
  Source: TStringList;
  AppClassName, AppTitle: String;
begin
  DebugLn('-->InitProject()');
  Result:=inherited InitProject(AProject);

  if Result <> mrOK then
    Exit;

  with TfrmNewProject.Create(nil) do
  try
    if ShowModal <> mrOK then
      Exit;

    FIncludeLibs := chbIncludeLibs.Checked;
    AppClassName := edtClassName.Text;
    AppTitle := edtTitle.Text;
  finally
    Free
  end;

  PFileName := 'SDP2Project1';

  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LazSDL2Extensions');
  AProject.AddPackageDependency('LazSDL2');
  AProject.Title := SDL2Application;
  AProject.LazCompilerOptions.Win32GraphicApp:=False;
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:= 'dist' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim + LowerCase(PFileName);
  AProject.LazCompilerOptions.Libraries := '$Android(Libraries)';
  AProject.Flags:=AProject.Flags+[pfMainUnitHasTitleStatement];
  AProject.Title:=AppTitle;

  {$REGION 'Project file'}
  PFile := AProject.CreateProjectFile(LowerCase(PFileName + '.lpr'));
  PFile.IsPartOfProject := True;
  AProject.AddFile(PFile, False);
  AProject.MainFileID:=0;

  Source := TStringList.Create;
  try
    Source.Add('{$IFDEF ANDROID} library {$ELSE} program {$ENDIF} ' + LowerCase(PFileName) + ';');
    Source.Add('');
    Source.Add('uses');
    Source.Add('{$IFDEF UNIX}');
    Source.Add('  CThreads,');
    Source.Add('{$ENDIF}');
    Source.Add('{$IFDEF ANDROID}');
    Source.Add('  jni, ctypes, SDL.Android.NativeActivity, SDL.Android.Logger,');
    Source.Add('{$ELSE}');
    Source.Add('  Event.Logger, eventlog,');
    Source.Add('{$ENDIF}');
    Source.Add('  interfaces, SDL.Init,');
    Source.Add('  SDL.Extended.Application, sdl2windowunit, SDL.Extended.Interfaces,');
    Source.Add('  SDL.Extended.Types');
    Source.Add('  { add your units here };');
    Source.Add('');
    Source.Add('type');
    Source.Add('');
    Source.Add('  { ' + AppClassName + ' }');
    Source.Add('');
    Source.Add('  ' + AppClassName + ' = class(TSDL2Application)');
    Source.Add('  protected');
    Source.Add('    procedure DoInitialize; override;');
    Source.Add('  end;');
    Source.Add('');
    Source.Add('procedure ' + AppClassName + '.DoInitialize;');
    Source.Add('begin');
    Source.Add('  inherited DoInitialize;');
    Source.Add('  Paths.SDL2 := ' + QuotedStr('SDL2') + ';');
    Source.Add('  Paths.SDL_Image := ' + QuotedStr('SDL2_image') + ';');
    Source.Add('end;');
    Source.Add('');
    Source.Add('{$IFDEF ANDROID}');
    Source.Add('exports');
    Source.Add('  JNI_OnLoad,');
    Source.Add('  JNI_OnUnload,');
    Source.Add('  SDL_GetEnv name ' + QuotedStr('Android_JNI_GetEnv') + ',');
    Source.Add('  SDL_Init name ' + QuotedStr('Java_org_libsdl_app_SDLActivity_nativeInit') + ',');
    Source.Add('  Native_Create name ' + QuotedStr('ANativeActivity_onCreate') + ';');
    Source.Add('{$ENDIF}');
    Source.Add('');
    Source.Add('{$IFDEF APP_LOGGING}');
    Source.Add('var');
    Source.Add('  Logger: ILogger;');
    Source.Add('{$ENDIF}');
    Source.Add('begin');
    Source.Add('  {$IFDEF APP_LOGGING}');
    Source.Add('  Logger := GetLogger;');
    Source.Add('  Logger.Log(logDebug, ' + QuotedStr('-->AppMain') + ');');
    Source.Add('  {$ENDIF}');
    Source.Add('  TSDL2Application.Init(' + AppClassName + ');');
    Source.Add('  try');
    Source.Add('    Application.Title:=' + QuotedStr(AppTitle) + ';');
    Source.Add('    {$IFDEF APP_LOGGING}');
    Source.Add('    Application.Logger := Logger;');
    Source.Add('    {$ENDIF}');
    Source.Add('    {$IFNDEF ANDROID}');
    Source.Add('    Application.Initialize;');
    Source.Add('    Application.CreateWnd;');
    Source.Add('    Application.Run;');
    Source.Add('    {$ENDIF}');
    Source.Add('  finally');
    Source.Add('    {$IFNDEF ANDROID}');
    Source.Add('    Application.Free;');
    Source.Add('    {$ENDIF}');
    Source.Add('  end;');
    Source.Add('  {$IFDEF APP_LOGGING}');
    Source.Add('  Logger.Log(logDebug, ' + QuotedStr('<--AppMain') + ');');
    Source.Add('  {$ENDIF}');
    Source.Add('end.');
    PFile.SetSourceText(Source.Text, True);
  finally
    Source.Free
  end;
  {$ENDREGION}
  DebugLn('<--InitProject()');
end;

function TSDL2ProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
var
  PFileName: String;
  Source: TStringList;
begin
  DebugLn('-->CreateStartFiles()');
  Result := inherited CreateStartFiles(AProject);

  if Result <> mrOK then
    Exit;

  Result := LazarusIDE.DoNewEditorFile(
    ProjectFileDescriptors.FindByName('SDL Window'),
    'SDL2WindowUnit.pas',
    '',
    [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]
  );

  {$REGION 'Java sources'}
  PFileName := '$PkgDir(LazSDL2Design)java_templates' + PathDelim + 'SDL' + PathDelim;
  if IDEMacros.SubstituteMacros(PFileName) then
  begin
    Source := TStringList.Create;
    try
      Source.LoadFromFile(PFileName + 'SDLActivity.java');
      Result := LazarusIDE.DoNewEditorFile(
        ProjectFileDescriptors.FindByName('JavaSource'),
        'SDLActivity.java',
        Source.Text,
        [nfIsPartOfProject,nfOpenInEditor]
      );
    finally
      Source.Free
    end;

    Source := TStringList.Create;
    try
      Source.LoadFromFile(PFileName + 'GameActivity.java');
      Result := LazarusIDE.DoNewEditorFile(
        ProjectFileDescriptors.FindByName('JavaSource'),
        'GameActivity.java',
        Source.Text,
        [nfIsPartOfProject,nfOpenInEditor]
      );
    finally
      Source.Free
    end;
  end;
  {$ENDREGION}

  LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename ,-1,-1, [ofProjectLoading,ofRegularFile]);

  with TAndroidProjectOptions.GetInstance as TAndroidProjectOptions do
  begin
    Clear;
    MainActivity := 'GameActivity';
    PackageName := 'com.pascal.game';
    NeedLoad := False;

    if FIncludeLibs then
    begin
      AndroidLibs.Add('$PkgDir(LazSDL2Design)bin' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim + '$Lib(SDL2)');
      AndroidLibs.Add('$PkgDir(LazSDL2Design)bin' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim + '$Lib(SDL2_image)');
      AndroidLibs.Add('$PkgDir(LazSDL2Design)bin' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim + '$Lib(SDL2_mixer)');
      AndroidLibs.Add('$PkgDir(LazSDL2Design)bin' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim + '$Lib(SDL2_net)');
      AndroidLibs.Add('$PkgDir(LazSDL2Design)bin' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim + '$Lib(SDL2_ttf)');
    end;
  end;

  DebugLn('<--CreateStartFiles()');
end;

end.

