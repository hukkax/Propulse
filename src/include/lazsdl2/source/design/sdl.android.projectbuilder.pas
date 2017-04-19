unit SDL.Android.ProjectBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL2.DesignStrings, DOM, Forms, SDL.ProjectBuilder;

type

  { TAndroidProjectBuilder }

  TAndroidProjectBuilder = class(TSDLProjectBuilder)
  private type
    TAndroidEnvs = (
      envProjDir,
      envLpr,
      envLpi,
      envIcon,
      envSDK,
      envAndroidDir,
      envTargetMinSDK,
      envTargetSDK,
      envTargetFile,
      envPackageName,
      envOpenGLES,
      envMainActivity,
      envScreensSmall,
      envScreensNormal,
      envScreensLarge,
      envScreensxLarge,
      envScreensAnyDensity
    );
    TAndroidMacroEnvs = envSDK..envScreensAnyDensity;
  private const
    FMacros: array [TAndroidMacroEnvs] of record
      Macro: String;
      Error: PString;
      Hint: PString;
    end = (
      (Macro: '$Android(SDK)'; Error: @ErrAndroidSDKNotSet; Hint: @rsSetAndroidEnvOptionGeneral), //envSDK
      (Macro: '$MakeDir($(ProjOutDir)android)'; Error: @rsErrUnitOutputDirectoryNotSet; Hint: nil), //envAndroidDir,
      (Macro: '$AndroidProject(TargetMinSDK)'; Error: @rsErrAndroidTargetNotFound; Hint: @rsSetAndroidEnvOptionGeneral), //envTargetMinSDK
      (Macro: '$AndroidProject(TargetSDK)'; Error: @rsErrAndroidTargetNotFound; Hint: @rsSetAndroidEnvOptionGeneral), //envTargetSDK
      (Macro: '$AndroidProject(ProjectName)'; Error: @rsErrTargetFileNoFound; Hint: nil), //envTargetFile
      (Macro: '$AndroidProject(PackageName)'; Error: @rsAndroidPacka; Hint: @rsSetAndroidProjectOptionGeneral), //envPackageName
      (Macro: '$AndroidProject(OpenGLES)'; Error: @rsErrOpenGLESVersionNotSet; Hint: @rsSetAndroidProjectOptionGeneral), //envOpenGLES
      (Macro: '$AndroidProject(MainActivity)'; Error: @rsErrMainActivityNotSet; Hint: @rsSetAndroidProjectOptionGeneral), //envMainActivity
      (Macro: '$AndroidProject(ScreensSmall)'; Error: @rsErrScreenSettingsNotSet; Hint: @rsSetAndroidProjectOptionGeneral), //envScreensSmall
      (Macro: '$AndroidProject(ScreensNormal)'; Error: @rsErrScreenSettingsNotSet; Hint: @rsSetAndroidProjectOptionGeneral), //envScreensNormal
      (Macro: '$AndroidProject(ScreensLarge)'; Error: @rsErrScreenSettingsNotSet; Hint: @rsSetAndroidProjectOptionGeneral), //envScreensLarge
      (Macro: '$AndroidProject(ScreensxLarge)'; Error: @rsErrScreenSettingsNotSet; Hint: @rsSetAndroidProjectOptionGeneral), //envScreensxLarge
      (Macro: '$AndroidProject(ScreensAnyDensity)'; Error: @rsErrScreenSettingsNotSet; Hint: @rsSetAndroidProjectOptionGeneral)  //envScreensAnyDensity
    );

  private
    FEnvs: TStringList;
    FLpiFile: TXMLDocument;
    function GetEnv(Index: TAndroidEnvs): String;
    function InternalBuldAndroid: Boolean;
    function InternalCopyResources: Boolean;
    function InternalCopySource: Boolean;
    function InternalGetLppiInfo(const Path: String; Default: String = ''): String;
    function InternalUpdateAndroidDirectory: Boolean;
    function InternalWriteConfigFiles: Boolean;
    function InternalWriteManifest: Boolean;
    function WriteEnv(const VarName: String; const EnvName: TAndroidMacroEnvs): Boolean;
    procedure SetEnv(Index: TAndroidEnvs; AValue: String);
  protected
    function CreateDirectories: Boolean; override;
    function CopyLibraries: Boolean; override;
    procedure DoBuild; override;
    procedure Init; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;


    property Env[Index: TAndroidEnvs]: String read GetEnv write SetEnv;
  end;

implementation

uses
  IDEMsgIntf, FileUtil, LazFileUtils, IDEExternToolIntf, LazIDEIntf, MacroIntf,
  Graphics, FPimage, FPImgCanv, FPReadPNG, FPWritePNG, LazCanvas, XMLRead, process, typinfo;

{ TAndroidProjectBuilder }

function TAndroidProjectBuilder.InternalUpdateAndroidDirectory: Boolean;
begin
  if not InternalCopyResources then
    Exit(False);

  if not InternalCopySource then
    Exit(False);

  if not InternalWriteConfigFiles then
    Exit(False);

  if not InternalWriteManifest then
    Exit(False);

  Result := True
end;

function TAndroidProjectBuilder.InternalCopyResources: Boolean;

  procedure CreateIcons(const IconFile: String);
  const
    ImgSizes: array [0..2] of Integer = (72, 48, 36);
    ImgFolders: array [0..2] of String = ('drawable-hdpi', 'drawable-mdpi', 'drawable-ldpi');
    ClearColor: TFPColor = (Red: 0; Green: 0; Blue: 0; Alpha: 0);
  var
    SrcIcon: TIcon;
    Png: TPortableNetworkGraphic;
    Buffer: TStream;
    ImgFrom: TFPMemoryImage;
    ImgTo: TFPMemoryImage;
    ImgCanvas: TFPImageCanvas;
    Writer: TFPWriterPNG;
    Reader: TFPReaderPNG;
    i, j, l, LargestSize, LargestPos: Integer;
  begin
    ImgFrom := TFPMemoryImage.create(0, 0);
    Writer := TFPWriterPNG.create;
    try
      Writer.UseAlpha := True;
      Writer.WordSized := False;

      SrcIcon := TIcon.Create;
      Png := TPortableNetworkGraphic.Create;
      Buffer := TMemoryStream.Create;
      try
        SrcIcon.LoadFromFile(IconFile);
        LargestPos := -1;
        LargestSize := -1;
        for i := 0 to SrcIcon.Count - 1 do
        begin
          SrcIcon.Current := i;
          if SrcIcon.Width > LargestSize then
          begin
            LargestSize := SrcIcon.Width;
            LargestPos := i;
          end;
        end;
        SrcIcon.Current := LargestPos;

        Png.Assign(SrcIcon);
        Png.SaveToStream(Buffer);
        Buffer.Position := 0;
        Reader := TFPReaderPNG.create;
        try
          ImgFrom.LoadFromStream(Buffer, Reader);
        finally
          Reader.Free;
        end;
      finally
        SrcIcon.Free;
        Png.Free;
        Buffer.Free;
      end;

      for l := Low(ImgSizes) to High(ImgSizes) do
      begin
        ImgTo := TFPMemoryImage.create(ImgSizes[l], ImgSizes[l]);
        ImgCanvas := TLazCanvas.create(ImgTo);
        try
          for i := 0 to ImgFrom.Width - 1 do
            for j := 0 to ImgFrom.Height - 1 do
              ImgCanvas.Colors[i, j] := ClearColor;
          ImgCanvas.StretchDraw(0, 0, ImgTo.Width, ImgTo.Height, ImgFrom);
          ImgTo.SaveToFile(Env[envAndroidDir] + 'res' + PathDelim + ImgFolders[l] + PathDelim + 'icon.png', Writer)
        finally
          ImgTo.Free;
          ImgCanvas.Free
        end
      end
    finally
    end;
  end;

var
  IconFile: String;
begin
  AddIDEMessage(mluVerbose, rsCopyingResources);

  if not FileExistsUTF8(Env[envIcon]) then
  begin
    AddIDEMessage(mluWarning, rsUsingLazarusIcon);
    IconFile := AppendPathDelim('$(LazarusDir)') + 'images' + PathDelim + 'icons' + PathDelim + 'blue_roundrect.ico';
    if not IDEMacros.SubstituteMacros(IconFile) then
    begin
      AddIDEMessage(mluError, Format(rsErrCouldNotFounLazarusIcon, [IconFile]));
      Exit(False)
    end;
  end
  else
    IconFile := Env[envIcon];

  if not FileExistsUTF8(IconFile) then
  begin
    AddIDEMessage(mluError, Format(rsErrCouldNotFounLazarusIcon, [IconFile]));
    Exit(False)
  end;

  CreateIcons(IconFile);

  with TStringList.Create do
  try
    Add('<?xml version="1.0" encoding="utf-8"?>');
    Add('<resources>');
    Add('    <string name="app_name">' + InternalGetLppiInfo('CONFIG/ProjectOptions/General/Title/Value') + '</string>');
    Add('</resources>');
    SaveToFile(Env[envAndroidDir] + 'res' + PathDelim + 'values' + PathDelim + 'strings.xml');
  finally
    Free
  end;

  Result := True
end;

function TAndroidProjectBuilder.InternalCopySource: Boolean;

  procedure MoveJavaFile(const SRC: String; const JavaFile: String);
  var
    JFile: TextFile;
    PckgName, SrcFolder, Path: String;
    PackagePath: TStringList;
  begin
    if not FileExistsUTF8(JavaFile) then
    begin
      AddIDEMessage(mluError, Format(rsErrFileNotFound, [JavaFile]));
      Exit;
    end;

    AssignFile(JFile, JavaFile);
    try
      Reset(JFile);
      ReadLn(JFile, PckgName);
      PckgName := LowerCase(StringReplace(Trim(PckgName), '.', PathDelim, [rfReplaceAll]));
    finally
      CloseFile(JFile);
    end;

    if Pos('package', PckgName) <> 1 then
    begin
      AddIDEMessage(mluError, Format('First line do not start with package (%s)', [JavaFile]));
      Exit;
    end;
    Delete(PckgName, 1, 7);
    PckgName := Trim(PckgName);
    Delete(PckgName, Length(PckgName), 1);

    PackagePath := TStringList.Create;
    try
      PackagePath.Delimiter := PathDelim;
      PackagePath.DelimitedText := PckgName;

      SrcFolder := SRC + 'src' + PathDelim;
      for Path in PackagePath do
      begin
        SrcFolder := SrcFolder + Path + PathDelim;

        if not ForceDirectoriesUTF8(SrcFolder) then
        begin
          AddIDEMessage(mluError, Format('Failed to create directory "%s"', [SrcFolder]));
          Exit;
        end
      end;
    finally
      PackagePath.Free;
    end;

    SrcFolder := AppendPathDelim(SrcFolder);
    if not CopyFile(JavaFile, SrcFolder + ExtractFileName(JavaFile)) then
      AddIDEMessage(mluError, Format('Failed to copy "%s" to "%s"', [JavaFile, SrcFolder + ExtractFileName(JavaFile)]));
  end;

var
  Dom: TXMLDocument;
  XmlOptions, XmlUnits, XmlFilename: TDOMNode;
  i: Integer;
  FileName: String;
begin
  AddIDEMessage(mluVerbose, rsCopyingJavaSources);

  Dom := TXMLDocument.Create;
  try
    ReadXMLFile(Dom, Env[envLpi]);

    XmlOptions := Dom.FirstChild.FindNode('ProjectOptions');
    if Assigned(XmlOptions) then
    begin
      XmlUnits := XmlOptions.FindNode('Units');
      if Assigned(XmlUnits) then
      begin
        for i := 0 to XmlUnits.ChildNodes.Count - 1 do
        begin
          XmlFilename := XmlUnits.ChildNodes[i].FindNode('Filename');
          if Assigned(XmlFilename) then
          begin
            FileName := String(TDOMElement(XmlFilename).AttribStrings['Value']);
            if LowerCase(ExtractFileExt(FileName)) = '.java' then
              MoveJavaFile(Env[envAndroidDir], Env[envProjDir] + FileName)
          end;
        end;
      end
    end
  finally
    Dom.Free
  end;

  Result := True;
end;

function TAndroidProjectBuilder.InternalWriteConfigFiles: Boolean;
begin
  with TStringList.Create do
  try
    Add('# This file is automatically generated by Android Tools.');
    Add('# Do not modify this file -- YOUR CHANGES WILL BE ERASED!');
    Add('#');
    Add('# This file must *NOT* be checked in Version Control Systems,');
    Add('# as it contains information specific to your local configuration.');
    Add('');
    Add('# location of the SDK. This is only used by Ant');
    Add('# For customization when using a Version Control System, please read the');
    Add('# header note.');
    Add('sdk.dir=' + Env[envSDK]);
    SaveToFile(Env[envAndroidDir] + 'local.properties')
  finally
    Free
  end;

  with TStringList.Create do
  try
    Add('# This file is automatically generated by Android Tools.');
    Add('# Do not modify this file -- YOUR CHANGES WILL BE ERASED!');
    Add('#');
    Add('# This file must be checked in Version Control Systems.');
    Add('#');
    Add('# To customize properties used by the Ant build system use,');
    Add('# "build.properties", and override values to adapt the script to your');
    Add('# project structure.');
    Add('');
    Add('# Project target.');
    Add('target=' + Env[envTargetSDK]);
    SaveToFile(Env[envAndroidDir] + 'default.properties')
  finally
    Free
  end;

  with TStringList.Create do
  try
    Add('<?xml version="1.0" encoding="UTF-8"?>');
    Add('<project name="' + ExtractFileNameOnly(Env[envTargetFile]) + '" default="help">');
    Add('');
    Add('    <!-- The local.properties file is created and updated by the ''android'' tool.');
    Add('         It contains the path to the SDK. It should *NOT* be checked into');
    Add('         Version Control Systems. -->');
    Add('    <loadproperties srcFile="local.properties" />');
    Add('');
    Add('    <!-- The ant.properties file can be created by you. It is only edited by the');
    Add('         ''android'' tool to add properties to it.');
    Add('         This is the place to change some Ant specific build properties.');
    Add('         Here are some properties you may want to change/update:');
    Add('');
    Add('         source.dir');
    Add('             The name of the source directory. Default is ''src''.');
    Add('         out.dir');
    Add('             The name of the output directory. Default is ''bin''.');
    Add('');
    Add('         For other overridable properties, look at the beginning of the rules');
    Add('         files in the SDK, at tools/ant/build.xml');
    Add('');
    Add('         Properties related to the SDK location or the project target should');
    Add('         be updated using the ''android'' tool with the ''update'' action.');
    Add('');
    Add('         This file is an integral part of the build system for your');
    Add('         application and should be checked into Version Control Systems.');
    Add('');
    Add('         -->');
    Add('    <property file="ant.properties" />');
    Add('');
    Add('    <!-- The project.properties file is created and updated by the ''android''');
    Add('         tool, as well as ADT.');
    Add('');
    Add('         This contains project specific properties such as project target, and library');
    Add('         dependencies. Lower level build properties are stored in ant.properties');
    Add('         (or in .classpath for Eclipse projects).');
    Add('');
    Add('         This file is an integral part of the build system for your');
    Add('         application and should be checked into Version Control Systems. -->');
    Add('    <loadproperties srcFile="default.properties" />');
    Add('');
    Add('    <!-- quick check on sdk.dir -->');
    Add('    <fail');
    Add('            message="sdk.dir is missing. Make sure to generate local.properties using ''android update project''"');
    Add('            unless="sdk.dir"');
    Add('    />');
    Add('');
    Add('');
    Add('<!-- extension targets. Uncomment the ones where you want to do custom work');
    Add('     in between standard targets -->');
    Add('<!--');
    Add('    <target name="-pre-build">');
    Add('    </target>');
    Add('    <target name="-pre-compile">');
    Add('    </target>');
    Add('');
    Add('    /* This is typically used for code obfuscation.');
    Add('       Compiled code location: ${out.classes.absolute.dir}');
    Add('       If this is not done in place, override ${out.dex.input.absolute.dir} */');
    Add('    <target name="-post-compile">');
    Add('    </target>');
    Add('-->');
    Add('');
    Add('    <!-- Import the actual build file.');
    Add('');
    Add('         To customize existing targets, there are two options:');
    Add('         - Customize only one target:');
    Add('             - copy/paste the target into this file, *before* the');
    Add('               <import> task.');
    Add('             - customize it to your needs.');
    Add('         - Customize the whole content of build.xml');
    Add('             - copy/paste the content of the rules files (minus the top node)');
    Add('               into this file, replacing the <import> task.');
    Add('             - customize to your needs.');
    Add('');
    Add('         ***********************');
    Add('         ****** IMPORTANT ******');
    Add('         ***********************');
    Add('         In all cases you must update the value of version-tag below to read ''custom'' instead of an integer,');
    Add('         in order to avoid having your file be overridden by tools such as "android update project"');
    Add('    -->');
    Add('    <!-- version-tag: 1 -->');
    Add('    <import file="${sdk.dir}/tools/ant/build.xml" />');
    Add('');
    Add('</project>');
    SaveToFile(Env[envAndroidDir] + 'build.xml')
  finally
    Free
  end;

  with TStringList.Create do
  try
    Add('# This file is used to override default values used by the Ant build system.');
    Add('#');
    Add('# This file must be checked in Version Control Systems, as it is');
    Add('# integral to the build system of your project.');
    Add('');
    Add('# This file is only used by the Ant script.');
    Add('');
    Add('# You can use this to override default values such as');
    Add('#  ''source.dir'' for the location of your java source folder and');
    Add('#  ''out.dir'' for the location of your output folder.');
    Add('');
    Add('# You can also use it define how the release builds are signed by declaring');
    Add('# the following properties:');
    Add('#  ''key.store'' for the location of your keystore and');
    Add('#  ''key.alias'' for the name of the key to use.');
    Add('# The password will be asked during the build when you use the ''release'' target.');
    Add('source.dir=src');
    SaveToFile(Env[envAndroidDir] + 'build.properties')
  finally
    Free
  end;

  Result := True
end;

function TAndroidProjectBuilder.InternalWriteManifest: Boolean;
var
  versionName: String;
  versionCode: String;
  glEsVersion: String;
  Permissions, MinSDK, TargetSDK: String;
  UsesPermissions: TStringList;
begin
  versionCode := InternalGetLppiInfo('CONFIG/ProjectOptions/VersionInfo/BuildNr/Value');
  if versionCode = '' then
  begin
    AddIDEMessage(mluError, rsErrBuildNumberIsMandatory);
    AddIDEMessage(mluHint, rsSetProjectOptionVersionInfo);
    Exit(False)
  end;

  versionName :=
    InternalGetLppiInfo('CONFIG/ProjectOptions/VersionInfo/MajorVersionNr/Value', '0') + '.' +
    InternalGetLppiInfo('CONFIG/ProjectOptions/VersionInfo/MinorVersionNr/Value', '0') + '.' +
    InternalGetLppiInfo('CONFIG/ProjectOptions/VersionInfo/RevisionNr/Value', '0');
  if versionName = '0.0.0' then
    AddIDEMessage(mluWarning, rsErrNoVersionInfo);

  glEsVersion := '0x000' + Copy(Env[envOpenGLES], 1, 1) + '000' + Copy(Env[envOpenGLES], 3, 1);

  MinSDK := Env[envTargetMinSDK];
  AddIDEMessage(mluVerbose, Format('Android min. target SDK: %s', [MinSDK]));
  MinSDK := Copy(MinSDK, Pos('-', MinSDK) + 1, Length(MinSDK));

  if MinSDK = '' then
  begin
    AddIDEMessage(mluError, rsErrAndroidTargetNotFound);
    AddIDEMessage(mluWarning, rsSetAndroidEnvOptionGeneral);
    Exit(False)
  end;

  TargetSDK := Env[envTargetSDK];
  AddIDEMessage(mluVerbose, Format('Android target SDK: %s', [TargetSDK]));
  TargetSDK := Copy(TargetSDK, Pos('-', TargetSDK) + 1, Length(TargetSDK));

  if TargetSDK = '' then
  begin
    AddIDEMessage(mluError, rsErrAndroidTargetNotFound);
    AddIDEMessage(mluWarning, rsSetAndroidEnvOptionGeneral);
    Exit(False)
  end;

  with TStringList.Create do
  try
    Add('<?xml version="1.0" encoding="utf-8"?>');
    Add('<manifest');
    Add('  xmlns:android="http://schemas.android.com/apk/res/android"');
    Add('  package="' + Env[envPackageName] + '"');
    Add('  android:versionCode="' + versionCode + '"');
    Add('  android:versionName="' + versionName + '">');

    Permissions := '$AndroidProject(Permissions)';
    if IDEMacros.SubstituteMacros(Permissions) then
    begin
      Add('');
      UsesPermissions := TStringList.Create;
      try
        UsesPermissions.DelimitedText := Permissions;

        for Permissions in UsesPermissions do
          Add('    <uses-permission android:name="android.permission.' + Permissions + '" />')
      finally
        UsesPermissions.Free
      end
    end;

    Add('');
    Add('    <uses-feature android:glEsVersion="' + glEsVersion + '" />');
    Add('');
    Add('    <supports-screens');
    Add('        android:smallScreens="' + Env[envScreensSmall] + '"');
    Add('        android:normalScreens="' + Env[envScreensNormal] + '"');
    Add('        android:largeScreens="' + Env[envScreensLarge] + '"');
    Add('        android:xlargeScreens="' + Env[envScreensxLarge] + '"');
    Add('        android:anyDensity="' + Env[envScreensAnyDensity] + '"');
    Add('    />');
    Add('');
    Add('    <uses-sdk');
    Add('        android:minSdkVersion="' + MinSDK + '"');
    Add('        android:targetSdkVersion="' + TargetSDK + '"');
    Add('    />');
    Add('');
    Add('    <application');
    Add('         android:label="@string/app_name"');
    Add('         android:icon="@drawable/icon"');
    Add('         android:allowBackup="true"');
    Add('         android:theme="@android:style/Theme.NoTitleBar.Fullscreen"');
    Add('         android:hardwareAccelerated="true"');
    Add('    >');
    Add('        <activity');
    Add('             android:name="' + Env[envMainActivity] + '"');
    Add('             android:label="@string/app_name"');
    Add('             android:configChanges="keyboardHidden|orientation"');
    Add('        >');
    Add('            <intent-filter>');
    Add('                <action android:name="android.intent.action.MAIN" />');
    Add('                <category android:name="android.intent.category.LAUNCHER" />');
    Add('            </intent-filter>');
    Add('        </activity>');
    Add('    </application>');
    Add('</manifest>');
    SaveToFile(Env[envAndroidDir] + 'AndroidManifest.xml')
  finally
    Free
  end;

  Result := True
end;

procedure TAndroidProjectBuilder.SetEnv(Index: TAndroidEnvs; AValue: String);
begin
  FEnvs.Values[GetEnumName(TypeInfo(TAndroidEnvs), Ord(Index))] := AValue
end;

function TAndroidProjectBuilder.CreateDirectories: Boolean;
begin
  DeleteDirectory(Env[envAndroidDir] + 'src', False);
  DeleteDirectory(Env[envAndroidDir] + 'bin', False);
  DeleteDirectory(Env[envAndroidDir] + 'libs', False);

  if not IDECreateDir(Env[envAndroidDir] + 'libs' + PathDelim + 'armeabi') then
    Exit(False);

  if not IDECreateDir(Env[envAndroidDir] + 'res' + PathDelim + 'drawable-hdpi') then
    Exit(False);

  if not IDECreateDir(Env[envAndroidDir] + 'res' + PathDelim + 'drawable-ldpi') then
    Exit(False);

  if not IDECreateDir(Env[envAndroidDir] + 'res' + PathDelim + 'drawable-mdpi') then
    Exit(False);

  if not IDECreateDir(Env[envAndroidDir] + 'res' + PathDelim + 'values') then
    Exit(False);

  if not IDECreateDir(Env[envAndroidDir] + 'src') then
    Exit(False);

  Result := True;
end;

function TAndroidProjectBuilder.CopyLibraries: Boolean;
begin
  Result := IDECopyLibraries(Env[envAndroidDir] + 'libs' + PathDelim + 'armeabi' + PathDelim)
end;

procedure TAndroidProjectBuilder.Init;
var
  i: TAndroidMacroEnvs;
begin
  Env[envProjDir] := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.GetFullFilename);
  Env[envLpr] := LazarusIDE.ActiveProject.MainFile.GetFullFilename;
  Env[envLpi] := ChangeFileExt(LazarusIDE.ActiveProject.MainFile.GetFullFilename, '.lpi');
  Env[envIcon] := ChangeFileExt(LazarusIDE.ActiveProject.MainFile.GetFullFilename, '.ico');

  ReadXMLFile(FLpiFile, Env[envLpi]);

  for i := Low(TAndroidMacroEnvs) to High(TAndroidMacroEnvs) do with FMacros[i] do
  begin
    if not WriteEnv(Macro, i) then
    begin
      if Assigned(Error) then
        AddIDEMessage(mluError, Error^);

      if Assigned(Hint) then
        AddIDEMessage(mluHint, Hint^);

      Exit
    end
  end;

  if not DirectoryExistsUTF8(Env[envSDK]) then
  begin
    AddIDEMessage(mluError, Format(rsErrAndroidSDKNotFound, [Env[envSDK]]));
    AddIDEMessage(mluError, rsSetAndroidEnvOptionGeneral);
    Exit;
  end;
end;

function TAndroidProjectBuilder.WriteEnv(const VarName: String;
  const EnvName: TAndroidMacroEnvs): Boolean;
var
  Param: String;
begin
  Param := VarName;
  Result := IDEMacros.SubstituteMacros(Param);

  if Result then
    Env[EnvName] := Param;
end;

function TAndroidProjectBuilder.InternalGetLppiInfo(const Path: String;
  Default: String): String;
var
  XmlNode: TDOMNode;
  XmlPath: TStringList;
  i: Integer;
begin
  XmlPath := TStringList.Create;
  try
    XmlPath.Delimiter := '/';
    XmlPath.DelimitedText := Path;

    XmlNode := FLpiFile;
    for i := 0 to XmlPath.Count - 1 do
    begin
      if i < XmlPath.Count - 1 then
        XmlNode := XmlNode.FindNode(WideString(XmlPath[i]))
      else
        Exit(String(TDOMElement(XmlNode).AttribStrings[WideString(XmlPath[i])]));

      if not Assigned(XmlNode) then
        Exit(Default)
    end
  finally
    XmlPath.Free
  end;
end;

function TAndroidProjectBuilder.InternalBuldAndroid: Boolean;
var
  LazProcess: TProcess;
  StdOutput: TStringList;
  Timeout: QWord;
  Msg, FileName, Adb: String;
  i: Integer;
begin
  LazProcess := TProcess.Create(nil);
  try
    LazProcess.CurrentDirectory := Env[envAndroidDir];
    LazProcess.Executable := IDEMacro('$Android(Ant)');

    if not FileExistsUTF8(LazProcess.Executable) then
    begin
      AddIDEMessage(mluError, rsErrPathToAntNotSet);
      AddIDEMessage(mluHint, rsSetAndroidEnvOptionGeneral);
      Exit(False);
    end;

    LazProcess.Parameters.Add('debug');
    LazProcess.Options := LazProcess.Options + [poWaitOnExit, poUsePipes,poStderrToOutPut, poNoConsole];
    LazProcess.Execute;

    StdOutput := TStringList.Create;
    try
      StdOutput.LoadFromStream(LazProcess.Output);

      for Msg in StdOutput do
        AddIDEMessage(mluVerbose, Msg);
    finally
      StdOutput.Free;
    end;

    if LazProcess.ExitCode <> 0 then
      Exit(False);

    FileName := IDEMacro('$(TargetFile)');
    DeleteFileUTF8(FileName);

    if not IDECopyFile(
      Env[envAndroidDir] + 'bin' + PathDelim + Env[envTargetFile] + '-debug.apk',
      AppendPathDelim(ExtractFilePath(FileName)) + Env[envTargetFile] + '-debug.apk') then
      Exit(False)
  finally
    LazProcess.Free
  end;

  Adb := IDEMacro('$MakeDir($MakeDir($Android(SDK))platform-tools)$MakeExe(adb)');
  if not FileExistsUTF8(Adb) then
  begin
    AddIDEMessage(mluWarning, Format('Adb not found at "%s" posible problems with android SDK', [Adb]));
    Exit;
  end
  else
    AddIDEMessage(mluVerbose2, 'Adb: ' + Adb);

  LazProcess := TProcess.Create(nil);
  try
    LazProcess.Executable := Adb;
    LazProcess.Options := LazProcess.Options + [poWaitOnExit, poUsePipes,poStderrToOutPut, poNoConsole];
    LazProcess.Parameters.Add('start-server');
    LazProcess.Execute;
  finally
    LazProcess.Free;
  end;

  LazProcess := TProcess.Create(nil);
  try
    LazProcess.Executable := Adb;
    LazProcess.Options := LazProcess.Options + [poWaitOnExit, poUsePipes,poStderrToOutPut, poNoConsole];
    LazProcess.Parameters.Add('devices');
    LazProcess.Execute;

    StdOutput := TStringList.Create;
    try
      StdOutput.LoadFromStream(LazProcess.Output);

      for i := 0 to StdOutput.Count - 1 do
        AddIDEMessage(mluVerbose, StdOutput[i]);

      for i := StdOutput.Count - 1  downto 0 do
        if Trim(StdOutput[i]) = '' then
          StdOutput.Delete(i);

      if StdOutput.Count < 2 then
      begin
        AddIDEMessage(mluWarning, 'No device found could not install apk file');
        Exit;
      end;
    finally
      StdOutput.Free;
    end;
  finally
    LazProcess.Free;
  end;

  LazProcess := TProcess.Create(nil);
  try
    LazProcess.Executable := Adb;
    LazProcess.Options := LazProcess.Options + [poUsePipes,poStderrToOutPut, poNoConsole];
    LazProcess.Parameters.Add('uninstall');
    LazProcess.Parameters.Add(Env[envPackageName]);

    AddIDEMessage(mluVerbose, 'Uninstaling: ' + Env[envPackageName]);
    Timeout := GetTickCount64 + 10000; //Try to instal for 10 seconds
    LazProcess.Execute;
    while LazProcess.Active do
    begin
      if GetTickCount64 > Timeout then
      begin
        AddIDEMessage(mluError, 'Adb uninstall timeout!');
        Exit(False)
      end;
    end;
  finally
    LazProcess.Free;
  end;

  LazProcess := TProcess.Create(nil);
  try
    LazProcess.Executable := Adb;
    LazProcess.Options := LazProcess.Options + [poUsePipes,poStderrToOutPut, poNoConsole];
    LazProcess.Parameters.Add('install');
    LazProcess.Parameters.Add(AppendPathDelim(ExtractFilePath(FileName)) + Env[envTargetFile] + '-debug.apk');

    AddIDEMessage(mluVerbose, 'Instaling: ' + AppendPathDelim(ExtractFilePath(FileName)) + Env[envTargetFile] + '-debug.apk');
    Timeout := GetTickCount64 + 10000; //Try to instal for 10 seconds
    LazProcess.Execute;
    while LazProcess.Active do
    begin
      if GetTickCount64 > Timeout then
      begin
        AddIDEMessage(mluError, 'Adb install timeout!');
        Exit(False)
      end;
    end;
  finally
    LazProcess.Free;
  end;
end;

function TAndroidProjectBuilder.GetEnv(Index: TAndroidEnvs): String;
begin
  Result := FEnvs.Values[GetEnumName(TypeInfo(TAndroidEnvs), Ord(Index))]
end;

procedure TAndroidProjectBuilder.AfterConstruction;
begin
  inherited AfterConstruction;
  FEnvs := TStringList.Create;
end;

procedure TAndroidProjectBuilder.BeforeDestruction;
begin
  FreeAndNil(FEnvs);
  FreeAndNil(FLpiFile);
  inherited BeforeDestruction;
end;

procedure TAndroidProjectBuilder.DoBuild;
begin
  inherited;

  AddIDEMessage(mluProgress, Format(rsAndroidDir, [Env[envAndroidDir]]));
  if not InternalUpdateAndroidDirectory then
    Exit;

  AddIDEMessage(mluProgress, rsBuildingAndroid);
  AddIDEMessage(mluProgress, '');

  Application.ProcessMessages;
  if not InternalBuldAndroid then
    Exit;
end;

end.

