unit SDL.ProjectBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Android.Options.Project;

type

  { TSDLProjectBuilder }

  TSDLProjectBuilder = class(TPersistent)
  private type
    TStringArray = array of String;
  private
    FDisrectories: TStringList;
    function GetProjectOptions: TAndroidProjectOptions;
  protected
    function CreateDirectories: Boolean; virtual; abstract;
    function CopyLibraries: Boolean; virtual; abstract;
    procedure Init; virtual; abstract;
    procedure DoBuild; virtual;
  public
    procedure Build;

    function IDECreateDir(const Dir: TFilename): Boolean;
    function IDECopyFile(const Source, Dest: TFilename): Boolean;
    function IDEMacro(const Macro: String): String;
    function IDEGetAllLibFiles: TStringArray;
    function IDECopyLibraries(const Dest: String): Boolean;

    property Disrectories: TStringList read FDisrectories;
    property ProjectOptions: TAndroidProjectOptions read GetProjectOptions;
  end;
  TSDLProjectBuilderClass = class of TSDLProjectBuilder;

  { TSDLDesktopProjectBuilder }

  TSDLDesktopProjectBuilder = class(TSDLProjectBuilder)
  protected
    function CreateDirectories: Boolean; override;
    function CopyLibraries: Boolean; override;
    procedure Init; override;
  end;

implementation

uses
  SDL2.DesignStrings, IDEMsgIntf, IDEExternToolIntf, FileUtil, LazFileUtils, MacroIntf, math;

{ TSDLDesktopProjectBuilder }

function TSDLDesktopProjectBuilder.CreateDirectories: Boolean;
begin
  if ProjectOptions.AndroidLibs.Count > 0 then
  begin
    if not IDECreateDir(IDEMacro('$TargetDir()lib')) then
      Exit(True);
  end;

  Result := True;
end;

function TSDLDesktopProjectBuilder.CopyLibraries: Boolean;
begin
  Result := IDECopyLibraries(IDEMacro('$TargetDir()lib' + PathDelim));
end;

procedure TSDLDesktopProjectBuilder.Init;
begin

end;

{ TSDLProjectBuilder }

function TSDLProjectBuilder.GetProjectOptions: TAndroidProjectOptions;
begin
  Result := TAndroidProjectOptions.GetInstance as TAndroidProjectOptions
end;

procedure TSDLProjectBuilder.DoBuild;
begin

end;

procedure TSDLProjectBuilder.Build;
begin
  Init;

  AddIDEMessage(mluVerbose, rsCreatingDirectoryStructure);
  if not CreateDirectories then
    Exit;

  AddIDEMessage(mluVerbose, rsCopyLibraries);
  if not CopyLibraries then
    Exit;

  DoBuild;
end;

function TSDLProjectBuilder.IDECreateDir(const Dir: TFilename): Boolean;
begin
  if not DirectoryExistsUTF8(Dir) then
  begin
    if not ForceDirectoriesUTF8(Dir) then
    begin
      AddIDEMessage(mluError, Format(ErrCouldNotCreateDirectory, [Dir]));
      Result := False
    end
    else
    begin
      AddIDEMessage(mluVerbose, Format(rsDirectoryCreated, [Dir]));
      Result := True
    end
  end
  else
    Result := True
end;

function TSDLProjectBuilder.IDECopyFile(const Source, Dest: TFilename): Boolean;
begin
  Result := CopyFile(Source, Dest);
  if not Result then
    AddIDEMessage(mluError, Format(rsErrCouldNotCopyFile, [Source, Dest,
      GetLastOSError]));
end;

function TSDLProjectBuilder.IDEMacro(const Macro: String): String;
begin
  Result := Macro;
  if not IDEMacros.SubstituteMacros(Result) then
    Result := ''
end;

function TSDLProjectBuilder.IDEGetAllLibFiles: TStringArray;
var
  Libraries: String;
  LibPath: String;
  AddTarget: Boolean;
  i: Integer;
  AndroidLibs: TStringList;
begin
  AndroidLibs := TStringList.Create;
  try
    Libraries := '$AndroidProject(Libraries)';
    if not IDEMacros.SubstituteMacros(Libraries) then
    begin
      SetLength(Result, 0);
      Exit;
    end
    else
      AndroidLibs.DelimitedText := Libraries;

    if (IDEMacro('$(TargetOS)') = 'android') and (AndroidLibs.IndexOf(IDEMacro('$(TargetFile)')) = -1) then
    begin
      SetLength(Result, AndroidLibs.Count + 1);
      AddTarget := True
    end
    else
    begin
      SetLength(Result, AndroidLibs.Count);
      AddTarget := False
    end;

    for i := 0 to AndroidLibs.Count - 1 do
    begin
      LibPath := AndroidLibs[i];
      if IDEMacros.SubstituteMacros(LibPath) then
        Result[i] := LibPath
      else
        Result[i] := 'null';
    end;

    if AddTarget then
      Result[AndroidLibs.Count] := IDEMacro('$(TargetFile)');
  finally
    AndroidLibs.Free
  end
end;

function TSDLProjectBuilder.IDECopyLibraries(const Dest: String): Boolean;
var
  FileFromCopy, FileToCopy, LibFile, TargetFile: String;
begin
  TargetFile := IDEMacro('$(TargetFile)');

  for LibFile in IDEGetAllLibFiles do
  begin
    if LibFile = 'null' then
      Continue;

    FileFromCopy := ExpandFileNameUTF8(LibFile, IDEMacro('$(ProjPath)'));

    if CompareStr(TargetFile, LibFile) <> EqualsValue then
      FileToCopy := AppendPathDelim(Dest) + ExtractFileName(FileFromCopy)
    else
      FileToCopy := AppendPathDelim(Dest) + 'libmain.so';

    if FileExistsUTF8(FileToCopy) then
    begin
      if not DeleteFileUTF8(FileToCopy) then
      begin
        AddIDEMessage(mluError, Format(rsErrFailedToDeleteFile, [FileToCopy]));
        Exit(False)
      end
    end;

    if FileIsSymlink(FileFromCopy) then
      FileFromCopy := ReadAllLinks(FileFromCopy, False);

    if not CopyFile(FileFromCopy, FileToCopy) then
    begin
      AddIDEMessage(mluError, Format(rsErrFailedToCopyFile, [FileFromCopy, FileToCopy]));
      Exit(False)
    end
    else
      AddIDEMessage(mluVerbose3, Format(rsFileCopied, [FileFromCopy, FileToCopy]))
  end;

  Result := True
end;

end.

