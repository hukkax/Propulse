// ======================================================================================
//  Utility functions for file handling, hukka 2015
// ======================================================================================

unit FileUtils;

interface

uses
	Types, Classes, SysUtils; //, IOUtils;

const
	IllegalFilenameChars = '<>:"/\|?*';

	function	IsFile(const Filename: String): Boolean;
	function 	ValidateFilename(const Filename: String): String;
	function	GetFileInfo(const Filename: String; out FileSize: Cardinal;
				out DateModified: TDateTime): Boolean;
	function 	GetFileSize(const Filename: String): Cardinal; inline;
	function 	GetFileModifiedDate(const Filename: String): TDateTime;
	function	CreateIni(const Filename, Name: String): Boolean;
	function 	DeleteToBin(const Filename: String): Boolean; overload;
	function 	DeleteToBin(const Filenames: TStrings): Boolean; overload;
	function	GetParentDir(const Path: String): String;
	function 	MoveDirectory(const fromDir, toDir: String): Boolean;
	function 	CopyFileSHFileOperation(const srcFile, destFile: String): Boolean;
	function 	IsSameFolder(const OldPath, NewPath: String): Boolean;
	function 	IsSubFolder(const OldPath, NewPath: String): Boolean;
	procedure 	FileSearch(const PathName, Extensions: String; var lstFiles: TStringList);


implementation

{$IFDEF WINDOWS}
uses
	//DSiWin32,
	ProTracker.Util,
	ShellAPI;
{$ENDIF}

function GetParentDir(const Path: String): String; inline;
begin
	Result := ExpandFileName(Path + '..');
end;

function IsSameFolder(const OldPath, NewPath: String): Boolean; inline;
begin
	Result :=
		LowerCase(IncludeTrailingPathDelimiter(OldPath)) =
		LowerCase(IncludeTrailingPathDelimiter(NewPath));
end;

function IsSubFolder(const OldPath, NewPath: String): Boolean; inline;
begin
	Result := (Pos(
		LowerCase(IncludeTrailingPathDelimiter(OldPath)),
		LowerCase(IncludeTrailingPathDelimiter(NewPath))) = 1);
end;

// Determines whether a path is a file or directory
//
function IsFile(const Filename: String): Boolean; inline;
var
	Attrs: Integer;
begin
	Attrs := FileGetAttr(Filename);
	Result := (Attrs and faDirectory) <> 0;
end;

function ValidateFilename(const Filename: String): String;
var
	X: Integer;
begin
	Result := Filename;
	for X := 1 to Length(Filename) do
		if Pos(Filename[X], IllegalFilenameChars) > 0 then
			Result[X] := ' ';
end;

function CopyFileSHFileOperation(const srcFile, destFile: String): Boolean;
{$IFDEF WINDOWS}
var
	shFOS: TShFileOpStruct;
begin
	ZeroMemory(@shFOS, SizeOf(TShFileOpStruct));

	shFOS.Wnd   := 0; //Application.MainForm.Handle; !!!
	shFOS.wFunc := FO_COPY;
	shFOS.pFrom := PChar(srcFile + #0);
	shFOS.pTo   := PChar(destFile + #0);

	//Do not ask the user to confirm the creation of a
	//new directory if the operation requires one to be created.
	shFOS.fFlags := FOF_NOCONFIRMATION {or FOF_NOCONFIRMMKDIR};

	Result := SHFileOperation(shFOS) = 0;
end;
{$ELSE}
begin
    Result := False;
end;
{$ENDIF}

function MoveDirectory(const fromDir, toDir: String): Boolean;
{$IFDEF WINDOWS}
var
	fos: TSHFileOpStruct;
begin
	ZeroMemory(@fos, SizeOf(fos));
	with fos do
	begin
		wFunc  := FO_MOVE;
		fFlags := FOF_FILESONLY;
		pFrom  := PChar(fromDir + #0);
		pTo    := PChar(toDir)
	end;
	Result := (0 = ShFileOperation(fos));
end;
{$ELSE}
begin
	Result := False;
end;
{$ENDIF}

// Gets file size and date
//
function GetFileInfo(const Filename: String; out FileSize: Cardinal;
	out DateModified: TDateTime): Boolean;
begin
	FileSize := GetFileSize(Filename);
	DateModified := GetFileModifiedDate(Filename);
end;

function GetFileSize(const Filename: String): Cardinal; inline;
var
	F: file of Byte;
begin
	Assign (F, Filename);
	Reset(F);
	Result := FileSize(F);
	Close(F);
end;

function GetFileModifiedDate(const Filename: String): TDateTime; inline;
begin
	Result := FileDateToDateTime(FileAge(Filename));
end;

{var
	info: TWin32FileAttributeData;
begin
	if GetFileAttributesEx(PWideChar(Filename), GetFileExInfoStandard, @info) then
		Result := DSiFileTimeToDateTime(info.ftLastWriteTime);
end;}

// If an inifile doesn't exist, creates it from internal resource
//
function CreateIni(const Filename, Name: String): Boolean;
var
	RS: TResourceStream;
	Sl: TStrings;
	Path: String;
begin
	if not FileExists(Filename) then
	begin
		Path := ExtractFilePath(Filename);
		if not DirectoryExists(Path) then
			ForceDirectories(Path);
		RS := TResourceStream.Create(hInstance, Name, RT_RCDATA);
		Sl := TStringList.Create;
		Sl.LoadFromStream(RS);
		Sl.SaveToFile(Filename);
		Sl.Free;
		RS.Free;
		Result := False;
	end
	else
		Result := True;
end;

// Deletes files to Recycle Bin.
// "Filenames" is a list of files and directories you want to delete.
//  After executing, you have to check which files were really deleted
//  because the user can cancel the deleting procedure.
function DeleteToBin(const Filenames: TStrings): Boolean;
{$IFDEF WINDOWS}
var
	iFile: Integer;
	sFilenames: String;
	Op: TSHFileOpStruct;
begin
	if (Filenames.Count < 1) then Exit(False);

	// Create a zero delimited string with two trailing zeros
	sFilenames := '';
	for iFile := 0 to Filenames.Count-1 do
		sFilenames := sFilenames +
			ExcludeTrailingPathDelimiter(Filenames.Strings[iFile]) + #0;

	FillChar(Op, SizeOf(Op), 0);

	//Op.Wnd := Seymour.WindowHandle; // not used, no dialog displayed
	Op.wFunc := FO_DELETE;
	Op.pFrom := PChar(sFilenames + #0);
	Op.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;

	Result := (SHFileOperation(Op) = 0);
end;
{$ELSE}
var
   	S: String;
begin
	for S in Filenames do
		DeleteFile(S);
    Result := True;
end;
{$ENDIF}

function DeleteToBin(const Filename: String): Boolean;
{$IFDEF WINDOWS}
var
	Op: TSHFileOpStruct;
begin
	FillChar(Op, SizeOf(Op), 0);

	//Op.Wnd := Seymour.WindowHandle;
	Op.wFunc := FO_DELETE;
	Op.pFrom := PChar(Filename + #0);
	Op.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;

	Result := (ShFileOperation(Op) = 0);
end;
{$ELSE}
begin
    DeleteFile(Filename);
    Result := True;
end;
{$ENDIF}

procedure FileSearch(const PathName, Extensions: String; var lstFiles: TStringList);
const
	FileMask = '*.*';
var
	Rec: TSearchRec;
	Path: String;
begin
	if not Assigned(lstFiles) then
		lstFiles := TStringList.Create;

	Path := IncludeTrailingPathDelimiter(PathName);
	if FindFirst(Path + FileMask, faAnyFile - faDirectory, Rec) = 0 then
	try
		repeat
			if AnsiPos(ExtractFileExt(Rec.name), Extensions) > 0 then
				lstFiles.Add(Path + Rec.name);
		until FindNext(Rec) <> 0;
	finally
		SysUtils.FindClose(Rec);
	end;
	if FindFirst(Path + '*.*', faDirectory, Rec) = 0 then
	try
		repeat
			if ((Rec.Attr and faDirectory) <> 0) and (Rec.name <> '.') and (Rec.name <> '..') then
				FileSearch(Path + Rec.name, Extensions, lstFiles);
		until FindNext(Rec) <> 0;
	finally
		FindClose(Rec);
	end;
end;

end.
