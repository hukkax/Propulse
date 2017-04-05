unit Screen.FileReq;

interface

uses
	Classes, Types, SysUtils,
	TextMode, CWE.Core, CWE.Widgets.Text;

const
	AmigaExts  = '[mod][p61][stk][nst][ust][nt][m15]';
	Extensions_Module = '[mod][it][p61]'; // [.xm][.s3m]

	STR_DIRECTORYUP = '<Parent>';
	STR_BOOKMARKS   = '<Bookmarks>';

	LISTITEM_BOOKMARK	= 1337;
	LISTITEM_DIR		= 1338;
	LISTITEM_DRIVE		= 1339;

	// FileSortMode
	FILESORT_NAME	= 0;
	FILESORT_SIZE	= 1;
	FILESORT_DATE	= 2;

	FILE_EXPLORE	= 3;
	FILE_RENAME		= 4;
	FILE_COPY		= 5;
	FILE_MOVE		= 6;
	FILE_DELETE		= 7;

type
	TFileList = class(TCWEList)
		function	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;
	end;

	TDirList = class(TCWEList)
		function 	GetPath: String; inline;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;
	end;

	TFileScreen = class(TCWEScreen)
	class var
		COLOR_FILE_PT,
		COLOR_FILE_NONPT,
		COLOR_FILE_SAMPLE,
		COLOR_FILE_EMPTYSAMPLE,
		COLOR_FILE_SIZE,
		COLOR_FILE_DATE,
		COLOR_FG_BOOKMARK,
		COLOR_FG_DRIVE,
		COLOR_FG_PARENTDIR,
		COLOR_FG_DIR	: Byte;
	private
		PrevFile:	String;
		BookmarkFile: String;

		function 	SearchHandler(var Key: Integer; Shift: TShiftState): Boolean;
		procedure 	SearchTermChanged;
	public
		Directory:	String;

		Bookmarks:	TStringList;

		SaveMode:	Boolean;
		SortMode:   Byte;
		InModule:	Boolean;

		FileList:	TFileList;
		DirList:	TDirList;
		FilenameEdit,
		DirEdit:	TCWEEdit;
		lblSearchHint,
		lblSearch,
		lblHeader:	TCWELabel;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
		destructor	Destroy; override;

		procedure 	HandleCommand(const Cmd: Cardinal); override;

		procedure 	ListDirs(var List: TCWEList; const Directory, PrevDir: String);
		procedure 	FileOrDirSelected(Sender: TCWEControl); dynamic; abstract;
		procedure 	DirOrFilenameEntered(Sender: TCWEControl); dynamic; abstract;
		procedure 	SortFiles;

		procedure 	LoadFile(const Filename: String); dynamic; abstract;
		procedure 	SaveFile(DoDialog: Boolean = True); dynamic; abstract;
		procedure	DeleteFile(DoDialog: Boolean = True);
		procedure	DeleteDir(DoDialog: Boolean = True);
		procedure 	CopyFile(const DestDir: String);

		procedure 	Show(aSaveMode: Boolean; const Dir: String); reintroduce;
		procedure 	SetDirectory(Dir: String); dynamic; abstract;

		procedure	LoadBookmarks(const Filename: String);
		procedure	SaveBookmarks;
		procedure	MoveBookmark(const Path: String; MoveDown: Boolean);
		procedure	AddBookmark(Path: String; Index: Integer = -1);
		procedure	RemoveBookmark(const Path: String);
		procedure 	BookmarksChanged(Recreate: Boolean = True);
	end;

	TModFileScreen = class(TFileScreen)
	private
	public
		procedure 	FileOrDirSelected(Sender: TCWEControl); override;

		procedure 	LoadModule(const Filename: String);
		procedure 	LoadFile(const Filename: String); override;
		procedure 	SaveFile(DoDialog: Boolean = True); override;

		procedure 	DirOrFilenameEntered(Sender: TCWEControl); override;
		procedure 	SetDirectory(Dir: String); override;

		procedure 	Paint; override;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;

var
	FileScreen: TFileScreen;
	FileRequester: TModFileScreen;


implementation

uses
	SDL2,
    Generics.Defaults, FileUtils, FileUtil,
	{$IFDEF WINDOWS}
	Windows,
	{$ENDIF}
	Layout, ShortcutManager,
	CWE.Dialogs, Dialog.ValueQuery,
	Screen.FileReqSample,
	ProTracker.Util,
	ProTracker.Editor,
	ProTracker.Player,
	ProTracker.Sample,
	Screen.Samples,
	Screen.Editor,
	MainWindow;

// ==========================================================================
{ Utility }
// ==========================================================================

function InSampleReq: Boolean; inline;
begin
	Result := (CurrentScreen = SampleRequester);
end;

function SortAlphabetically(constref L, R: TCWEListItem): Integer;
begin
	Result := CompareText(L.Captions[0], R.Captions[0]);
end;

function SortByColumn(constref L, R: TCWEListItem): Integer;
begin
	Result := CompareText(
		R.Captions[FileScreen.SortMode],
		L.Captions[FileScreen.SortMode]);
end;

// ==========================================================================
{ TFileScreen }
// ==========================================================================

constructor TFileScreen.Create(var Con: TConsole; const sCaption, sID: AnsiString);
var
	H, W: Integer;
begin
	inherited;

	Bookmarks := TStringList.Create;
	Bookmarks.Duplicates := dupIgnore;
	Bookmarks.CaseSensitive := False;

	W := 54;
	if (Self is TModFileScreen) then
		H := Console.Height - 4
	else
		H := Console.Height - 11;

	FileList := TFileList.Create(Self, '', 'File List',
		Types.Rect(1, 3, W, Console.Height-4), True);
	DirList  := TDirList.Create(Self, '', 'Directory List',
		Types.Rect(W+2, 3, Console.Width-2, H), True);
	DirEdit  := TCWEEdit.Create(Self, '', 'Directory Edit',
		Bounds(1, Console.Height-3, W, 1), True);
	FilenameEdit  := TCWEEdit.Create(Self, '', 'Filename Edit',
		Bounds(1, Console.Height-2, W, 1), True);

	FileList.Columns := 3;
	FileList.ColumnWidth[1] := 8;
	FileList.ColumnWidth[2] := 17;
	FileList.SetBorder(True, True, True, True);
	FileList.SetData(2, 2, 'File size color');

	DirEdit.Border.Enabled := False;
	DirEdit.WantHover := True;
	FilenameEdit.Border.Enabled := False;
	FilenameEdit.WantHover := True;
	DirList.Border.Pixel := True;

	TCWEBorder.Create(Self, '', '', Types.Rect(
		DirEdit.Rect.Left, DirEdit.Rect.Top, FilenameEdit.Rect.Right, FilenameEdit.Rect.Bottom));

	lblSearchHint := TCWELabel.Create(Self, 'Type to search:', 'SearchHint',
		Types.Rect(52+4, Console.Height-3, Console.Width-1, Console.Height-2));
	lblSearch := TCWELabel.Create(Self, '', 'Search',
		Types.Rect(52+4, Console.Height-2, Console.Width-1, Console.Height-1));
	RegisterLayoutControl(TCWEControl(lblSearchHint), CTRLKIND_LABEL, False, True, False);
	RegisterLayoutControl(TCWEControl(lblSearch),     CTRLKIND_LABEL, False, True, False);

	RegisterLayoutControl(TCWEControl(FileList),  CTRLKIND_BOX, False, True, True);
	RegisterLayoutControl(TCWEControl(DirList),   CTRLKIND_BOX, False, True, True);
	RegisterLayoutControlClass(TCWEControl(Self), TCWEEdit, CTRLKIND_BOX, False, True, False);
	//RegisterLayoutControl(TCWEControl(Ctrl),      CTRLKIND_BOX, False, True, True);

	lblHeader := AddHeader('');
	RegisterLayoutControl(TCWEControl(lblHeader), CTRLKIND_LABEL, False, True, False);
end;

destructor TFileScreen.Destroy;
begin
	if Bookmarks <> nil then
	begin
		SaveBookmarks;
		Bookmarks.Free;
	end;
	inherited;
end;

procedure TFileScreen.DeleteFile(DoDialog: Boolean = True);
var
	Filename: String;
begin
	Filename := Directory + ValidateFilename(FilenameEdit.Caption);

	if (Filename = '') or (FileExists(Filename) = False) then
	begin
		Log(TEXT_ERROR + 'Cannot delete "%s": File not found!', [Filename]);
		Exit;
	end;

	if DoDialog then
	begin
		ModalDialog.MessageDialog(ACTION_DELETEFILE, 'Delete File',
			'Delete selected file?',
			[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0);
	end
	else
	begin
		DeleteToBin(Filename);
		SetDirectory(DirEdit.Caption);
	end;
end;

procedure TFileScreen.DeleteDir(DoDialog: Boolean = True);
var
	Dir: String;
begin
	Dir := IncludeTrailingPathDelimiter(Directory) +
		DirList.Items[DirList.ItemIndex].Captions[0];

	if (Dir = '') or (DirectoryExists(Dir) = False) then
	begin
		Log(TEXT_ERROR + 'Cannot delete "%s": Directory not found!', [Dir]);
		Exit;
	end;

	if DoDialog then
	begin
		ModalDialog.MessageDialog(ACTION_DELETEDIR, 'Delete Directory',
			'Delete selected directory?',
			[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0);
	end
	else
	begin
		DeleteToBin(Dir);
		SetDirectory(DirEdit.Caption);
	end;
end;

procedure TFileScreen.CopyFile(const DestDir: String);
var
	DestFile, Filename: String;
begin
	if (DestDir = '') or (DirectoryExists(DestDir) = False) then
	begin
		Log(TEXT_ERROR + 'Cannot copy file: Directory "%s" not found!', [DestDir]);
		Exit;
	end;

	Filename := ValidateFilename(FilenameEdit.Caption);
	DestFile := IncludeTrailingPathDelimiter(DestDir)   + Filename;
	Filename := IncludeTrailingPathDelimiter(Directory) + Filename;

	if FileExists(Filename) then
	begin
		if FileExists(DestFile) then
			ModalDialog.ShowMessage('Copy Error', 'Destination file already exists!')
		else
		begin
			if FileUtil.CopyFile(Filename, DestFile, True) then
				ModalDialog.ShowMessage('File Copy', 'File copied to ' + DestDir + '.')
			else
				ModalDialog.ShowMessage('Copy Error', 'Error copying file!')
		end;
	end;
end;

procedure TFileScreen.Show(aSaveMode: Boolean; const Dir: String);
begin
	if ModalDialog.Dialog <> nil then Exit;

	FileScreen := Self;
	SaveMode := aSaveMode;

	if (Self = FileRequester) or (Dir <> Directory) then
		SetDirectory(Dir);

	ChangeScreen(TCWEScreen(Self));

	if lblSearch <> nil then
		lblSearch.SetCaption(''); // clear search
end;

procedure TFileScreen.ListDirs(var List: TCWEList; const Directory, PrevDir: String);
var
	sr: TSearchRec;
	ld: DWord;
	i: Integer;
	li: TCWEListItem;
begin
	List.Clear;

	// list subdirs
	//
	if SysUtils.FindFirst(Directory + '*', faDirectory, sr) = 0 then
	repeat
		if ((sr.Attr and faDirectory) = faDirectory)
			and (sr.Name <> '.') and (sr.Name <> '..') then
				List.Items.Add(TCWEListItem.Create(
					sr.Name, LISTITEM_DIR, nil, COLOR_FG_DIR));
	until (SysUtils.FindNext(sr) <> 0);
	SysUtils.FindClose(sr);

	// sort list alphabetically !!!
	List.Items.Sort(TComparer<TCWEListItem>.Construct(SortAlphabetically));

	li := TCWEListItem.Create(STR_DIRECTORYUP, LISTITEM_HEADER);
	li.ColorFore := COLOR_FG_PARENTDIR;
	li.ColorBack := TConsole.COLOR_BLANK;
	List.Items.Insert(0, li); // up a dir

	{$IFDEF WINDOWS}
	ld := GetLogicalDrives;
	for i := 25 downto 0 do
		if (ld and (1 shl i)) > 0 then
		begin
			li := TCWEListItem.Create(Char(Ord('A') + i) + ':\', LISTITEM_DRIVE);
			li.ColorFore := COLOR_FG_DRIVE;
			List.Items.Insert(0, li);
		end;
	{$ELSE}
	li := TCWEListItem.Create(GetUserDir, LISTITEM_DRIVE, nil, COLOR_FG_DRIVE);
    List.Items.Insert(0, li);
	li := TCWEListItem.Create('/', LISTITEM_DRIVE, nil, COLOR_FG_DRIVE);
    List.Items.Insert(0, li);
	{$ENDIF}

	BookmarksChanged(False);
	List.AdjustScrollbar;

	for i := 0 to List.Items.Count-1 do
	begin
		li := List.Items[i];
		if li.Data = LISTITEM_BOOKMARK then
			Continue
		else
		if li.Data = LISTITEM_HEADER then
			List.Select(i)
		else
		if li.Captions[0] = PrevDir then
		begin
			List.Select(i);
			Break;
		end;
	end;
end;

procedure TFileScreen.SortFiles;
begin
	if SortMode = FILESORT_NAME then
		FileList.Items.Sort(TComparer<TCWEListItem>.Construct(SortAlphabetically))
	else
		FileList.Items.Sort(TComparer<TCWEListItem>.Construct(SortByColumn));

	if PrevFile <> '' then
		FileList.Select(PrevFile);
end;

// This handles keypresses that KeyPress event doesn't
// (TODO see if this could be consolidated into KeyPress)
function TFileScreen.SearchHandler(var Key: Integer; Shift: TShiftState): Boolean;
begin
	if (Key = SDLK_BACKSPACE) and (lblSearch <> nil) then
	begin
		if ssCtrl in Shift then
			lblSearch.Caption := ''
		else
		if lblSearch.Caption <> '' then
			lblSearch.Caption := Copy(lblSearch.Caption, 1, Length(lblSearch.Caption)-1);

		SearchTermChanged;
		Result := True;
	end
	else
		Result := False;
end;

// Do a search for the typed in search string, starting from selected item
// then wrapping back to the first row if no matches were found; select the first match if any
procedure TFileScreen.SearchTermChanged;
var
	ST: AnsiString;
	Y: Integer;
begin
	if lblSearch = nil then Exit;

	lblSearch.Paint;
	if lblSearch.Caption = '' then Exit;

	ST := LowerCase(lblSearch.Caption);

	for Y := 0 to FileList.Items.Count-1 do
	begin
		if Pos(ST, LowerCase(FileList.Items[Y].Captions[0])) = 1 then
		begin
			// found match, select it!
			FileList.Select(Y);
			FileOrDirSelected(FileList);
			Exit;
		end;
	end;
end;

procedure TFileScreen.HandleCommand(const Cmd: Cardinal);
var
	IsFile, IsFolder: Boolean;
	Dir: String;
begin
	IsFolder := DirList.Focused;
	IsFile   := FileList.Focused;

	case Cmd of

		FILESORT_NAME, FILESORT_SIZE, FILESORT_DATE:
		begin
			SortMode := Cmd;
			if InSampleReq then
			begin
				Options.Dirs.SampleSortMode := SortMode;
				if SampleRequester.InModule then Exit;
			end
			else
				Options.Dirs.FileSortMode := SortMode;
			SetDirectory(Directory);
		end;

		FILE_EXPLORE:
			if FileScreen.InModule then
				SelectFileInExplorer(SampleRequester.ModFilename)
			else
				SelectFileInExplorer(
					IncludeTrailingPathDelimiter(DirEdit.Caption) +
					FilenameEdit.Caption);

		FILE_COPY:
			if IsFolder then
			begin
				case DirList.Items[DirList.ItemIndex].Data of
					LISTITEM_BOOKMARK:
						Dir := Bookmarks[DirList.ItemIndex];
					LISTITEM_DIR, LISTITEM_HEADER, LISTITEM_DRIVE:
						Dir := DirList.GetPath;
				else
					Exit;
				end;
				if Dir <> '' then
					CopyFile(Dir);
			end
			else
				CopyFile(DirList.GetPath);


		FILE_DELETE:
			if IsFolder then
			begin
				case DirList.Items[DirList.ItemIndex].Data of
					LISTITEM_BOOKMARK:
						RemoveBookmark(Bookmarks[DirList.ItemIndex]);
					LISTITEM_DRIVE:
						ModalDialog.ShowMessage('Delete',
							'Can''t delete a drive!');
					LISTITEM_DIR:
						DeleteDir(True);
				end;
			end
			else
			if IsFile then
				DeleteFile(True);

		FILE_RENAME:
		;

		FILE_MOVE:
		;

	end;
end;

// ==========================================================================
{ Bookmarks }
// ==========================================================================

procedure TFileScreen.LoadBookmarks(const Filename: String);
begin
	BookmarkFile := Filename;
	if FileExists(Filename) then
		Bookmarks.LoadFromFile(Filename);
end;

procedure TFileScreen.SaveBookmarks;
begin
	Bookmarks.SaveToFile(BookmarkFile);
end;

procedure TFileScreen.MoveBookmark(const Path: String; MoveDown: Boolean);
var
	i, n: Integer;
begin
	i := Bookmarks.IndexOf(Path);
	if i >= 0 then
	begin
		if MoveDown then
			n := +1
		else
			n := -1;
		if (i+n >= 0) and (i+n < Bookmarks.Count) then
		begin
			Bookmarks.Move(i, i+n);
			BookmarksChanged;
			DirList.ItemIndex := i+n;
			DirList.Paint;
		end;
	end;
end;

procedure TFileScreen.AddBookmark(Path: String; Index: Integer = -1);
begin
	if Bookmarks.IndexOf(Path) >= 0 then Exit;

	{AskString(ACTION_ASKED_STRING, 'Input value:',
		Key, True, TValQuery.DialogCallback);}

	if Index < 0 then
		Bookmarks.Add(Path)
	else
		Bookmarks.Insert(Index, Path);
	BookmarksChanged;
end;

procedure TFileScreen.RemoveBookmark(const Path: String);
var
	i: Integer;
begin
	i := Bookmarks.IndexOf(Path);
	if i >= 0 then Bookmarks.Delete(i);
	BookmarksChanged;
end;

procedure TFileScreen.BookmarksChanged(Recreate: Boolean = True);
var
	i: Integer;
	li: TCWEListItem;
	S, SS: String;
begin
	if Recreate then
	for i := DirList.Items.Count-1 downto 0 do
	begin
		li := DirList.Items[i];
		if li.Data = LISTITEM_BOOKMARK then
			DirList.Items.Delete(i);
	end;

	for i := Bookmarks.Count-1 downto 0 do
	begin
		SS := ExcludeTrailingPathDelimiter(Bookmarks[i]);
		S := ExtractFileName(SS);
		if S = '' then S := SS;

		li := TCWEListItem.Create(S, LISTITEM_BOOKMARK);
		li.ColorFore := COLOR_FG_BOOKMARK;

		DirList.Items.Insert(0, li);
	end;

	if Recreate then
	begin
		DirList.AdjustScrollbar;
		DirList.Paint;
	end;
end;

// ==========================================================================
{ TModFileScreen }
// ==========================================================================

constructor TModFileScreen.Create(var Con: TConsole; const sCaption, sID: AnsiString);
begin
	RegisterScreenLayout(Self, 'FileRequester');

	inherited;

	LoadBookmarks(ConfigPath + 'bookmarks-modules.ini');
	SortMode := Options.Dirs.FileSortMode;

	FileList.OnChange		:= FileOrDirSelected;
	DirList.OnChange		:= FileOrDirSelected;
	DirEdit.OnChange		:= DirOrFilenameEntered;
	FilenameEdit.OnChange	:= DirOrFilenameEntered;

	SaveMode := False;
	LoadLayout(Self);

//	FileList.Selection3D := True;
//	FileList.Data[2].Value := 255;	// disable 3d border on selection

	ActiveControl := FileList;
end;

procedure TModFileScreen.LoadModule(const Filename: String);
begin
	if (Module <> nil) and (Module.Modified) then
	begin
		ModalDialog.MessageDialog(ACTION_LOADMODULE,
			'Load Module', 'Current module not saved. Proceed?',
			[btnOK, btnCancel], btnCancel, Window.DialogCallback, Filename);
	end
	else
		Window.DoLoadModule(Filename);
end;

procedure TModFileScreen.SaveFile(DoDialog: Boolean = True);
var
	Filename: String;
const
	S_OW = 'Overwrite file?';
begin
	Filename := Directory + ValidateFilename(FilenameEdit.Caption);

	if (DoDialog) and (FileExists(Filename)) then
	begin
		if not InSampleReq then
			ModalDialog.MessageDialog(ACTION_SAVEFILE, 'Save Module', S_OW,
				[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0)
		else
			ModalDialog.MessageDialog(ACTION_SAVEFILE, 'Save Sample', S_OW,
				[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0);
	end
	else
		PatternEditor.SaveModule(Filename);
end;

procedure TModFileScreen.LoadFile(const Filename: String);
begin
	if ValidFilename(Filename) then
		LoadModule(Filename)
	else
		ModalDialog.ShowMessage('Load Module', 'Invalid filename!');
end;

procedure TModFileScreen.DirOrFilenameEntered;
begin
	if Sender = DirEdit then
		SetDirectory(DirEdit.Caption)
	else
	begin
		if SaveMode then
			SaveFile
		else
			LoadFile(Directory + FilenameEdit.Caption);
	end;
end;

procedure TModFileScreen.FileOrDirSelected;
var
	S: String;
begin
	if Sender = FileList then
	begin
		S := FileList.GetCaption;
		FilenameEdit.SetCaption(S);
		PrevFile := S;
	end;
end;

procedure TModFileScreen.Paint;
begin
	FileList.ColumnColor[1] := COLOR_FILE_SIZE;
	FileList.ColumnColor[2] := COLOR_FILE_DATE;

	inherited;

	if SaveMode then
		lblHeader.SetCaption('Save Module' + Shortcuts.GetShortcut('', 'Screen.Save', True))
	else
		lblHeader.SetCaption('Load Module' + Shortcuts.GetShortcut('', 'Screen.Load', True));

	FileList.Paint;
end;

procedure TModFileScreen.SetDirectory(Dir: String);
var
	sr: TSearchRec;
	IsPT, IsMod: Boolean;
	ssL, ssR, PrevDir, lFilename, Filename: String;
	li: TCWEListItem;
	x: Integer;
label
	Done;
begin
	lblSearch.SetCaption(''); // clear search

	if (Dir = '') or (not DirectoryExists(Dir)) then
	begin
		FileList.Clear;
		Exit;
	end;

	PrevDir := ExtractFileName(ExcludeTrailingPathDelimiter(Directory));

	if (IncludeTrailingPathDelimiter(Dir) <> Directory) then
	begin
		PrevFile := '';
		Directory := IncludeTrailingPathDelimiter(Dir);
	end;

	FileList.Clear;
	DirEdit.SetCaption(Directory);

	Options.Dirs.Modules := Directory;

	// list files in current dir
	//
	if SysUtils.FindFirst(Directory + '*', faAnyFile, sr) = 0 then
	begin
		repeat
			if (sr.Attr and faDirectory) = 0 then
			begin
				lFilename := LowerCase(sr.Name);
				Filename := StringReplace(lFilename, '~', '', []);

				x := Pos('.', Filename);
				if x <= 1 then Continue;

				ssL := Copy(Filename, 1, x-1);
				ssR := Copy(Filename, x+1, MAXINT);

				IsPT := (Pos('[' + ssL + ']', AmigaExts) > 0);
				if not IsPT then
					IsPT := (Pos('[' + ssR + ']', AmigaExts) > 0);

				if IsPT then
					IsMod := True
				else
					IsMod := (Pos('[' + ssR + ']', Extensions_Module) > 0);

				if (IsMod) and (sr.Size > 0) then
				begin
					{if sr.Size < 1024 then
						SizeS := Format('%6.1d B', [sr.Size])
					else
					if sr.Size >= 1024 * 1024 then
						SizeS := Format('%6.1f M', [sr.Size / (1024*1024)])
					else
						SizeS := Format('%6.1f K', [sr.Size / 1024]);}

					if Options.Dirs.RawFileSizes then
						ssL := Format('%8d', [sr.Size])
					else
						ssL := Format('%6.1f K', [sr.Size / 1024]);

					li := TCWEListItem.Create(
						sr.Name + COLUMNSEPARATOR +
						ssL + COLUMNSEPARATOR +
						FormatDateTime('yyyy-mm-dd hh:nn', FileDateToDateTime(sr.Time)));

					if IsPT then
						li.ColorFore := COLOR_FILE_PT
					else
						li.ColorFore := COLOR_FILE_NONPT;

					FileList.Items.Add(li);
				end;
			end;
		until SysUtils.FindNext(sr) <> 0;
		SysUtils.FindClose(sr);

		SortFiles;
	end;

	// list drives
	//
	ListDirs(TCWEList(DirList), Directory, PrevDir);

Done:
	DirList.Change;
	FileList.Change;
	FileList.AdjustScrollbar;

	Paint;
end;

// ==========================================================================
{ TFileList }
// ==========================================================================

function TFileList.TextInput(var Key: Char): Boolean;
begin
{
    if (Ord(Key) < 32) or (HiWord(GetKeyState(SDLK_CONTROL)) <> 0) then Exit(False); // !!!
	if (InSampleReq)  and (HiWord(GetKeyState(SDLK_SHIFT))   =  0) then Exit(False);
}
	with FileScreen do
		if lblSearch <> nil then
		begin
			lblSearch.Caption := lblSearch.Caption + LowerCase(Key);
			SearchTermChanged;
		end;
	Result := True;
end;

function TFileList.KeyDown;

	function GetFilename: String;
	begin
		Result := GetCaption;
		if Result <> '' then
			Result := FileScreen.Directory + Result;
	end;

var
	S: TSample;
	i: Integer;
	Sc: ControlKeyNames;
	Scn: EditorKeyNames;
begin
	if InSampleReq then
		FileScreen := SampleRequester
	else
		FileScreen := FileRequester;

	Result := True;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));
	case Sc of

		ctrlkeyINSERT:
			FileScreen.AddBookmark(FileScreen.Directory);

		ctrlkeyRETURN:
			if not InSampleReq then			// Load/save module
			begin
				if FileRequester.SaveMode then
					FileRequester.SaveFile
				else
					FileRequester.LoadFile(GetFilename);
			end
			else
			case Items[ItemIndex].Data of	// load/save sample

				LI_LOADABLE:
					if SampleRequester.InModule then
					begin
						S := TSample(Items[ItemIndex].ObjData);
						if (S <> nil) and (S.Length > 0) then
						begin
							Module.Samples[CurrentSample-1].Assign(S);
							SampleScreen.AfterSampleLoaded(S);
						end;
					end
					else
						SampleRequester.DirOrFilenameEntered(nil);

				LI_ENTERABLE:
					with SampleRequester do
					if not SaveMode then	// list module samples
					begin
						if SampleMod <> nil then
							FreeAndNil(SampleMod);

						SampleMod := TPTModule.Create(True);
						ModFilename := GetFilename;

						if SampleMod.LoadFromFile(ModFilename) then
						begin
							InModule := True;
							SetDirectory('');
						end
						else
							ModFilename := '';
					end;

				LI_UNENTER:
					SampleRequester.SetDirectory(SampleRequester.Directory);

			end; // case


		ctrlkeyDELETE:
			FileScreen.DeleteFile(True);

		ctrlkeyRIGHT:
			FileScreen.ActivateControl(FileScreen.DirList);

	else
		Result := False;

		// Play notes
		//
		if (InSampleReq) and (Shift = []) then
		begin
			Scn := EditorKeyNames(Shortcuts.Find(EditorKeys, Key, Shift));
			case Scn of

				keyNoteC_lo..keyNoteB_hi:
				begin
					i := Integer(Scn) - Integer(keyNoteC_lo) + (Integer(PatternEditor.HighOctave) * 12);
					if i < 36 then // go no higher than B-3
					begin
						Module.PlaySample(i, Module.Samples.Count+0 , PatternEditor.Cursor.Channel);
						Result := True;
					end;
				end;

				keySelectOctaveLo,
				keySelectOctaveHi:
				begin
					Editor.SetOctave(Scn = keySelectOctaveHi);
					Result := True;
				end;
			end;
		end; // InSamplereq
	end;

	if not Result then
	begin
		Result := inherited KeyDown(Key, Shift);
		if not Result then
			Result := FileScreen.SearchHandler(Key, Shift);
	end;
end;

// ==========================================================================
{ TDirList }
// ==========================================================================

// typing here will search for a filename
function TDirList.TextInput(var Key: Char): Boolean;
begin
	if Items[ItemIndex].Data = LISTITEM_BOOKMARK then
		Exit(False);

	Result := True;
	FileRequester.FileList.TextInput(Key);
end;

function TDirList.GetPath: String;
begin
	Result := Items[ItemIndex].Captions[0];
end;

function TDirList.KeyDown;
var
	Dir: String;
	Scr: TFileScreen;
	Sc: ControlKeyNames;
begin
	Result := True;
	Scr := TFileScreen(Screen);
	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	case Sc of

		ctrlkeyDELETE:
			Scr.HandleCommand(FILE_DELETE);

		ctrlkeyINSERT:
		begin
			if Shift = [ssShift] then
				FileScreen.HandleCommand(FILE_COPY)
			else
			case Items[ItemIndex].Data of // no Shift
				LISTITEM_BOOKMARK:
					Scr.AddBookmark(Scr.Directory, ItemIndex);
				LISTITEM_DRIVE,
				LISTITEM_DIR:
					Scr.AddBookmark(GetPath);
			end;
		end;

		ctrlkeyPLUS,
		ctrlkeyMINUS:
			if Items[ItemIndex].Data = LISTITEM_BOOKMARK then
				Scr.MoveBookmark(Scr.Bookmarks[ItemIndex], (Sc = ctrlkeyPLUS));

		ctrlkeyRETURN:	// enter directory
		begin
			case Items[ItemIndex].Data of
				LISTITEM_BOOKMARK:
				begin
					Dir := Scr.Bookmarks[ItemIndex];
					Scr.SetDirectory(Dir);
					Exit;
				end;
				LISTITEM_DIR, LISTITEM_HEADER:
					Dir := GetPath;
				LISTITEM_DRIVE:
				begin
					Dir := GetPath;
					Scr.SetDirectory(Dir);
					Exit;
				end;
			else
				Exit;
			end;

			with Scr do
			begin
				if Dir = STR_DIRECTORYUP then
					SetDirectory(
						ExpandFileName(IncludeTrailingPathDelimiter(Directory)
						+ '..' + PathDelim))
				else
					SetDirectory(Directory + Dir);
			end;
		end;

		ctrlkeyLEFT:
			Scr.ActivateControl(Scr.FileList);

	else
		Result := inherited KeyDown(Key, Shift);
		if not Result then
			Result := Scr.SearchHandler(Key, Shift);
	end;
end;


end.

