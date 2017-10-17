unit Screen.FileReq;

interface

uses
	Classes, Types, SysUtils, contnrs,
	ShortcutManager,
	TextMode, CWE.Core, CWE.Dialogs, CWE.Widgets.Text;

const
	AmigaExts  = '[mod][p61][stk][nst][ust][nt][m15]';
	Extensions_Module = '[mod][it][p61]'; // [.xm][.s3m]

	STR_DIRECTORYUP = '<Parent>';
	STR_BOOKMARKS   = '<Bookmarks>';

	LISTITEM_BOOKMARK	= 1337;
	LISTITEM_DIR		= 1338;
	LISTITEM_DRIVE		= 1339;
	LISTITEM_PARENT		= LISTITEM_HEADER;

	// FileSortMode
	FILESORT_NAME	= 0;
	FILESORT_SIZE	= 1;
	FILESORT_DATE	= 2;

	FILE_EXPLORE	= 3;
	FILE_RENAME		= 4;
	FILE_COPY		= 5;
	FILE_MOVE		= 6;
	FILE_DELETE		= 7;
	FILE_CREATEDIR	= 8;
	FILE_BOOKMARK	= 9;

	FILE_MERGEMODULE = 10;

type
	FileOpKeyNames = (
		filekeyNONE,
		filekeyRename,
		filekeyCopy,
		filekeyMove,
		filekeyDelete,
		filekeyCreate,
		filekeyModMerge
	);

	TBookmark = class
		Path,
		Name:	String;
	end;

	TBookmarkList = class(TObjectList)
	private
		function	Get(Index: Integer): TBookmark;
		procedure 	Put(Index: Integer; AValue: TBookmark);
	public
		function	IndexOf(APath: String): Integer;
		function	Add(ABookmark: TBookmark): Integer; overload;
		procedure	LoadFromFile(const Filename: String);
		procedure	SaveToFile(const Filename: String);
		property 	Items[Index: Integer]: TBookmark read Get write Put; default;
	end;

	TFileList = class(TCWEList)
		function	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;
	end;

	TDirList = class(TCWEList)
		function 	GetPath(Fullpath: Boolean): String; inline;

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

		procedure 	RenameCallback(ID: Word; ModalResult: TDialogButton;
					Tag: Integer; Data: Variant; Dlg: TCWEDialog);
		function	SearchHandler(var Key: Integer; Shift: TShiftState): Boolean;
		procedure	SearchTermChanged;
	public
		Directory:	String;

		Bookmarks:	TBookmarkList;

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
		function 	OnContextMenu: Boolean; override;

		procedure 	ListDirs(var List: TCWEList; const Directory, PrevDir: String);
		procedure 	FileOrDirSelected(Sender: TCWEControl); dynamic; abstract;
		procedure 	DirOrFilenameEntered(Sender: TCWEControl); dynamic; abstract;
		procedure 	SortFiles;
		function	GetSelectedFile(FullPath: Boolean = True;
					WantTypedName: Boolean = False): String;
		procedure	SelectFile(Filename: String);

		procedure 	LoadFile(const Filename: String); dynamic; abstract;
		procedure 	SaveFile(DoDialog: Boolean = True); dynamic; abstract;
		procedure	DeleteFile(DoDialog: Boolean = True);
		procedure	DeleteDir(DoDialog: Boolean = True);
		procedure 	CopyFile(const DestDir: String; MoveFile: Boolean = False);

		procedure 	Show(aSaveMode: Boolean; const Dir: String); reintroduce; virtual;
		procedure 	SetDirectory(Dir: String); dynamic; abstract;

		procedure	LoadBookmarks(const Filename: String);
		procedure	SaveBookmarks;
		procedure 	RenameBookmark(const Path, NewName: String);
		procedure	MoveBookmark(const Path: String; MoveDown: Boolean);
		procedure	AddBookmark(Path: String; Index: Integer = -1);
		procedure	RemoveBookmark(const Path: String);
		procedure 	BookmarksChanged(Recreate: Boolean = True);

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
	end;

	TModFileScreen = class(TFileScreen)
	public
		procedure 	FileOrDirSelected(Sender: TCWEControl); override;

		procedure 	LoadModule(const Filename: String);
		procedure 	LoadFile(const Filename: String); override;
		procedure 	SaveFile(DoDialog: Boolean = True); override;

		procedure 	DirOrFilenameEntered(Sender: TCWEControl); override;
		procedure 	SetDirectory(Dir: String); override;

		procedure 	Paint; override;
		procedure 	Show(aSaveMode: Boolean; const Dir: String); override;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;

var
	FileScreen: TFileScreen;
	FileRequester: TModFileScreen;
	FileOpKeys: TKeyBindings;


	function GetParentDir(const Dir: String): String; inline;


implementation

uses
	SDL.Api.Types,
    Generics.Defaults, FileUtils, FileUtil,
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	Layout, CWE.MainMenu,
	Dialog.ValueQuery,
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

function GetParentDir(const Dir: String): String;
begin
	Result := ExpandFileName(
		IncludeTrailingPathDelimiter(Dir) + '..' + PathDelim);
end;

// ==========================================================================
{ TFileScreen }
// ==========================================================================

constructor TFileScreen.Create(var Con: TConsole; const sCaption, sID: AnsiString);
var
	H, W: Integer;
begin
	inherited;

	Bookmarks := TBookmarkList.Create;

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

function TFileScreen.OnContextMenu: Boolean;

	procedure AddCopyMove;
	begin
		ContextMenu.AddCmdEx(FILE_COPY,		'Copy file here');
		ContextMenu.AddCmdEx(FILE_MOVE, 	'Move file here');
	end;

begin
	inherited;
	with ContextMenu do
	begin
		SetSection(EditorKeys);

		if (FileList.Focused) or (DirList.Focused) then
		begin
			if FileList.Focused then
			begin
				AddSection('File listing');

				AddCmdEx(FILESORT_NAME,				'Sort by name');
				AddCmdEx(FILESORT_SIZE,				'Sort by size');
				AddCmdEx(FILESORT_DATE,				'Sort by date');
				{$IFDEF WINDOWS}
				AddCmdEx(FILE_EXPLORE,				'Show file in Explorer');
				{$ENDIF}

				AddSection('File operations');

				AddCmdEx(FILE_RENAME, 				'Rename file');
				AddCmdEx(FILE_COPY,					'Copy file to directory');
				AddCmdEx(FILE_MOVE, 				'Move file to directory');
				AddCmdEx(FILE_DELETE, 				'Delete file');

				if (Self = FileRequester) and (not SaveMode) and (not InModule) then
				begin
					AddSection('Module');
					AddCmdEx(FILE_MERGEMODULE,		'Merge into current');
				end;
			end
			else
			if DirList.Focused then
			begin
				case DirList.Items[DirList.ItemIndex].Data of

					LISTITEM_BOOKMARK:
					begin
						AddSection('Bookmark operations');
						AddCopyMove;

						AddCmdEx(FILE_BOOKMARK, 			'Bookmark directory');
						AddCmdEx(FILE_RENAME, 				'Rename bookmark');
						AddCmdEx(FILE_DELETE, 				'Delete bookmark');
					end;

					LISTITEM_DRIVE:
					begin
						{$IFDEF WINDOWS}
						AddSection('Drive operations');
						AddCopyMove;
						AddCmdEx(FILE_BOOKMARK, 			'Bookmark drive');
						{$ELSE}
						AddSection('Directory operations');
						AddCopyMove;
						AddCmdEx(FILE_BOOKMARK, 			'Bookmark directory');
						{$ENDIF}
					end;

					LISTITEM_DIR:
					begin
						AddSection('Directory operations');
						AddCopyMove;

						AddCmdEx(FILE_RENAME, 				'Rename directory');
						AddCmdEx(FILE_CREATEDIR, 			'Create directory');
						{$IFDEF WINDOWS}
						AddCmdEx(FILE_DELETE, 				'Delete directory');
						{$ENDIF}
						AddCmdEx(FILE_BOOKMARK, 			'Bookmark directory');
					end;

				end;
			end;

			Result := False;
		end;
	end;
end;

function TFileScreen.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
begin
	Result := True;
	case FileOpKeyNames(Shortcuts.Find(FileOpKeys, Key, Shift)) of
		filekeyDelete:	HandleCommand(FILE_DELETE);
		filekeyCopy:	HandleCommand(FILE_COPY);
		filekeyMove:	HandleCommand(FILE_MOVE);
		filekeyRename:	HandleCommand(FILE_RENAME);
		filekeyCreate:	HandleCommand(FILE_CREATEDIR);
		filekeyModMerge:HandleCommand(FILE_MERGEMODULE);
	else
		Result := inherited;
	end;
end;

function TFileScreen.GetSelectedFile(FullPath: Boolean = True;
	WantTypedName: Boolean = False): String;
begin
	if (SaveMode) and (WantTypedName) and (FilenameEdit.Focused) then
		Result := ValidateFilename(FilenameEdit.Caption)
	else
		Result := ValidateFilename(FileList.GetCaption);
	if FullPath then
		Result := IncludeTrailingPathDelimiter(Directory) + Result;
end;

procedure TFileScreen.DeleteFile(DoDialog: Boolean = True);
var
	Filename: String;
begin
	Filename := GetSelectedFile;

	if (Filename = '') or (FileExists(Filename) = False) then
	begin
		Log(TEXT_ERROR + 'Cannot delete "%s": File not found!', [Filename]);
		Exit;
	end;

	if DoDialog then
	begin
		ModalDialog.MessageDialog(ACTION_DELETEFILE, 'Delete File',
			Format('Delete "%s"?', [ExtractFilename(Filename)]),
			[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0);
	end
	else
	begin
		DeleteToBin(Filename);
		SetDirectory(Directory);
	end;
end;

procedure TFileScreen.DeleteDir(DoDialog: Boolean = True);
var
	Dir: String;
begin
	{$IFNDEF WINDOWS}
	ModalDialog.ShowMessage('Delete Directory', 'Not implemented on non-Windows platforms!');
	Exit;
	{$ENDIF}

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
			Format('Delete directory "%s"?', [DirList.Items[DirList.ItemIndex].Captions[0]]),
			[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0);
	end
	else
	begin
		DeleteToBin(Dir); //FileUtil.DeleteDirectory
		SetDirectory(Directory);
	end;
end;

procedure TFileScreen.CopyFile(const DestDir: String; MoveFile: Boolean = False);
var
	DestFile, Filename: String;
begin
	if (DestDir = '') or (DirectoryExists(DestDir) = False) then
	begin
		Log(TEXT_ERROR + 'Cannot copy file: Directory "%s" not found!', [DestDir]);
		Exit;
	end;

	Filename := GetSelectedFile(False);
	DestFile := IncludeTrailingPathDelimiter(DestDir)   + Filename;
	Filename := IncludeTrailingPathDelimiter(Directory) + Filename;

	if FileExists(Filename) then
	begin
		if FileExists(DestFile) then
			ModalDialog.ShowMessage('Copy Error', 'Destination file already exists!')
		else
		begin
			if FileUtil.CopyFile(Filename, DestFile, True) then
			begin
				if MoveFile then
				begin
					DeleteToBin(Filename);
					ModalDialog.ShowMessage('File Move', 'File moved to ' + DestDir + '.');
					SetDirectory(Directory);
				end
				else
					ModalDialog.ShowMessage('File Copy', 'File copied to ' + DestDir + '.');
			end
			else
				ModalDialog.ShowMessage('Copy Error', 'Error copying file!');
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

	li := TCWEListItem.Create(STR_DIRECTORYUP, LISTITEM_PARENT);
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
		if li.Data = LISTITEM_PARENT then
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
begin
	if lblSearch = nil then Exit;

	lblSearch.Paint;
	if lblSearch.Caption <> '' then
		SelectFile(lblSearch.Caption);
end;

procedure TFileScreen.SelectFile(Filename: String);
var
	Y: Integer;
begin
	Filename := LowerCase(Filename);

	for Y := 0 to FileList.Items.Count-1 do
	begin
		if Pos(Filename, LowerCase(FileList.Items[Y].Captions[0])) = 1 then
		begin
			// found match, select it!
			FileList.Select(Y);
			FileOrDirSelected(FileList);
			Exit;
		end;
	end;
end;

procedure TFileScreen.RenameCallback(ID: Word;
	ModalResult: TDialogButton; Tag: Integer; Data: Variant;
	Dlg: TCWEDialog);
var
	Ct: TCWEControl;
	Dir, NewName: String;
	X: Integer;
const
	InvalidChars = ':*?\/|';
begin
	if Dlg = nil then Exit;
	if ModalResult <> btnOK then Exit;

	Ct := Dlg.Dialog.FindControl('Edit');
	if Ct = nil then Exit;

	NewName := TCWEEdit(Ct).Caption;
	if NewName = '' then Exit;

	if ID <> ACTION_RENAMEBOOKMARK then
		for X := 1 to Length(InvalidChars) do
			if Pos(InvalidChars[X], NewName) > 0 then
			begin
				Log(TEXT_ERROR + 'Rename aborted - name contains illegal characters! ');
				Exit;
			end;

	case ID of

		ACTION_RENAMEFILE:
		begin
			Dir := IncludeTrailingPathDelimiter(Directory);
			if not RenameFile(Dir + GetSelectedFile(False), Dir + NewName) then
			begin
				Log(TEXT_ERROR + 'File rename failed! ' + Dir + NewName);
				Exit;
			end;
			SetDirectory(Directory);
			SelectFile(NewName);
		end;

		ACTION_RENAMEDIR:
		begin
			NewName := IncludeTrailingPathDelimiter(
				IncludeTrailingPathDelimiter(Directory) + NewName);
			Dir := IncludeTrailingPathDelimiter(DirList.GetPath(True));
			if not RenameFile(Dir, NewName) then
			begin
				Log(TEXT_ERROR + 'Directory rename failed! ');
				Log(Directory + ' -> ' + NewName);
				Exit;
			end
			else
				SetDirectory(Directory);
		end;

		ACTION_RENAMEBOOKMARK:
			RenameBookmark(DirList.GetPath(True), NewName);

		ACTION_CREATEDIR:
		begin
			NewName := IncludeTrailingPathDelimiter(
				IncludeTrailingPathDelimiter(Directory) + NewName);
			if not CreateDir(NewName) then
			begin
				Log(TEXT_ERROR + 'Directory creation failed! ');
				Exit;
			end
			else
				SetDirectory(Directory);
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
	Dir := '';

	case Cmd of

		FILESORT_NAME, FILESORT_SIZE, FILESORT_DATE:
		begin
			SortMode := Cmd;
			if InSampleReq then
			begin
				Options.Dirs.SampleSortMode := Cmd;
				if SampleRequester.InModule then Exit;
			end
			else
				Options.Dirs.FileSortMode := Cmd;
			SetDirectory(Directory);
		end;

		FILE_EXPLORE:
			if FileScreen.InModule then
				SelectFileInExplorer(SampleRequester.ModFilename)
			else
				SelectFileInExplorer(GetSelectedFile);

		FILE_COPY, FILE_MOVE:
		begin
			case DirList.Items[DirList.ItemIndex].Data of
				LISTITEM_BOOKMARK:
					Dir := Bookmarks[DirList.ItemIndex].Path;
				LISTITEM_DIR, LISTITEM_PARENT, LISTITEM_DRIVE:
					Dir := DirList.GetPath(True);
			end;
			if Dir <> '' then
				CopyFile(Dir, (Cmd = FILE_MOVE));
		end;

		FILE_DELETE:
			if IsFolder then
			begin
				case DirList.Items[DirList.ItemIndex].Data of
					LISTITEM_BOOKMARK:
						RemoveBookmark(Bookmarks[DirList.ItemIndex].Path);
					LISTITEM_DRIVE:
						ModalDialog.ShowMessage('Delete', 'Can''t delete a drive!');
					LISTITEM_DIR:
						DeleteDir(True);
				end;
			end
			else
			if IsFile then
				DeleteFile(True);

		FILE_RENAME:
			if IsFolder then
			begin
				case DirList.Items[DirList.ItemIndex].Data of
					LISTITEM_BOOKMARK:
						AskString(ACTION_RENAMEBOOKMARK, 'Rename Bookmark',
							DirList.GetPath(False), False, RenameCallback);
					LISTITEM_DRIVE:
						ModalDialog.ShowMessage('Rename',
							'Can''t rename a drive!');
					LISTITEM_DIR:
						AskString(ACTION_RENAMEDIR, 'Rename Directory',
							DirList.GetPath(False), False, RenameCallback);
				end;
			end
			else
			if IsFile then
				AskString(ACTION_RENAMEFILE, 'Rename File',
					GetSelectedFile(False), False, RenameCallback);

		FILE_CREATEDIR:
			AskString(ACTION_CREATEDIR, 'Create Directory',
				'', False, RenameCallback);

		FILE_BOOKMARK:
			if not DirList.Focused then
				AddBookmark(Directory)
			else
			case DirList.Items[DirList.ItemIndex].Data of
				LISTITEM_BOOKMARK:
					AddBookmark(Directory, DirList.ItemIndex);
				LISTITEM_DRIVE, LISTITEM_DIR:
					AddBookmark(DirList.GetPath(True));
			end;

		FILE_MERGEMODULE:
			Module.MergeWithFile(GetSelectedFile);

	end;
end;

// ==========================================================================
{ TBookmarkList }
// ==========================================================================

function TBookmarkList.Get(Index: Integer): TBookmark;
begin
	Result := TBookmark(inherited Get(Index));
end;

procedure TBookmarkList.Put(Index: Integer; AValue: TBookmark);
begin
	inherited Put(Index, Pointer(AValue));
end;

function TBookmarkList.IndexOf(APath: String): Integer;
var
	i: Integer;
begin
	Result := -1;
	APath := LowerCase(APath);
	for i := 0 to Count-1 do
		if LowerCase(Items[i].Path) = APath then
			Exit(i);
end;

function TBookmarkList.Add(ABookmark: TBookmark): Integer;
begin
	Result := IndexOf(ABookmark.Path);
	if Result < 0 then
		Result := inherited Add(ABookmark);
end;

procedure TBookmarkList.LoadFromFile(const Filename: String);
var
	Bm: TBookmark;
	sl: TStringList;
	S: String;
	x: Integer;
begin
	Clear;

	sl := TStringList.Create;
	sl.LoadFromFile(Filename);

	for S in sl do
	begin
		if S = '' then Continue;

		x := Pos('=', S);
		Bm := TBookmark.Create;

		if x > 1 then
		begin
			Bm.Name := Copy(S, 1, x-1);
			Bm.Path := Copy(S, x+1, MaxInt);
		end
		else
		begin
			Bm.Path := S;
			if Bm.Path[1] = '=' then
				Bm.Path := Copy(S, 2, MaxInt);
			Bm.Name := ExtractFilename(ExcludeTrailingPathDelimiter(S));
		end;
		Add(Bm);
	end;

	sl.Free;
end;

procedure TBookmarkList.SaveToFile(const Filename: String);
var
	i: Integer;
	sl: TStringList;
begin
	sl := TStringList.Create;
	for i := 0 to Count-1 do
		sl.Add(Items[i].Name + '=' + Items[i].Path);
	sl.SaveToFile(Filename);
	sl.Free;
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
var
	Bm: TBookmark;
begin
	if Bookmarks.IndexOf(Path) >= 0 then Exit;

	{AskString(ACTION_ASKED_STRING, 'Input value:',
		Key, True, TValQuery.DialogCallback);}

	Bm := TBookmark.Create;
	Bm.Path := Path;
	Bm.Name := ExtractFilename(ExcludeTrailingPathDelimiter(Path));
	if Bm.Name = '' then Bm.Name := Path;

	if Index < 0 then
		Bookmarks.Add(Bm)
	else
		Bookmarks.Insert(Index, Bm);
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

procedure TFileScreen.RenameBookmark(const Path, NewName: String);
var
	i: Integer;
begin
	i := Bookmarks.IndexOf(Path);
	if i >= 0 then
	begin
		Bookmarks[i].Name := NewName;
		BookmarksChanged;
	end
	else
		Log(TEXT_ERROR + 'Could not rename bookmark "%s"!', [Path]);
end;

procedure TFileScreen.BookmarksChanged(Recreate: Boolean = True);
var
	i: Integer;
	li: TCWEListItem;
	S: String;
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
		S := Bookmarks[i].Name;
		if S = '' then
			S := ExtractFilename(Bookmarks[i].Path);

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

	FileList.OnChange		:= FileOrDirSelected;
	DirList.OnChange		:= FileOrDirSelected;
	DirEdit.OnChange		:= DirOrFilenameEntered;
	FilenameEdit.OnChange	:= DirOrFilenameEntered;

	SortMode := Options.Dirs.FileSortMode;
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
	Filename, StrOW: String;
begin
	Filename := GetSelectedFile(True, True);
	if (DoDialog) and (FileExists(Filename)) then
	begin
		StrOW := Format('Overwrite file "%s"?', [ExtractFilename(Filename)]);
		if not InSampleReq then
			ModalDialog.MessageDialog(ACTION_SAVEFILE, 'Save Module', StrOW,
				[btnYES, btnCancel], btnCancel, Window.DialogCallback, 0)
		else
			ModalDialog.MessageDialog(ACTION_SAVEFILE, 'Save Sample', StrOW,
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

procedure TModFileScreen.DirOrFilenameEntered(Sender: TCWEControl);
begin
	if Sender = DirEdit then
		SetDirectory(DirEdit.Caption)
	else
	begin
		if FileList.Focused then
			FilenameEdit.SetCaption(FileList.GetCaption);
		if SaveMode then
			SaveFile
		else
			LoadFile(GetSelectedFile);
	end;
end;

procedure TModFileScreen.FileOrDirSelected(Sender: TCWEControl);
var
	S: String;
begin
	if Sender = FileList then
	begin
		S := FileList.GetCaption;
		if not SaveMode then
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

procedure TModFileScreen.Show(aSaveMode: Boolean; const Dir: String);
begin
	inherited Show(aSaveMode, Dir);

	if aSaveMode then
	begin
		if Module.Info.Filename <> '' then
		begin
			FilenameEdit.SetCaption(ExtractFilename(Module.Info.Filename));
			FileList.Select(FilenameEdit.Caption);
		end
		else
		if Trim(Module.Info.Title) <> '' then
			FilenameEdit.SetCaption(Trim(Module.Info.Title))
		else
			FilenameEdit.SetCaption('');
		ActivateControl(FilenameEdit);
	end
	else
		ActivateControl(FileList);
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

				x := Filename.LastIndexOf('.') + 1;
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
			// play selected sample instead of searching for filename
			if (InSampleReq) and (not IsShiftPressed) then Exit(False);
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
			FileScreen.HandleCommand(FILE_BOOKMARK);

		ctrlkeyRETURN:
			if not InSampleReq then			// Load/save module
				FileRequester.DirOrFilenameEntered(Self)
			else
			if IsValidItemIndex(ItemIndex) then
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

							SampleMod := TPTModule.Create(True, True);
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

function TDirList.GetPath(Fullpath: Boolean): String;
begin
	case Items[ItemIndex].Data of

		LISTITEM_BOOKMARK:
		with TFileScreen(Screen) do
		begin
			if Fullpath then
				Result := Bookmarks[ItemIndex].Path
			else
				Result := Bookmarks[ItemIndex].Name;
		end;

		LISTITEM_DRIVE:
		begin
			Result := Items[ItemIndex].Captions[0];
		end;

		LISTITEM_PARENT:
		begin
			// returns STR_DIRECTORYUP for "<Parent>" item if Fullpath = False!
			Result := Items[ItemIndex].Captions[0];
			if (Fullpath) and (Result = STR_DIRECTORYUP) then
				Result := GetParentDir(TFileScreen(Screen).Directory);
		end;

		LISTITEM_DIR:
		begin
			Result := Items[ItemIndex].Captions[0];
			if Fullpath then
				Result := IncludeTrailingPathDelimiter(
					TFileScreen(Screen).Directory) + Result;
		end;

	end;
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

		ctrlkeyINSERT:
			if Shift = [ssShift] then
				FileScreen.HandleCommand(FILE_COPY)
			else
				FileScreen.HandleCommand(FILE_BOOKMARK);

		ctrlkeyPLUS,
		ctrlkeyMINUS:
			if Items[ItemIndex].Data = LISTITEM_BOOKMARK then
				Scr.MoveBookmark(Scr.Bookmarks[ItemIndex].Path, (Sc = ctrlkeyPLUS));

		ctrlkeyRETURN:	// enter directory
		begin
			case Items[ItemIndex].Data of
				LISTITEM_BOOKMARK:
				begin
					Dir := Scr.Bookmarks[ItemIndex].Path;
					Scr.SetDirectory(Dir);
					Exit;
				end;
				LISTITEM_DIR, LISTITEM_PARENT:
					Dir := GetPath(False);
				LISTITEM_DRIVE:
				begin
					Dir := GetPath(False);
					Scr.SetDirectory(Dir);
					Exit;
				end;
			else
				Exit;
			end;

			with Scr do
			begin
				if Dir = STR_DIRECTORYUP then
					SetDirectory(GetParentDir(Directory))
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

