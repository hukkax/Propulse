unit Screen.FileReqSample;

interface

uses
	Classes, Types, SysUtils,
	TextMode, CWE.Core, CWE.Widgets.Text,
	ProTracker.Player, SampleView,
	Screen.Samples,	Screen.FileReq;

const
	MAX_SAMPLEFILE_SIZE = 2048*1024;

	LI_LOADABLE  = 0;
	LI_ENTERABLE = 1;
	LI_UNENTER   = LISTITEM_HEADER;

	COLOR_ENTERABLE       = 3;  // module
	COLOR_UNENTER         = 10; // dir up

type
	TSampleFileScreen = class(TFileScreen)
	private
	public
		SampleMod:	TPTModule;
		ModFilename:String;

		Waveform:	TSampleView;
		lblFormat:	TCWELabel;

		procedure 	DirOrFilenameEntered(Sender: TCWEControl); override;
		procedure 	FileOrDirSelected(Sender: TCWEControl); override;

		procedure 	SetDirectory(Dir: String); override;
		procedure 	Paint; override;

		procedure 	LoadFile(const Filename: String); override;
		procedure 	SaveFile(DoDialog: Boolean = True); override;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
		destructor	Destroy; override;
	end;

var
	SampleRequester: TSampleFileScreen;


implementation


uses
	Generics.Defaults, FileUtils, //IOUtils,
	Layout, CWE.Dialogs,
	ShortcutManager,
	ProTracker.Util,
	ProTracker.Sample,
	ProTracker.Editor,
	Screen.Editor;


constructor TSampleFileScreen.Create;
var
	X1: Integer;
begin
	RegisterScreenLayout(Self, 'SampleFileRequester');

	inherited;

	SortMode := Options.Dirs.SampleSortMode;
	LoadBookmarks(ConfigPath + 'bookmarks-samples.ini');

	X1 := DirList.Rect.Left;

	Waveform := TSampleView.Create(Self, '', 'Sample Waveform',
		Types.Rect(X1, Console.Height-8, Console.Width-1, Console.Height-4), True);
	Waveform.Init(False, True);
	RegisterLayoutControl(TCWEControl(Waveform),  CTRLKIND_BOX, False, True, True);

	lblFormat := TCWELabel.Create(Self, '', 'Sample Format',
		Types.Rect(X1, DirList.Rect.Bottom+1, Console.Width-1, DirList.Rect.Bottom+2));
	RegisterLayoutControl(TCWEControl(lblFormat), CTRLKIND_LABEL, False, True, False);

	lblSearchHint.SetCaption('Use Shift to search:');

	FileList.OnChange     := FileOrDirSelected;
	DirList.OnChange      := FileOrDirSelected;
	DirEdit.OnChange      := DirOrFilenameEntered;
	FilenameEdit.OnChange := DirOrFilenameEntered;

	InModule   := False;
	SaveMode   := False;
	SampleMod  := nil;
	ModFilename := '';

	ActiveControl := FileList;

	LoadLayout(Self);
end;

destructor TSampleFileScreen.Destroy;
begin
	if SampleMod <> nil then
		FreeAndNil(SampleMod);
	inherited Destroy;
end;

procedure TSampleFileScreen.Paint;
begin
	FileList.ColumnColor[1] := COLOR_FILE_SIZE;
	FileList.ColumnColor[2] := COLOR_FILE_DATE;

	inherited;

	Waveform.DrawBorder;
	Waveform.Paint;

	if SaveMode then
		lblHeader.SetCaption('Save Sample')
	else
		lblHeader.SetCaption('Load Sample');

	FileList.Paint;
end;

procedure TSampleFileScreen.DirOrFilenameEntered;
var
	Filename: String;
begin
	if Sender = DirEdit then
		SetDirectory(DirEdit.Caption)
	else
	begin
		if FileList.Focused then
			FilenameEdit.SetCaption(FileList.GetCaption);
		Filename := Directory + ValidateFilename(FilenameEdit.Caption);
		if not SaveMode then
			LoadFile(Filename)
		else
			SaveFile(False);
	end;
end;

procedure TSampleFileScreen.FileOrDirSelected(Sender: TCWEControl);
var
	Item: TCWEListItem;
	S: AnsiString;
	Sam: TImportedSample;
begin
	if Sender <> FileList then Exit;

	S := FileList.GetCaption;
	if not SaveMode then
		FilenameEdit.SetCaption(S);

	Module.Stop;
	if FileList.ItemIndex >= FileList.Items.Count then Exit;

	Item := FileList.Items[FileList.ItemIndex];
	if Item = nil then Exit;

	if (Item.Data = LI_ENTERABLE) or (Item.Data = LI_UNENTER) then
		Waveform.Sample := nil
	else
	begin
		if InModule then
			Module.Samples.Last.Assign(TSample(Item.ObjData))
		else
			Module.Samples.Last.LoadFromFile(Directory + FileList.GetCaption);
	end;

	Waveform.Sample := Module.Samples.Last;
	Waveform.DrawWaveform;

	if Item.Data = LI_UNENTER then
		S := 'Back to dir. list'
	else
	if Item.ObjData <> nil then
	begin
		Sam := TImportedSample(Item.ObjData);
		if Sam.Length < 2 then
			S := '(empty)'
		else
		begin
			if Sam.Is16Bit then
				S := '16-bit'
			else
				S := '8-bit';
			if Sam.IsStereo then
				S := S + ' stereo'
			else
				S := S + ' mono';
		end;
	end
	else
	if (Waveform.Sample <> nil) and (Waveform.Sample.Length > 1) then
		S := '8-bit mono'
	else
		S := '';

	lblFormat.SetCaption(S);
end;

procedure TSampleFileScreen.LoadFile(const Filename: String);
begin
	SampleScreen.LoadSample(Filename);
end;

procedure TSampleFileScreen.SaveFile(DoDialog: Boolean);
var
	Filename: String;
	Sam: TSample;
begin
	Sam := GetCurrentSample;
	if not IsEmptySample(Sam) then
	begin
		Filename := Directory + ValidateFilename(FilenameEdit.Caption);
		Sam.SaveToFile(Filename, SamFmtFromExt);
		SetDirectory(ExtractFilePath(Filename));
		ChangeScreen(TCWEScreen(SampleScreen));
	end;
end;

procedure TSampleFileScreen.SetDirectory(Dir: String);

	function GetSize(i: Cardinal): String;
	begin
		if i = 0 then
			Result := ''
		else
		if Options.Dirs.RawFileSizes then
			Result := Format('%8d', [i])
		else
			Result := Format('%6d K', [RoundUp(i / 1024)]);
	end;

var
	sr: TSearchRec;
	IsMod: Boolean;
	PrevDir,
	sModFile, Filename: String;
	i: Integer;
	cf: Byte;
	S: TSample;
label
	Done;
const
	AmigaExts  = '[mod.][p61.]';
	Extensions_Module = '[.mod][.it][.p61]';
begin
	FileList.Clear;

	if (InModule) and (not SaveMode) and (Dir = '') then
	begin
		sModFile := ExtractFilename(ModFilename);
		FileList.Items.Add(TCWEListItem.Create(
			'Back to directory', LI_UNENTER, nil, COLOR_UNENTER));
		if (SampleMod = nil) or (SampleMod.ImportInfo.Samples = nil) then Exit;

		for S in SampleMod.ImportInfo.Samples do
		begin
			if S.Length > 0 then
				cf := COLOR_FILE_SAMPLE //COLOR_LOADABLE_INMOD
			else
				cf := COLOR_FILE_EMPTYSAMPLE;

			FileList.Items.Add(TCWEListItem.Create(
				S.GetName + COLUMNSEPARATOR +
				GetSize(S.Length * 2) + COLUMNSEPARATOR + sModFile,
				LI_LOADABLE, Pointer(S), cf));
		end;
		goto Done;
	end
	else
	begin
		InModule := False;
		ModFilename := '';
	end;

	if (Dir = '') or (not DirectoryExists(Dir)) then Exit;

	PrevDir := ExtractFileName(ExcludeTrailingPathDelimiter(Directory));
	Directory := IncludeTrailingPathDelimiter(Dir);
	DirEdit.SetCaption(Directory);
	Options.Dirs.Samples := Directory;

	// list files in current dir
	//
	if SysUtils.FindFirst(Directory + '*', faAnyFile, sr) = 0 then
	begin
		repeat
			if (sr.Attr and faDirectory) = 0 then
			begin
				{$IFDEF UNIX}
				if (Pos('.', sr.Name) = 1) then Continue;
				{$ENDIF}

				Filename := StringReplace(LowerCase(sr.Name), '~', '', []); // ???
				IsMod :=
					(Pos('['+Copy(Filename, 1, 4)+']', AmigaExts) >= 1) or
					(Pos('['+ExtractFileExt(Filename)+']', Extensions_Module) >= 1);

				if (not IsMod) then
				begin
					if (sr.Size <= 0) or (sr.Size > MAX_SAMPLEFILE_SIZE) then
						Continue;
				end;

				cf := COLOR_FILE_SAMPLE;	// not in Else block because compiler
				i := LI_LOADABLE;			// is being dumb and giving a warning

				if IsMod then
				begin
					if SaveMode then Continue;
					cf := COLOR_ENTERABLE;
					i := LI_ENTERABLE; // module flag
				end;

				FileList.Items.Add(TCWEListItem.Create(
					sr.Name + COLUMNSEPARATOR +
					GetSize(sr.Size) + COLUMNSEPARATOR +
					FormatDateTime('yyyy-mm-dd hh:nn', FileDateToDateTime(sr.Time)),
					i, nil, cf));
			end;
		until FindNext(sr) <> 0;
		FindClose(sr);

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

end.


