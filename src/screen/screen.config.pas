unit Screen.Config;

interface

uses
	Classes, Types, SysUtils,
	TextMode, ConfigurationManager,
	CWE.Core, CWE.Widgets.Text;

const
	HEADER_FG = 6;
	HEADER_BG = 15;

type
	TCWEConfigList = class(TCWETwoColumnList)
	public
		ConfigManager: TConfigurationManager;

		procedure 	ItemFromConfig(var LI: TCWEListItem);

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		procedure 	Init(var aConfigManager: TConfigurationManager);

		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
	end;

	TConfigScreen = class(TCWEScreen)
	private
		WaitingForKeyBinding: Boolean;
	public
		ColorList,
		List:			TCWEConfigList;
		KeyList:		TCWETwoColumnList;
		bLoadPalette,
		bSavePalette:	TCWEButton;

		PaletteConfig:	TConfigurationManager;
		PaletteRGB:		array[0..15] of array[0..2] of Byte;

		procedure 	LoadPalette(const Presetname: String = '');
		procedure 	SavePalette(const Presetname: String = '');

		procedure 	ListKeyBindings;
		procedure 	GetNewKeyBinding(Sender: TCWEControl;
					var Key: Integer; Shift: TShiftState; var Handled: Boolean);

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;

		procedure	Paint; override;
		procedure 	ListPaintItem(Sender: TCWEControl;
					var Item: TCWEListItem;	Index: Word; P: TPoint);

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
		destructor	Destroy; override;

		procedure	Init(var aConfigManager: TConfigurationManager);
		procedure	Show; override;
	end;

var
	ConfigScreen: TConfigScreen;


implementation

uses
	SDL.Api.Types, Graphics32,
	Layout,
	ShortcutManager,
	Screen.Editor,
	Screen.FileReq,
	Screen.Help,
	ProTracker.Editor,
	ProTracker.Util,
	CWE.Dialogs,
	CWE.Widgets.Scrollers,
	SampleView;

// ============================================================================
// Callbacks
// ============================================================================

procedure ColorChanged;
var
	i, P: Integer;
	LI: TCWEListItem;
begin
	with ConfigScreen do
	begin
		i := ColorList.ItemIndex;
		LI := ColorList.Items[i];

		P := i mod 3;
		i := i div 3;
		if i < 16 then
		begin
			PaletteRGB[i, P] := TConfigItemByte(LI.ObjData).GetValue;
			Console.Palette[i] := Color32(
				PaletteRGB[i, 0] * 4, PaletteRGB[i, 1] * 4, PaletteRGB[i, 2] * 4);
		end;

		Console.FillRect(Rect, ' ', TConsole.COLOR_TEXT, TConsole.COLOR_PANEL);
		Console.Refresh;
		Paint;
	end;
end;

// ============================================================================
{ TConfigScreen }
// ============================================================================

constructor TConfigScreen.Create;
const
	X2 = 58;
begin
	inherited;

	RegisterScreenLayout(Self, 'Configuration');

	AddHeader('');

	List := TCWEConfigList.Create(Self, '', 'Options',
		Types.Rect(1, 5, X2, 21), True);
	List.ColumnWidth[1] := 18;
	List.ColumnColor[1] := 3;
	List.Border.Pixel := True;
	RegisterLayoutControl(List, CTRLKIND_BOX, False, True, True);

	KeyList := TCWETwoColumnList.Create(Self, '', 'Keybindings',
		Types.Rect(1, 24, X2, 44), True);
	KeyList.ColumnWidth[1] := 18;
	KeyList.OnKeyDown := GetNewKeyBinding;
	KeyList.ColumnColor[0] := 2;
	KeyList.Border.Pixel := True;
	RegisterLayoutControl(KeyList, CTRLKIND_BOX, False, True, True);

	ColorList := TCWEConfigList.Create(Self, '', 'Palette',
		Types.Rect(61, 5, 78, 40+4), True);
	ColorList.ColumnWidth[1] := 2;
	ColorList.OnPaintItem := ListPaintItem;
	ColorList.Selection3D := True;
	ColorList.Border.Pixel := True;
	RegisterLayoutControl(ColorList, CTRLKIND_BOX, False, True, True);

	TCWELabel.Create(Self, 'Program Settings (Arrows to change values)', '',
		Types.Rect(1, 3, 58, 4)).ColorFore := 1;
	TCWELabel.Create(Self, 'Color Palette', '',
		Types.Rect(61, 3, 74, 4)).ColorFore := 1;
	TCWELabel.Create(Self, 'Key Bindings (Enter to modify)', '',
		Types.Rect(1, 22, 58, 23)).ColorFore := 1;

{	bLoadPalette := TCWEButton.Create(Self, 'Load preset', 'LoadPal',
		Bounds(ColorList.Rect.Left, ColorList.Rect.Bottom+1, 18, 1), True);}
//	bLoadPalette.OnChange := LoadPalette; !!!

{	bSavePalette := TCWEButton.Create(Self, 'Save preset', 'SavePal',
		Bounds(ColorList.Rect.Left, ColorList.Rect.Bottom+3, 18, 1), True);}
//	bSavePalette.OnChange := SavePalette; !!!

	ActiveControl := List;

	LoadLayout(Self);

	PaletteConfig := TConfigurationManager.Create;
end;

destructor TConfigScreen.Destroy;
begin
	PaletteConfig.Free;
	inherited;
end;

procedure TConfigScreen.Init(var aConfigManager: TConfigurationManager);
const
	Cn = 'RGB';
var
	S: AnsiString;
	P, C: Integer;
begin
	List.Init(aConfigManager);

	// ==============================================================

	for P := 0 to High(PaletteRGB) do
	begin
		PaletteRGB[P, 0] := RedComponent  (Console.Palette[P]) div 4;
		PaletteRGB[P, 1] := GreenComponent(Console.Palette[P]) div 4;
		PaletteRGB[P, 2] := BlueComponent (Console.Palette[P]) div 4;
		S := Format('Color %.2d ', [P]);
		for C := 0 to 2 do
			PaletteConfig.AddByte('Palette', S + Cn[C+1],
				@PaletteRGB[P, C], PaletteRGB[P, C]).
				SetInfo('§', 0, 63, [], ColorChanged, '%.2d');
	end;

	// ==============================================================
(*
	S := 'List';

	PaletteConfig.AddByte(S, 'Foreground',
		@TCWEList.COL_LIST_FORE, 3).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Background',
		@TCWEList.COL_LIST_BACK, 0).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');
*)

	// ==============================================================

	S := 'General';

	PaletteConfig.AddByte(S, 'Blank',
		@Console.COLOR_BLANK, Console.COLOR_BLANK).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

{	PaletteConfig.AddByte(S, 'Text',
		@Console.COLOR_TEXT, Console.COLOR_TEXT).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');}

	PaletteConfig.AddByte(S, 'Panel',
		@Console.COLOR_PANEL, Console.COLOR_PANEL).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, '3D Dark',
		@Console.COLOR_3DDARK, Console.COLOR_3DDARK).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, '3D Light',
		@Console.COLOR_3DLIGHT, Console.COLOR_3DLIGHT).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Light',
		@Console.COLOR_LIGHT, Console.COLOR_LIGHT).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	// ==============================================================

	S := 'PatternEditor';

	PaletteConfig.AddByte(S, 'FG',
		@PatternEditor.ColorFore, 6).
		SetInfo('Foreground', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'BG',
		@PatternEditor.ColorBack, 0).
		SetInfo('Background', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'CUR_FG',
		@TPatternEditor.COL_CUR_FG, 0).
		SetInfo('Cursor fore', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'CUR_BG',
		@TPatternEditor.COL_CUR_BG, 3).
		SetInfo('Cursor back', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'HL_ROW',
		@TPatternEditor.COL_HL_ROW, 14).
		SetInfo('Highlight row', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'HL_BEAT',
		@TPatternEditor.COL_HL_BEAT, 15).
		SetInfo('Highlight beat', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'SEL_BG',
		@TPatternEditor.COL_SEL_BG, 8).
		SetInfo('Selected back', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'SEL_FG',
		@TPatternEditor.COL_SEL_FG, 3).
		SetInfo('Selected fore', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'HL_SEL_ROW',
		@TPatternEditor.COL_HL_SEL_ROW, 9).
		SetInfo('Sel. HL row', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'HL_SEL_BEAT',
		@TPatternEditor.COL_HL_SEL_BEAT, 9).
		SetInfo('Sel. HL beat', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'ROW',
		@TPatternEditor.COL_ROW, 0).
		SetInfo('Row number', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'ROW_CURRENT',
		@TPatternEditor.COL_ROW_CURRENT, 3).
		SetInfo('Row, playing', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'ROW_CURRENTOTHER',
		@TPatternEditor.COL_ROW_CURRENTOTHER, 6).
		SetInfo('Current row', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX
	PaletteConfig.AddByte(S, 'INVALID',
		@TPatternEditor.COL_INVALID, 13).
		SetInfo('Invalid notes', 0, 15, [], ColorChanged, '%.2d');
		//       XXXXXXXXXXXXXX

	// ==============================================================

	S := 'Scrollbar';

	PaletteConfig.AddByte(S, 'Gutter',
		@TCWEScrollbar.COLOR_SB_GUTTER, 1).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Thumb',
		@TCWEScrollbar.COLOR_SB_THUMB, 3).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Button',
		@TCWEScrollbar.COLOR_SB_BUTTON, 2).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Icon',
		@TCWEScrollbar.COLOR_SB_ICON, 11).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Hover',
		@TCWEScrollbar.COLOR_SB_HOVER, 11).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Active',
		@TCWEScrollbar.COLOR_SB_ACTIVE, 11).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	// ==============================================================

	S := 'File Requester';

	// File list

	PaletteConfig.AddByte(S, 'File Size',
		@TFileScreen.COLOR_FILE_SIZE, 2).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'File Date',
		@TFileScreen.COLOR_FILE_DATE, 10).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'PT Module',
		@TFileScreen.COLOR_FILE_PT, 2).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Other Module',
		@TFileScreen.COLOR_FILE_NONPT, 3).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Sample File',
		@TFileScreen.COLOR_FILE_SAMPLE, 6).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Empty Sample',
		@TFileScreen.COLOR_FILE_EMPTYSAMPLE, 15).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	// Directory list

	PaletteConfig.AddByte(S, 'Bookmark',
		@TFileScreen.COLOR_FG_BOOKMARK, 10).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Drive',
		@TFileScreen.COLOR_FG_DRIVE, 2).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'ParentDir',
		@TFileScreen.COLOR_FG_PARENTDIR, 6).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Directory',
		@TFileScreen.COLOR_FG_DIR, 3).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	// ==============================================================

	S := 'Waveform';

	PaletteConfig.AddByte(S, 'Background',
		@TSampleView.COLOR_BACKGROUND, 0).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Waveform',
		@TSampleView.COLOR_WAVEFORM, 6).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Waveform peaks',
		@TSampleView.COLOR_WAVEFORM_PEAKS, 15).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Overrun',
		@TSampleView.COLOR_OVERRUN, 4).
		SetInfo('Waveform >64K', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Center line',
		@TSampleView.COLOR_CENTERLINE, 15).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Loop point',
		@TSampleView.COLOR_LOOP, 5).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Loop hover',
		@TSampleView.COLOR_LOOP_HOVER, 11).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Playback pos.',
		@TSampleView.COLOR_PLAYBACK, 11).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Selection fore',
		@TSampleView.COLOR_SEL_FORE, 11).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	PaletteConfig.AddByte(S, 'Selection back',
		@TSampleView.COLOR_SEL_BACK, 13).
		SetInfo('§', 0, 15, [], ColorChanged, '%.2d');

	// ==============================================================

	LoadPalette;

	ListKeyBindings;
	List.AdjustScrollbar;

	UpdateHeader('Screen.Config');
end;

procedure TConfigScreen.LoadPalette(const Presetname: String = '');

	function GetPaletteIniName(const Presetname: String = ''): String;
	begin
		if Presetname = '' then
		begin
			Result := ConfigPath + FILENAME_PALETTE;
			if not FileExists(Result) then
				Result := DataPath + FILENAME_PALETTE;
		end
		else
			Result := Presetname; //DataPath + 'palette\' + Presetname+'.ini';
	end;

var
	CI: TConfigItem;
	LI: TCWEListItem;
	i, N, P: Integer;
	S: AnsiString;
begin
	PaletteConfig.Filename := GetPaletteIniName(Presetname);
	PaletteConfig.Load;

	ColorList.Items.Clear;
	S := '';

	for CI in PaletteConfig.Items do
	begin
		if CI.Caption = '' then Continue;

		if (CI.Section <> S) and (S <> '') then
		begin
			LI := TCWEListItem.Create(CI.Section, LISTITEM_HEADER, nil, HEADER_FG, HEADER_BG);
			ColorList.Items.Add(LI);
		end;

		LI := TCWEListItem.Create('', 0, CI);
		List.ItemFromConfig(LI);
		ColorList.Items.Add(LI);
		S := CI.Section;
	end;

	for i := 0 to 15*3 do
	begin
		LI := ColorList.Items[i];
		P := i mod 3;
		N := i div 3;
		PaletteRGB[N, P] := TConfigItemByte(LI.ObjData).GetValue;
		Console.Palette[N] := Color32(
			PaletteRGB[N,0] * 4, PaletteRGB[N,1] * 4, PaletteRGB[N,2] * 4);
	end;

	ColorList.AdjustScrollbar;
	ColorChanged;
end;

procedure TConfigScreen.SavePalette(const Presetname: String = '');
begin
	if PaletteConfig = nil then Exit;

	if Presetname = '' then
		PaletteConfig.Filename := ConfigPath + FILENAME_PALETTE
	else
		PaletteConfig.Filename := Presetname;

	PaletteConfig.Save;
end;

procedure TConfigScreen.Show;
begin
	inherited;
	// make sure config changes are reflected in settings list
	List.Init(List.ConfigManager);
	List.Paint;
end;

function TConfigScreen.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
var
	LI: TCWEListItem;
	CI: TConfigItem;
begin
	// we want to always receive Return and Delete in KeyList
	// even if they're global bindings for something else
	if (KeyList.Focused) and (Key in [SDLK_RETURN, SDLK_DELETE]) then
		Result := True
	else
		Result := False;

	if (Key = SDLK_F1) and (ActiveControl = List) then
	begin
		LI := List.Items[List.ItemIndex];
		if LI.ObjData <> nil then
		begin
			CI := TConfigItem(LI.ObjData);
			//ModalDialog.ShowMessage(CI.Name, CI.Section + '.' + CI.Name);
			ModalDialog.MultiLineMessage(CI.Caption, Help.Memo.GetSection(CI.Section + '.' + CI.Name));
			Exit(True);
		end;
	end;

	if WaitingForKeyBinding then
	begin
		// ignore Shift/Ctrl/Alt without other keys
		case Key of
			0,
			SDLK_LSHIFT, SDLK_RSHIFT,
			SDLK_LCTRL,  SDLK_RCTRL,
			SDLK_LALT,   SDLK_RALT,
			SDLK_LGUI,   SDLK_RGUI:
				Exit;
		else
			GetNewKeyBinding(KeyList, Key, Shift, Result); // finalize
			Result := True;
		end;
	end
	else
		inherited;
end;

procedure TConfigScreen.ListKeyBindings;
var
	Section: TKeyBindings;
	Binding: TKeyBinding;
	LI: TCWEListItem;
begin
	KeyList.Items.Clear;
	for Section in Shortcuts.Sections do
	begin
		LI := TCWEListItem.Create(Section.Section, LISTITEM_HEADER, nil, HEADER_FG, HEADER_BG);
		KeyList.Items.Add(LI);

		for Binding in Section.Keys do
		begin
			LI := TCWEListItem.Create(
				Binding.Name + COLUMNSEPARATOR + ShortCutToText(Binding.Shortcut),
				0, Binding);
			KeyList.Items.Add(LI);
		end;
	end;
	KeyList.AdjustScrollbar;
end;

function TConfigScreen.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if not WaitingForKeyBinding then
		Result := inherited
	else
		Result := False;
end;

function TConfigScreen.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
begin
	if not WaitingForKeyBinding then
		Result := inherited
	else
		Result := False;
end;

procedure TConfigScreen.Paint;
begin
	if not WaitingForKeyBinding then
		inherited;
end;

procedure TConfigScreen.GetNewKeyBinding(Sender: TCWEControl;
	var Key: Integer; Shift: TShiftState; var Handled: Boolean);
const
	EDGE = 4;
var
	i: Integer;
	Binding: TKeyBinding;
begin
	Handled := False;

	i := KeyList.ItemIndex;
	if i < 0 then Exit;
	Binding := TKeyBinding(KeyList.Items[i].ObjData);
	if Binding = nil then Exit;

	if not WaitingForKeyBinding then
	begin
		if Key <> SDLK_RETURN then
			Exit;

		i := Console.Height div 2;
		Console.FrameRectPx(
			Types.Rect(EDGE, i-2, Console.Width-EDGE, i+3),
			False, True, 2);
		Console.WriteHeader(
			'Enter new keyboard shortcut now!', i-1, 0, 2,
			EDGE+1, Console.Width-EDGE-1);
		Console.WriteCentered(
			Binding.Name + ' = ' +
			ShortCutToText(Binding.Shortcut),
			i+1, EDGE+1, Console.Width-EDGE-1, 1, 2);

		WaitingForKeyBinding := True;
		Handled := True;
	end
	else
	begin
		Handled := True;
		if Key = 0 then Exit;

		Binding.Shortcut := ShortCut(Key, Shift);

		ListKeyBindings;
		WaitingForKeyBinding := False;

		Console.Clear(True, 0, 0, 2);
		Console.Refresh;
		Paint;
	end;
end;

// ============================================================================
{ TCWEConfigList }
// ============================================================================

constructor TCWEConfigList.Create;
begin
	inherited;

	ColorFore := 2; //TConsole.COLOR_PANEL;
	ColumnColor[0] := ColorFore;
	WantHover := True;
end;

procedure TCWEConfigList.Init(var aConfigManager: TConfigurationManager);
var
	CI: TConfigItem;
	LI: TCWEListItem;
	S: AnsiString;
	Sections: TStringList;
begin
	ConfigManager := aConfigManager;

	Sections := TStringList.Create;
	Sections.OwnsObjects := False;
	Sections.Duplicates := dupIgnore;
	Sections.Sorted := True;

	for CI in ConfigManager.Items do
		if CI.Caption <> '' then
			Sections.Add(CI.Section);

	Items.Clear;

	for S in Sections do
	begin
		if Sections.Count > 1 then
		begin
			LI := TCWEListItem.Create(S, LISTITEM_HEADER, nil, HEADER_FG, HEADER_BG);
			Items.Add(LI);
		end;
		for CI in ConfigManager.Items do
			if (CI.Section = S) and (CI.Caption <> '') then
			begin
				LI := TCWEListItem.Create('', 0, CI);
				ItemFromConfig(LI);
				Items.Add(LI);
			end;
	end;

	Sections.Free;
end;

procedure TCWEConfigList.ItemFromConfig(var LI: TCWEListItem);
var
	CI: TConfigItem;
	S: AnsiString;
begin
	if (LI = nil) then Exit;
	CI := TConfigItem(LI.ObjData);
	if (CI = nil) then Exit;

	S := CI.ValueToString;
	if S <> '' then
	begin
		LI.Captions[0] := CI.Caption + ':';
		LI.Captions[1] := S;
	end;
end;

function TCWEConfigList.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
var
	Sc: ControlKeyNames;
	CI: TConfigItem;
	LI: TCWEListItem;
	Modifier: Integer;
begin
	Result := False;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, []));
	case Sc of

		ctrlkeyLEFT, ctrlkeyRIGHT,
		ctrlkeyPLUS, ctrlkeyMINUS:
		begin
			LI := Items[ItemIndex];
			if LI.ObjData = nil then Exit;
			CI := TConfigItem(LI.ObjData);

			Modifier := 1;

			if Sc in [ctrlkeyPLUS, ctrlkeyMINUS] then
				if (CI.Max - CI.Min) > 20 then
					Modifier := CI.LargeStep;

			if Shift = [ssShift]  then
				Modifier := CI.LargeStep * 10;

			if Sc in [ctrlkeyLEFT, ctrlkeyMINUS] then
				CI.ModifyValue(-CI.Step * Modifier)
			else
				CI.ModifyValue(+CI.Step * Modifier);

			ItemFromConfig(LI);
			Result := True;
			Paint;

			if Assigned(CI.Callback) then CI.Callback;
		end;

	end;

	if not Result then
		Result := inherited;
end;

function TCWEConfigList.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
const
	Modifier = 1;
var
	CI: TConfigItem;
	LI: TCWEListItem;
begin
	if ((Selection3D) and (P.X <= 14)) or	// hack for ColorList
		((P.X + Rect.Left) < ColumnX[1]-1) then
	begin
		Result := inherited; // scroll
		Exit;
	end;

	LI := Items[ItemIndex];
	if LI.ObjData = nil then
		Exit(False);

	CI := TConfigItem(LI.ObjData);

	if WheelDelta < 0 then
		CI.ModifyValue(-CI.Step * Modifier)
	else
	if WheelDelta > 0 then
		CI.ModifyValue(+CI.Step * Modifier);

	ItemFromConfig(LI);
	Paint;
	Result := True;
end;

// ============================================================================
// Custom list painting for color settings
// ============================================================================

procedure TConfigScreen.ListPaintItem(Sender: TCWEControl;
	var Item: TCWEListItem;	Index: Word; P: TPoint);
var
	X, c: Integer;
	CI: TConfigItem;
begin
	if Sender <> ColorList then Exit;

	c := Index div 3;
	if c < 16 then
	begin
		with TCWETwoColumnList(Sender) do
		begin
			for X := Rect.Left+12 to Rect.Left+13 do
				Console.SetColor(X, P.Y, c, c);
			Data[1].Value := c;
			if c = 11 then
				Data[0].Value := 0
			else
				Data[0].Value := 11;
		end;
	end
	else
	if Item.Data = LISTITEM_HEADER then
		Sender.Data[1].Value := 15
	else
	begin
		CI := Item.ObjData;
		if (CI <> nil) and (CI is TConfigItemByte) then
		begin
			c := TConfigItemByte(CI).Value^;
			with TCWETwoColumnList(Sender) do
			begin
				for X := Rect.Left+15 to Rect.Left+16 do
					Console.SetColor(X, P.Y, c, c);
				Data[1].Value := c;
			end;
		end;
	end;
end;

end.

