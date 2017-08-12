unit Screen.Editor;

interface

uses
	Classes, Types, SysUtils, Math,
	TextMode, CWE.Core, CWE.Widgets.Text, CWE.Widgets.Numeric,
	ProTracker.Player, ProTracker.Editor;

const
	CMD_PATTERN_INSERT	= 1;
	CMD_PATTERN_DELETE	= 2;
	CMD_PATTERN_CLONE	= 3;

	LINE_PATTERN_END   = 45 - 1;
	LINE_PATTERN_BEGIN = LINE_PATTERN_END - 32;
	PATTERN_CHAN_WIDTH = 14;

	EffectHints: array[$0..$F] of AnsiString = (
		'Arpeggio',
		'Pitch slide up',
		'Pitch slide down',
		'Slide to note',
		'Vibrato',
		'Slide to note + Volume slide',
		'Vibrato + Volume slide',
		'Tremolo',
		'Unused',
		'Set sample offset',
		'Volume slide',
		'Jump to order',
		'Volume',
		'Pattern break',
		'Extended: ',
		'Set speed/tempo'
	);

	ExtEffectHints: array[$0..$F] of AnsiString = (
		'Set filter',
		'Fine pitch slide up',
		'Fine pitch slide down',
		'Set glissando',
		'Set vibrato waveform',
		'Set finetune',
		'Set/Jump to loop',
		'Set tremolo waveform',
		'Unused',
		'Retrigger note',
		'Fine volume slide up',
		'Fine volume slide down',
		'Note cut',
		'Note delay',
		'Pattern delay',
		'Invert loop'
	);


type
	TOrderlistCursor = record
		X, Y:		Byte;
	end;

	TOrderList = class(TCWEControl)
	public
		Cursor:		TOrderlistCursor;
		Offset:		Byte;

		procedure 	Paint; override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
	end;

	TEditorScreen = class(TCWEScreen)
	private
		DefaultMessage: String;
		AppStartedTime: TDateTime;
		VUClippedCounter: Integer;

		function 	ScopeWheel(Sender: TCWEControl; Shift: TShiftState;
					DirDown: Boolean; P: TPoint): Boolean;
	public
		lblAmp,
		lblMessage,
		lblSizeSong,
		lblSizeSamples,
		lblSizeTotal,
		lblTimeDisplay,
		lblPlayMode: 	TCWELabel;
		lblFilename,
		lblSongTitle:	TCWEEdit;
		lblPattern,
		lblOrder,
		lblSample,
		lblOctave,
		lblTempo: 		TCWESunkenLabel;
		Scope:			TCWEControl;
		AmpSlider:		TCWESlider;
		lblChannels:	array [0..AMOUNT_CHANNELS-1] of TCWELabel;

		procedure 	Reset;
		procedure 	Paint; override;

		procedure	UpdateVUMeter(Len: Integer);
		procedure	UpdateTimeDisplay;
		procedure 	UpdateInfoLabels(Repaint: Boolean = False);
		procedure 	MessageText(const S: String; LogIt: Boolean = False);

		procedure 	ToggleChannel(Channel: Byte);
		procedure 	SetOctave(Hi: Boolean);
		procedure 	SetSample(i: Integer = -1);
		procedure 	SelectPrevPattern;
		procedure 	SelectNextPattern;
		procedure 	SeekTo(Order, Row: Byte);

		function	OnContextMenu: Boolean; override;
		procedure 	HandleCommand(const Cmd: Cardinal); override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;

		procedure 	AmplificationChange(Sender: TCWEControl);
		procedure 	SongTitleChanged(Sender: TCWEControl);
		function	LabelMouseWheel(Sender: TCWEControl;
					Shift: TShiftState; DirDown: Boolean; P: TPoint): Boolean;

		procedure	Show; override;
		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;


var
	Editor: 		TEditorScreen;
	PatternEditor: 	TPatternEditor;
	OrderList: 		TOrderList;


implementation

uses
	MainWindow, BuildInfo, ShortcutManager, Layout, CWE.MainMenu,
	SDL.Api.Types, Graphics32,
	ProTracker.Sample,
	ProTracker.Util;

type
	OrderListKeyNames = {%H-}(
		keyNONE,
		keySetLength,
		keyEditPattern
	);

var
	{%H-}OrderlistKeys: TKeyBindings;


// ==========================================================================
// TEditorScreen
// ==========================================================================

constructor TEditorScreen.Create;

	procedure EnableMouseOn(const lbl: TCWEControl);
	begin
		lbl.OnMouseWheel := LabelMouseWheel;
		lbl.WantHover := True;
	end;

var
	i: Integer;
	ctrl: TCWEControl;
begin
	inherited;

	RegisterScreenLayout(Self, 'Editor');

	CurrentPattern := 0;

	DefaultMessage := 'Propulse Tracker ' + ProTracker.Util.VERSION +
	{$IFDEF DEBUG} ' [DEBUG]' + {$ENDIF}
		' '#7' ' + Build.CompileDate;

	// Labels
	//
	lblMessage := TCWELabel.Create(Self,
		DefaultMessage, 'App Title and Build Data',
		Types.Rect(10, 1, Con.Width-12, 2), True);
	(lblMessage as TCWELabel).Alignment := ALIGN_CENTER;
	lblMessage.ColorFore := TConsole.COLOR_3DLIGHT;
	lblMessage.ColorBack := TConsole.COLOR_3DDARK;

	i := 1;
{	AddControl(TCWELabel, 'Songname', '', Bounds(i, 3, 8, 1));
	AddControl(TCWELabel, 'Filename', '', Bounds(i, 5, 8, 1));
	AddControl(TCWELabel, 'Pattern',  '', Bounds(i, 7, 8, 1));
	AddControl(TCWELabel, 'Order',    '', Bounds(i + 16, 7, 5, 1));}

	Inc(i, 9);
	lblSongTitle := TCWEEdit.Create(Self, '', 'Song Title',
		Bounds(i, 3, 20, 1), True);
	lblSongTitle.MaxLength := 20;
	lblSongTitle.OnChange := SongTitleChanged;
	lblSongTitle.ReportAnyChange := True;
	lblSongTitle.WantHover := True;
	RegisterLayoutControl(lblSongTitle, CTRLKIND_BOX, False, True, False);

	lblFilename := TCWEEdit.Create(Self, '', 'Song Filename',
		Bounds(i, 5, 20, 1), True);
	lblFilename.WantHover := True;
	RegisterLayoutControl(lblFilename, CTRLKIND_BOX, False, True, False);

	lblPattern := TCWESunkenLabel.Create(Self, '00/01', 'PatternNum',
		Bounds(i, 7, 5, 1), True);
	EnableMouseOn(lblPattern);

	lblOrder := TCWESunkenLabel.Create(Self, '000/000', 'OrderNum',
		Bounds(i + 13, 7, 7, 1), True);
//	EnableMouseOnLabel(lblOrder);

	i := 32;
{	AddControl(TCWELabel, 'Sample', '', Bounds(i, 3, 6, 1));
	AddControl(TCWELabel, 'Octave', '', Bounds(i, 5, 6, 1));
	AddControl(TCWELabel, 'Tempo',  '', Bounds(i, 7, 6, 1));}

	Inc(i, 7);
	lblSample := TCWESunkenLabel.Create(Self,
		'00:......................', 'Current Sample',
		Bounds(i, 3, 22+3, 1), True);
	lblOctave := TCWESunkenLabel.Create(Self,
		'Hi', 'Current Octave',
		Bounds(i, 5, 2, 1), True);
	lblTempo := TCWESunkenLabel.Create(Self,
		'006/125', 'Current Speed',
		Bounds(i, 7, 7, 1), True);

	EnableMouseOn(lblSample);
	EnableMouseOn(lblOctave);

	lblPlayMode := TCWELabel.Create(Self,
		'Stopped', 'Playback Indicator',
		Bounds({i + 11, 7,} 1, 1, 9, 1), True);
	lblPlayMode.ColorFore := TConsole.COLOR_3DLIGHT;
	lblPlayMode.ColorBack := TConsole.COLOR_3DDARK;

	i := 66;
	lblTimeDisplay := TCWELabel.Create(Self,
		'0:00:00', 'Time Display',
		Bounds(i, 3, 13, 1), True);
	lblTimeDisplay.ColorFore := TConsole.COLOR_3DLIGHT;
	lblTimeDisplay.ColorBack := TConsole.COLOR_3DDARK;

	lblSizeSong := TCWELabel.Create(Self,
		'Song:    000k', 'Song size',
		Bounds(i, 5, 13, 1), True);
	lblSizeSamples := TCWELabel.Create(Self,
		'Samples: 000k', 'Samples size',
		Bounds(i, 6, 13, 1), True);
	lblSizeTotal := TCWELabel.Create(Self,
		'Total:   000k', 'Total size',
		Bounds(i, 7, 13, 1), True);

	Scope := TCWEControl.Create(Self, '', 'Scope', Bounds(52, 5, 12, 3), True);
	Scope.SetData(0, 13, 'Clipped color');
	EnableMouseOn(Scope);
	Scope.OnMouseWheel := ScopeWheel;
	Scope.WantHover := False;
	Scope.WantMouse := True;
	RegisterLayoutControl(Scope, CTRLKIND_BOX, False, True, True);

	//Type=Text	ID=AmpLabel	X1=66	X2=70	Y1=06
	lblAmp := TCWELabel.Create(Self, 'Amp', 'AmpLabel', Bounds(79-3, 6, 4, 1), True);

	//Type=Box	ID=AmpSlider	X1=71	X2=79
	AmpSlider := TCWESlider.Create(Self, '', 'AmpSlider', Types.Rect(66, 6, 79-4, 7), True);
	AmpSlider.Min := 0;
	AmpSlider.Max := 100;
	AmpSlider.OnChange := AmplificationChange;
	RegisterLayoutControl(TCWEControl(AmpSlider), CTRLKIND_BOX, False, True, False);


	// Section headers
	//
{	ctrl := TCWELabel.Create(Self,
		'Pattern Editor (F2)', '', Types.Rect(1, 8, 59, 9));
	(ctrl as TCWELabel).Alignment := ALIGN_HEADER;
	ctrl := TCWELabel.Create(Self,
		'Orders (F11)', '', Types.Rect(61, 8, 79, 9));
	(ctrl as TCWELabel).Alignment := ALIGN_HEADER;}

	// Pattern editor widget
	//
	PatternEditor := TPatternEditor.Create(Self, '', 'PatternView',
		Bounds(1, 12, AMOUNT_CHANNELS * (PATTERN_CHAN_WIDTH + 1) - 2, 32), True);
	RegisterLayoutControl(TCWEControl(PatternEditor), CTRLKIND_BOX, True, False, True);

	// Orderlist editor widget
	//
	Orderlist := TOrderList.Create(Self, '', 'OrderList',
		Types.Rect(61, LINE_PATTERN_BEGIN, 67, LINE_PATTERN_END), True);
	Orderlist.SetBorder(True, True, True, False);
	Orderlist.ColorBack := TConsole.COLOR_BLANK;
	RegisterLayoutControl(TCWEControl(OrderList), CTRLKIND_BOX, True, False, True);
	Orderlist.WantMouse := True;
	Orderlist.WantKeyboard := True;

	// Channel labels
	//
	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
		ctrl := TCWELabel.Create(Self, 'Channel ' + IntToStr(i+1), '', Bounds(
			PatternEditor.Rect.Left + 3 + (i * PATTERN_CHAN_WIDTH),
			PatternEditor.Rect.Top-1, PATTERN_CHAN_WIDTH-1, 1), True);
		ctrl.ColorFore := TConsole.COLOR_3DLIGHT;
		ctrl.ColorBack := TConsole.COLOR_3DDARK;
		(ctrl as TCWELabel).Alignment := ALIGN_CENTER;
		lblChannels[i] := TCWELabel(ctrl);
	end;

	SetOctave(True);
	ActiveControl := PatternEditor;

	with Shortcuts do
	begin
		OrderlistKeys := SetContext('OrderList');

		Bind(keySetLength,		'SetLength',	ShortCut(SDLK_SPACE));
		Bind(keyEditPattern,	'EditPattern',	ShortCut(SDLK_RETURN));
	end;

	// register all label controls
	RegisterLayoutControlClass(TCWEControl(Self), TCWELabel, CTRLKIND_LABEL, False, True, False);

	LoadLayout(Self);

	lblMessage.SetCaption(DefaultMessage);

	AppStartedTime := Now;
end;

procedure TEditorScreen.AmplificationChange(Sender: TCWEControl);
var
	amp: Single;
begin
	amp := AmpSlider.Position / 10;
	lblAmp.SetCaption(Format('%.1f', [amp]));
	Options.Audio.Amplification := 10.0 - amp;
	Module.ApplyAudioSettings;
end;

procedure TEditorScreen.UpdateVUMeter(Len: Integer);
var
	p: Single;
	C: TColor32;
	X, Y, ox, oy, w, h, hh: Integer;
	R: TRect;
begin
	// Channel VUmeters
	//
	for hh := 0 to AMOUNT_CHANNELS-1 do
	begin
		h := Console.Font.Height div 2;
		w  := (Editor.lblChannels[hh].Width + 1) * Console.Font.Width;
		oy := (Editor.lblChannels[hh].Rect.Top - 1) * (h*2) + h;
		ox := (Editor.lblChannels[hh].Rect.Left) * Console.Font.Width;

		Console.Bitmap.FillRect(Bounds(ox, oy, w, h),
			Console.Palette[Editor.lblChannels[hh].ColorBack]);

		if Module.Channel[hh].Paula.Volume > 0 then
		begin
			p := (w - 2) * Module.Channel[hh].Paula.Volume;
			Console.Bitmap.FillRect(Bounds(ox + 1, oy + 1, Trunc(p), h-2),
				Console.Palette[Editor.lblChannels[hh].ColorFore]);
			Module.Channel[hh].Paula.Volume := Module.Channel[hh].Paula.Volume - 0.025;
		end;
	end;

	// Scope
	//
	Editor.Scope.GetPixelRect(R{%H-});

	if Module.ClippedSamples > 0 then
		VUClippedCounter := 3;

	if VUClippedCounter > 0 then
	begin
		// C := Color32(VUClippedCounter*16, 0, 0);
		C := Console.Palette[Editor.Scope.Data[0].Value];
		Dec(VUClippedCounter);
	end
	else
		C := Console.Palette[Editor.Scope.ColorBack];

	Console.Bitmap.FillRect(R, C);

	C := Console.Palette[Editor.Scope.ColorFore];

	ox := R.Left;
	oy := R.Top;
	w := R.Right - R.Left - 1;
	h := R.Bottom - R.Top - 1;
	hh := h div 2;

	if Len = 0 then
		Console.Bitmap.HorzLine(ox, oy + hh, ox + w, C)
	else
	begin
		p := (Len div 2 - 1) / w; // step
		for X := 0 to w do
		begin
			Y := VUbuffer[Trunc(p * X)] + VUbuffer[Trunc(p * X) + 1];
			Y := Trunc(Y / 2 / 65536 * h);
			Console.Bitmap.Pixel[ox + X, oy + Y + hh] := C;
		end;
	end;
end;

function TEditorScreen.ScopeWheel(Sender: TCWEControl;
	Shift: TShiftState; DirDown: Boolean; P: TPoint): Boolean;
var
	B: PByte;
begin
	Result := True;
	if ssShift in Shift then
		B := @Scope.ColorBack
	else
		B := @Scope.ColorFore;

	if (not DirDown) and (B^ > 0) then
		Dec(B^)
	else
	if (DirDown) and (B^ < 15) then
		Inc(B^);
end;

procedure TEditorScreen.MessageText(const S: String; LogIt: Boolean = False);
begin
	if S = '' then
	begin
		Window.MessageTextTimer := -1;
		lblMessage.SetCaption(DefaultMessage);
	end
	else
	begin
		Window.MessageTextTimer := 2000 div Window.TimerInterval;
		lblMessage.SetCaption(S);
		if LogIt then Log(S);
	end;
end;

procedure TEditorScreen.SongTitleChanged(Sender: TCWEControl);
begin
	Module.SetTitle(lblSongTitle.Caption);
	Module.SetModified(True, True);
end;

function TEditorScreen.LabelMouseWheel(Sender: TCWEControl;
	Shift: TShiftState; DirDown: Boolean; P: TPoint): Boolean;
var
	lbl: TCWELabel;
	Delta: Integer;
begin
	Result := True;
	if not (Sender is TCWELabel) then Exit;

	if DirDown then
		Delta := 1
	else
		Delta := -1;

	lbl := TCWELabel(Sender);

	if lbl = lblPattern then
	begin
		if Delta < 0 then
			SelectPrevPattern
		else
			SelectNextPattern;
	end
	else
	if lbl = lblOrder then
	begin
		//
	end
	else
	if lbl = lblSample then
	begin
		SetSample(CurrentSample + Delta);
	end
	else
	if lbl = lblOctave then
	begin
		SetOctave(Delta > 0);
	end;
end;

procedure TEditorScreen.SetOctave(Hi: Boolean);
begin
	PatternEditor.HighOctave := Hi;
	if Hi then
		lblOctave.SetCaption('Hi')
	else
		lblOctave.SetCaption('Lo');
end;

procedure TEditorScreen.ToggleChannel(Channel: Byte);
begin
	Module.Channel[Channel].Enabled := not Module.Channel[Channel].Enabled;
	UpdateInfoLabels(True);
end;

procedure TEditorScreen.SelectPrevPattern;
begin
	if {(Module.PlayMode = PLAY_SONG)} FollowPlayback then
		with Module do
		begin
			PlayPos.Order := Max(PlayPos.Order - 1, 0);
			PlayPos.Pattern := OrderList[Module.PlayPos.Order];
			PlayPos.Row := 0;
			RepostChanges;
		end
	else
	if CurrentPattern > 0 then
	begin
		if 	(Module.Info.PatternCount = CurrentPattern) and
			(Module.IsPatternEmpty(CurrentPattern)) then
				Module.CountUsedPatterns;
				//Module.Info.PatternCount := CurrentPattern;
		Dec(CurrentPattern);
		UpdateInfoLabels;
	end;

	PatternEditor.ValidateCursor;
	if Active then
		PatternEditor.Paint;
end;

procedure TEditorScreen.SelectNextPattern;
begin
	if FollowPlayback then
		Module.NextPosition
	else
	if CurrentPattern < MAX_PATTERNS-1 then
	begin
		Inc(CurrentPattern);
		UpdateInfoLabels;
	end;
	PatternEditor.ValidateCursor;
	if Active then
		PatternEditor.Paint;
end;

procedure TEditorScreen.SeekTo(Order, Row: Byte);
begin
	Module.PlayPos.Order := Order;
	CurrentPattern := Module.OrderList[Order];
	PatternEditor.Cursor.Row := Row;

	UpdateInfoLabels;
	PatternEditor.ValidateCursor;

	FollowPlayback := False;
	Editor.ActiveControl := PatternEditor;
	ChangeScreen(TCWEScreen(Editor));

	if Active then
		PatternEditor.Paint;
end;

procedure TEditorScreen.HandleCommand(const Cmd: Cardinal);
begin
	case Cmd of

		CMD_PATTERN_INSERT:
		;

		CMD_PATTERN_DELETE:
		;

		CMD_PATTERN_CLONE:
		;

	end;
end;

function TEditorScreen.KeyDown;
var
	Sc: EditorKeyNames;
begin
	Result := False;

	Sc := EditorKeyNames(Shortcuts.Find(EditorKeys, Key, Shift));

	// don't inherit - we don't want tab to iterate controls here
	case Sc of

		keySelectOctaveLo,
		keySelectOctaveHi:
		begin
			SetOctave(Sc = keySelectOctaveHi);
			Result := True;
		end;

		// previous pattern
		keySelectPatternPrev:
		begin
			SelectPrevPattern;
			Result := True;
		end;

		// next pattern
		keySelectPatternNext:
		begin
			SelectNextPattern;
			Result := True;
		end;

	end;

	if (not Result) and (ActiveControl <> nil) then
		Result := ActiveControl.KeyDown(Key, Shift);
end;

procedure TEditorScreen.Paint;
var
	x, y: Integer;
begin
	if not Active then Exit;

	Console.BeginUpdate;

	// pattern view
	//
	PatternEditor.DrawBorder(Types.Rect(
		PatternEditor.Rect.Left+3, PatternEditor.Rect.Top,
		PatternEditor.Rect.Right, PatternEditor.Rect.Bottom));

	// channel separators
	//
	for x := 1 to AMOUNT_CHANNELS-1 do
	for y := PatternEditor.Rect.Top to PatternEditor.Rect.Bottom-1 do
		Console.PutChar(x * PATTERN_CHAN_WIDTH + (PatternEditor.Rect.Left + 2), y, 168, 2, 0);

	UpdateInfoLabels;

	inherited Paint;

	UpdateVUMeter(0);

	Console.EndUpdate;
end;

procedure TEditorScreen.Reset;
begin
	with PatternEditor do
	begin
		Cursor.X := 0;
		Cursor.Row := 0;
		Cursor.Column := COL_NOTE;
		Cursor.Channel := 0;
		PatternEditor.ValidateCursor;
	end;
	with OrderList do
	begin
		Cursor.X := 0;
		Cursor.Y := 0;
		Offset := 0;
	end;
	Module.CountUsedPatterns;
	UpdateInfoLabels;
end;

procedure TEditorScreen.UpdateInfoLabels(Repaint: Boolean = False);
var
	i, x, y, xx, sizePatt, sizeSamp: Integer;
	S: AnsiString;
	ch: AnsiChar;
const
	CHR_ARROW    = #171;
	CHR_TRIANGLE = #169;
	CHR_PERIOD   = #170;
	CHR_EMPTY    = #143;
	//CHR_SPACE    = #32;
begin
	if not Active then Exit;

	{if Module.PlayMode = PLAY_SONG then
	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
	end}

	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
		if PatternEditor.Cursor.Channel = i then
			lblChannels[i].ColorBack := 7
		else
			lblChannels[i].ColorBack := 1;

		if Module.Channel[i].Enabled then
			lblChannels[i].ColorFore := 3
		else
			lblChannels[i].ColorFore := 0;

		// channel mask indicator
		//
		x := lblChannels[i].Rect.Left;
		y := PatternEditor.Rect.Bottom;

		if PatternEditor.Cursor.Channel = i then
		begin
			if PatternEditor.Cursor.Column = COL_NOTE then
			begin
				S := CHR_ARROW + CHR_ARROW + CHR_ARROW + CHR_EMPTY;
				ch := CHR_TRIANGLE;
			end
			else
			begin
				ch := CHR_PERIOD;
				S := CHR_PERIOD + CHR_PERIOD + CHR_PERIOD + CHR_EMPTY;
			end;

			if PatternEditor.EditMask[EM_SAMPLE] then
				S := S + CH + CH + CHR_EMPTY
			else
				S := S + CHR_EMPTY + CHR_EMPTY + CHR_EMPTY;
			if PatternEditor.EditMask[EM_VOLUME] then
				S := S + CH + CH + CHR_EMPTY
			else
				S := S + CHR_EMPTY + CHR_EMPTY + CHR_EMPTY;
			if PatternEditor.EditMask[EM_EFFECT] then
				S := S + CH + CH + CH + CHR_EMPTY
			else
				S := S + CHR_EMPTY + CHR_EMPTY + CHR_EMPTY;

			xx := PatternEditor.Cursor.X mod 14;
			if xx in [2..12] then
				S[xx+1] := CHR_ARROW;
		end
		else
			S := StringOfChar(CHR_EMPTY, 13);
		Console.Write(S, x, y, 3);
	end;

	lblPattern.SetCaption(
		Format('%.2d/%.2d', [CurrentPattern, Module.Info.PatternCount]));
	lblOrder.SetCaption(
		Format('%.3d/%.3d', [Module.PlayPos.Order, Module.Info.OrderCount-1]));
	lblTempo.SetCaption(
		Format('%.3d/%.3d', [Module.CurrentSpeed, Module.CurrentBPM]));

	sizeSamp := 0;
	for sizePatt := 0 to Module.Samples.Count-1 do
		Inc(sizeSamp, Module.Samples[sizePatt].Length * 2);
	sizePatt := 1084 + ((Module.Info.PatternCount + 1) * 1024);

	i := Round(Max(sizePatt, sizeSamp) / 1024);
	if i < 10 then
		x := 4
	else
	if i < 100 then
		x := 5
	else
	if i < 1000 then
		x := 6
	else
		x := 7;

	lblSizeSong.SetCaption(
		Format('Song:   %*.1fK', [x, sizePatt / 1024]));
	lblSizeSamples.SetCaption(
		Format('Samples:%*.1fK', [x, sizeSamp / 1024]));
	lblSizeTotal.SetCaption(
		Format('Total:  %*.1fK', [x, (sizePatt + sizeSamp) / 1024]));

	if Repaint then Paint;
end;

procedure TEditorScreen.UpdateTimeDisplay;
var
	Hour, Min, Sec, MSec: Word;
	Diff: TTime;
begin
	if Module.PlayMode = PLAY_STOPPED then
		Diff := Now - AppStartedTime
	else
		Diff := Now - Module.PlayStarted;
	DecodeTime(Diff, Hour, Min, Sec, MSec);
	lblTimeDisplay.SetCaption({'Time:' + }Format('%2d:%.2d:%.2d', [Hour, Min, Sec]));
end;

procedure TEditorScreen.SetSample(i: Integer = -1);
begin
	if i in [1..31] then
		CurrentSample := i;
	if IsEmptySample(Module.Samples[CurrentSample-1]) then
		lblSample.ColorFore := 2
	else
		lblSample.ColorFore := 5;
	lblSample.SetCaption(
		Format('%.2d:%s', [CurrentSample, Module.Samples[CurrentSample-1].Name]));
end;

procedure TEditorScreen.Show;
begin
	AmpSlider.Position := 100 - Trunc(Options.Audio.Amplification * 10);
	inherited;
end;

function TEditorScreen.OnContextMenu: Boolean;
begin
	inherited;
	with ContextMenu do
	begin
		SetSection(EditorKeys);

		AddSection('Block operations');

		AddCmd(Ord(keyBlockCut),				'Cut');
		AddCmd(Ord(keyBlockCopy),				'Copy');
		AddCmd(Ord(keyBlockPaste),				'Paste');
		AddCmd(Ord(keyBlockOverwrite),			'Overwrite');
		AddCmd(Ord(keyBlockMix),				'Mix');
		AddCmd(Ord(keyBlockSwap),				'Swap');
		AddCmd(Ord(keyBlockDouble),				'Double size');
		AddCmd(Ord(keyBlockHalve),				'Halve size');
		AddCmd(Ord(keyBlockSlideWipeEffect),	'Slide effect values');
		AddCmd(Ord(keyBlockSetSample),			'Replace sample');
		AddCmd(Ord(keyTransposeSemitoneUp),		'Transpose semitone up');
		AddCmd(Ord(keyTransposeSemitoneDown),	'Transpose semitone down');
		AddCmd(Ord(keyTransposeOctaveUp),		'Transpose octave up');
		AddCmd(Ord(keyTransposeOctaveDown),		'Transpose octave down');

		{AddSection('Pattern operations'); 		// handled in Screen.Editor

		AddCmdEx(CMD_PATTERN_INSERT,		 		'Insert pattern');
		AddCmdEx(CMD_PATTERN_DELETE,		 		'Delete pattern');
		AddCmdEx(CMD_PATTERN_CLONE,		 		'Duplicate pattern');}
	end;
	Result := True;
end;


// ==========================================================================
// TOrderList
// ==========================================================================

function TOrderList.KeyDown;
var
	p, i: Integer;
	Sc: ControlKeyNames;
begin
	if Key = 0 then Exit(False);
	Result := True;

	if Key = SDLK_F7 then
	begin
		Module.Play(Cursor.Y, 0);
		Exit;
	end;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	case Sc of

		ctrlkeyUP:
			if Cursor.Y > 0 then
				Dec(Cursor.Y);

		ctrlkeyDOWN:
			if Cursor.Y < 127 then
				Inc(Cursor.Y);

		ctrlkeyLEFT,
		ctrlkeyRIGHT:
			Cursor.X := 1 - Cursor.X;

		ctrlkeyRETURN,
		ctrlkeyTAB:
		begin
			if Sc = ctrlkeyRETURN then
				CurrentPattern := Module.OrderList[Cursor.Y];
			Editor.ActiveControl := PatternEditor;
			Editor.Paint;
		end;

		ctrlkeySPACE:
		begin
			Module.Info.OrderCount := Cursor.Y + 1;
			Editor.UpdateInfoLabels;
		end;

		ctrlkeyINSERT:
		begin
			Module.OrderList.Insert(Cursor.Y, CurrentPattern);
			if Module.Info.OrderCount < 127 then
				Inc(Module.Info.OrderCount);
		end;

		ctrlkeyDELETE:
		begin
			Module.OrderList.Delete(Cursor.Y);
			if (Module.Info.OrderCount > 0) and (Cursor.Y < Module.Info.OrderCount) then
				Dec(Module.Info.OrderCount);
		end;

		ctrlkeyHOME:
			Cursor.Y := 0;

		ctrlkeyEND:
			Cursor.Y := 127;

		ctrlkeyPGUP:
			Cursor.Y := Max(0, Cursor.Y - Height);

		ctrlkeyPGDN:
			Cursor.Y := Min(127, Cursor.Y + Height);

	else
		i := Pos(Chr(Key), KeyboardNumbers) - 1;
		if i >= 0 then
		begin
			p := Module.OrderList[Cursor.Y];
			if Cursor.X = 0 then
			begin
				Module.OrderList[Cursor.Y] := (i * 10) + (p mod 10);
				Cursor.X := 1;
			end
			else
			begin
				Module.OrderList[Cursor.Y] := (p div 10 * 10) + i;
				Cursor.X := 0;
				Inc(Cursor.Y);
			end;
		end
		else
			Exit(False); // no need to repaint
	end;

	// fix cursor coords
	//
	//i := Height; //31
	if (Offset > 0) and (Cursor.Y < Offset) then
		Offset := Cursor.Y
	else
	if Cursor.Y >= (Offset + Height) then
		Offset := Min(Cursor.Y - Height, 127-31);

	Paint;
end;

procedure TOrderList.Paint;
var
	y, i: Integer;
	con, col: Byte;
begin
//	inherited;
	DrawBorder(Types.Rect(Rect.Left + 4, Rect.Top, Rect.Right, Rect.Bottom), ColorBack);

	if (Module.PlayMode = PLAY_SONG) and (Module.Info.OrderCount > Height) then
	begin
		y := Module.PlayPos.Order;
		if y > (Offset + Height div 2) then
			Offset := Min(y - (Height div 2), 127-31)
		else
		if y < Offset then
			Offset := 0;
	end;

	for y := 0 to Height do
	begin
		i := y + Offset;

		if (Module.PlayMode = PLAY_SONG) and (i = Module.PlayPos.Order) then
			con := 3
		else
			con := 0;

		if i >= Module.Info.OrderCount then
			col := 1
		else
			col := 2;

		Console.Write(
			Format('%.3d', [i]),
			Rect.Left, Rect.Top + y, con);
		Console.Write(
			TextVals[Module.OrderList[i]],
			Rect.Left + 4, Rect.Top + y, col);
	end;

	// draw cursor
	//
	if Focused then
	begin
		y := Rect.Top + Cursor.Y - Offset;
		if (y >= Rect.Top) and (y < Rect.Bottom) then
			Console.SetColor(Cursor.X + Rect.Left + 4, y, 0, 3);
	end;
end;

end.
