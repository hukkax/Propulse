unit ProTracker.Editor;

interface

uses
//	Windows,
	Classes, Types, SysUtils, Math, SDL2,
	TextMode, CWE.Core, CWE.Widgets.Text, ShortcutManager,
	ProTracker.Player;

{.$DEFINE TIMING}

const
	CmdCharsITExt: AnsiString = 'QSDZ';
	CmdCharsPT:    AnsiString = '0123456789AB DEF';
	CmdCharsIT:    AnsiString = 'JFEGHLKRXODB C AQSDZ';
	//CmdCharsIT:  AnsiString = 'JFEGHLKRXODB CZA';

	KeyboardNotes      = 'ZSXDCVGBHNJMQ2W3ER5T6Y7UI9O0P';
	KeyboardNumbers    = '0123456789';
	KeyboardHexNumbers = KeyboardNumbers + 'ABCDEF';
	//KeyboardEffects    = KeyboardHexNumbers;

	EM_SAMPLE = 0;
	EM_VOLUME = 1;
	EM_EFFECT = 2;


type
	PasteMode = (pstInsert, pstOverwrite, pstMix);

	EditColumn = (
		COL_NOTE, COL_OCTAVE,
		COL_SAMPLE_1, COL_SAMPLE_2,
		COL_VOLUME_1, COL_VOLUME_2,
		COL_COMMAND,
		COL_PARAMETER_1, COL_PARAMETER_2,
		COL_NEXTCHANNEL
	);

	EditorKeyNames = (
		keyNONE,
		keyNoteC_lo,	keyNoteCClo,	keyNoteD_lo,	keyNoteDDlo,
		keyNoteE_lo,	keyNoteF_lo,	keyNoteFFlo,	keyNoteG_lo,
		keyNoteGGlo,	keyNoteA_lo,	keyNoteAAlo,	keyNoteB_lo,
		keyNoteC_hi,	keyNoteCChi,	keyNoteD_hi,	keyNoteDDhi,
		keyNoteE_hi,	keyNoteF_hi,	keyNoteFFhi,	keyNoteG_hi,
		keyNoteGGhi,	keyNoteA_hi,	keyNoteAAhi,	keyNoteB_hi,
		keyNoteModifierPreview,		keyNoteClear,
		keyNoteDelete,				keyNoteInsert,
		keyNoteDeleteRow,			keyNoteInsertRow,
		keyNoteUseLast,				keyNoteGetLast,
		keyNotePlay,				keyNotePlayRow,
		keyMoveUp,					keyMoveDown,
		keyMoveLeft,				keyMoveRight,
		keyMoveUpAlt,				keyMoveDownAlt,
		keyMoveLeftAlt,				keyMoveRightAlt,
		keyMoveForwards,			keyMoveBackwards,
		keyMovePgUp,				keyMovePgDn,
		keyMoveTop,					keyMoveBottom,
		keyMoveHome,				keyMoveEnd,
		keyPlaybackPlayFrom,		keySelectModifierSkipValue,
		keySelectPatternNext,		keySelectPatternPrev,
		keySelectOctaveLo,			keySelectOctaveHi,
		keySelectSampleNext,		keySelectSamplePrev,
		keyBlockMarkStart,			keyBlockMarkEnd,
		keyBlockMarkWhole,			keyBlockUnmark,
		keyBlockModifier,
		keyBlockCut,				keyBlockCutMasked,			keyBlockCopy,
		keyBlockPaste,				keyBlockOverwrite,			keyBlockMix,
		keyBlockPasteMasked,		keyBlockOverwriteMasked,	keyBlockMixMasked,
		keyBlockDouble,				keyBlockHalve,
		keyBlockSetSample,			keyBlockSlideWipeEffect,
		keyBlockSwap,				keyToggleEditMask,
		keyTransposeSemitoneUp,		keyTransposeSemitoneDown,
		keyTransposeOctaveUp,		keyTransposeOctaveDown,
		keyChannelToggle,			keyChannelSolo
	);

	TEditCursor = record
		X,
		Row,
		Channel:	Byte;
		Column:		EditColumn;
		Note:		PNote;
	end;

	TClipPattern = record
		Notes: 		array [0..AMOUNT_CHANNELS-1, 0..63] of TNote;
		Size:       TPoint;
		//procedure	FromClipboard;
		//procedure	ToClipboard;
	end;

	TPatternEditor = class(TCWEControl)
	class var
		COL_INVALID,

		COL_HL_ROW,			COL_HL_BEAT,

		COL_SEL_BG,			COL_SEL_FG,
		COL_HL_SEL_ROW,		COL_HL_SEL_BEAT,

		COL_CUR_FG,			COL_CUR_BG,

		COL_ROW,
		COL_ROW_CURRENT,	COL_ROW_CURRENTOTHER:		Byte;

	private
		Locked:		Boolean;
		Drawing:	Boolean;
		Marking: 	Boolean;
		MarkPos: 	TPoint;
	public
		Cursor:		TEditCursor;
		Selection: 	TRect;
		NoteStep,
		ScrollPos:	Byte;
		ClipBuf: 	TClipPattern;
		HighOctave:	Boolean;
		LastNote:	TNote;
		EditMask:	array[EM_SAMPLE..EM_EFFECT] of Boolean;

		procedure 	SetModified(B: Boolean = True; Force: Boolean = False);
		procedure	ValidateCursor;

		function 	GetNote(Pattern, Channel, Row: Byte): PNote;
		function 	IsValidPosition(Pattern, Channel, Row: Byte): Boolean;
		function 	SetNote(Pattern, Channel, Row: Byte; Note: TNote;
					Masked: Boolean = False): Boolean;

		procedure 	InsertNote(Pattern, Channel, Row: Byte; WholePattern: Boolean = False);
		procedure 	DeleteNote(Pattern, Channel, Row: Byte; WholePattern: Boolean = False);

		procedure 	NoteTranspose(Pattern, Channel, Row: Byte; Semitones: ShortInt);
		procedure 	Transpose(Semitones: ShortInt);

		procedure 	BufferClear(R: TRect; Masked: Boolean = False);
		procedure 	BufferCopy(R: TRect);
		procedure 	BufferPaste(Mix: PasteMode; Masked: Boolean = False);
		procedure 	BlockSwap;
		procedure 	BlockDoubleOrHalve(DoubleIt: Boolean);
		procedure	BlockWipeEffects;
		procedure	BlockSlideEffect;
		procedure 	BlockSetSample;
		function 	PrepareSelectionForRender: Byte;

		procedure 	ReplaceSample(iFrom, iTo: Byte);

		procedure 	SelectChannel(Ch: Integer);
		procedure 	Advance;

		procedure 	MessageText(const S: String); inline;
		procedure 	SaveModule(const Filename: String = '');

		procedure 	Paint; override;

		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

var
	CmdChars: AnsiString;
	CurrentSample,
	CurrentOrder,
	CurrentPattern: Byte;
	EditorKeys: TKeyBindings;

const
	EditColumnX: array [EditColumn] of Byte = ( 0, 2, 4,5, 7,8,  10,11,12, 0 );


implementation

uses
	MainWindow,
	{$IFDEF TIMING}
	TimeMeasurer,
	{$ENDIF}
	//ClipBrd,
	ProTracker.Util,
	Screen.Log,
	Screen.FileReq,
	Screen.Editor,
	CWE.Dialogs,
	Dialog.RenderAudio;

{$IFDEF TIMING}
var
	Time: TTimeMeasurer;
{$ENDIF}


// ==========================================================================
// TPatternEditor
// ==========================================================================

constructor TPatternEditor.Create;
begin
	inherited;

	Locked := False;
	NoteStep := 1;
	Selection := Types.Rect(-1, -1, -1, -1);

	SetBorder(True, True, True, False);

	WantKeyboard := True;
	WantMouse := True;
	WantHover := False;

	EditMask[EM_SAMPLE] := True;
	EditMask[EM_VOLUME] := True;
	EditMask[EM_EFFECT] := False;

	{$IFDEF TIMING}
	Time.Init;
	{$ENDIF}

	with Shortcuts do
	begin
		EditorKeys := SetContext('Editor');

		Bind(keyNoteC_lo, 'Note.C-lo', 'Z');	Bind(keyNoteCClo, 'Note.C#lo', 'S');
		Bind(keyNoteD_lo, 'Note.D-lo', 'X');	Bind(keyNoteDDlo, 'Note.D#lo', 'D');
		Bind(keyNoteE_lo, 'Note.E-lo', 'C');	Bind(keyNoteF_lo, 'Note.F-lo', 'V');
		Bind(keyNoteFFlo, 'Note.F#lo', 'G');	Bind(keyNoteG_lo, 'Note.G-lo', 'B');
		Bind(keyNoteGGlo, 'Note.G#lo', 'H');	Bind(keyNoteA_lo, 'Note.A-lo', 'N');
		Bind(keyNoteAAlo, 'Note.A#lo', 'J');	Bind(keyNoteB_lo, 'Note.B-lo', 'M');
		Bind(keyNoteC_hi, 'Note.C-hi', 'Q');	Bind(keyNoteCChi, 'Note.C#hi', '2');
		Bind(keyNoteD_hi, 'Note.D-hi', 'W');	Bind(keyNoteDDhi, 'Note.D#hi', '3');
		Bind(keyNoteE_hi, 'Note.E-hi', 'E');	Bind(keyNoteF_hi, 'Note.F-hi', 'R');
		Bind(keyNoteFFhi, 'Note.F#hi', '5');	Bind(keyNoteG_hi, 'Note.G-hi', 'T');
		Bind(keyNoteGGhi, 'Note.G#hi', '6');	Bind(keyNoteA_hi, 'Note.A-hi', 'Y');
		Bind(keyNoteAAhi, 'Note.A#hi', '7');	Bind(keyNoteB_hi, 'Note.B-hi', 'U');

		//Bind(keyNoteModifierPreview,	'Note.Modifier.Preview','Shift', True);
		Bind(keyToggleEditMask,			'Toggle.EditMask',			',');
		Bind(keyNoteClear,				'Note.Clear', 				'.');
		Bind(keyNoteDelete,				'Note.Delete', 				'Delete');
		Bind(keyNoteInsert,				'Note.Insert', 				'Insert');
		Bind(keyNoteDeleteRow,			'Note.DeleteRow', 			'Alt+Delete');
		Bind(keyNoteInsertRow,			'Note.InsertRow', 			'Alt+Insert');
		Bind(keyNoteUseLast,			'Note.UseLast', 			'Space');
		Bind(keyNoteGetLast,			'Note.GetLast', 			'Return');
		Bind(keyNotePlay,				'Note.Play', 				'4');
		Bind(keyNotePlayRow,			'Note.PlayRow',				'8');
		Bind(keyMoveUp,					'Move.Up',  				'Up');
		Bind(keyMoveDown,				'Move.Down',				'Down');
		Bind(keyMoveLeft,				'Move.Left', 				'Left');
		Bind(keyMoveRight,				'Move.Right',				'Right');
		Bind(keyMoveUpAlt,				'Move.Up.Alt',				'Alt+Up');
		Bind(keyMoveDownAlt,			'Move.Down.Alt',			'Alt+Down');
		Bind(keyMoveLeftAlt,			'Move.Left.Alt',			'Alt+Left');
		Bind(keyMoveRightAlt,			'Move.Right.Alt',			'Alt+Right');
		Bind(keyMovePgUp,				'Move.PgUp', 				'PageUp');
		Bind(keyMovePgDn,				'Move.PgDn', 				'PageDown');
		Bind(keyMoveTop,				'Move.Top', 				'Ctrl+PageUp');
		Bind(keyMoveBottom,				'Move.Bottom', 				'Ctrl+PageDown');
		Bind(keyMoveHome,				'Move.Home', 				'Home');
		Bind(keyMoveEnd,				'Move.End', 				'End');
		Bind(keyMoveForwards,			'Move.Forwards',			'Tab');
		Bind(keyMoveBackwards,			'Move.Backwards',			'Shift+Tab');
		//Bind(keySelectModifierSkipValue,'Select.Modifier.SkipValue','Alt', True);
		Bind(keySelectPatternNext,		'Select.Pattern.Next', 		'KeyPad +');
		Bind(keySelectPatternPrev,		'Select.Pattern.Prev', 		'KeyPad -');
		Bind(keySelectOctaveLo,			'Select.Octave.Lo', 		'Keypad /');
		Bind(keySelectOctaveHi,			'Select.Octave.Hi', 		'Keypad *');
		Bind(keySelectSampleNext,		'Select.Sample.Next', 		['Ctrl+Down', '>']);
		Bind(keySelectSamplePrev,		'Select.Sample.Prev', 		['Ctrl+Up', '<']);
		Bind(keyBlockMarkStart,			'Block.MarkStart',	 		'Alt+B');
		Bind(keyBlockMarkEnd,			'Block.MarkEnd', 			'Alt+E');
		Bind(keyBlockMarkWhole,			'Block.MarkWhole', 			'Alt+L');
		Bind(keyBlockUnmark,			'Block.Unmark', 			'Alt+U');
		//Bind(keyBlockModifier,		'Block.Modifier', 			'Shift', True);
		Bind(keyBlockCut,				'Block.Cut', 				'Alt+Z');
		Bind(keyBlockCutMasked,			'Block.CutMasked', 			'Alt+Shift+Z');
		Bind(keyBlockCopy,				'Block.Copy', 				'Alt+C');
		Bind(keyBlockPaste,				'Block.Paste', 				'Alt+P');
		Bind(keyBlockOverwrite,			'Block.Overwrite', 			'Alt+O');
		Bind(keyBlockMix,				'Block.Mix', 				'Alt+M');
		Bind(keyBlockPasteMasked,		'Block.PasteMasked',		'Alt+Shift+P');
		Bind(keyBlockOverwriteMasked,	'Block.OverwriteMasked',	'Alt+Shift+O');
		Bind(keyBlockMixMasked,		 	'Block.MixMasked', 			'Alt+Shift+M');
		Bind(keyBlockDouble,			'Block.Double', 			'Alt+F');
		Bind(keyBlockHalve,				'Block.Halve', 				'Alt+G');
		Bind(keyBlockSetSample,			'Block.SetSample',			'Alt+S');
		Bind(keyBlockSwap,				'Block.Swap',				'Alt+Y');
		Bind(keyBlockSlideWipeEffect,	'Block.SlideOrWipeEffects',	'Alt+X');
		Bind(keyTransposeSemitoneUp,	'Transpose.Semitone.Up',	'Alt+Q');
		Bind(keyTransposeSemitoneDown,	'Transpose.Semitone.Down',	'Alt+A');
		Bind(keyTransposeOctaveUp,		'Transpose.Octave.Up', 	 	'Shift+Alt+Q');
		Bind(keyTransposeOctaveDown,	'Transpose.Octave.Down', 	'Shift+Alt+A');
		Bind(keyChannelToggle,			'Channel.Toggle', 			'Alt+F9');
		Bind(keyChannelSolo,			'Channel.Solo', 			'Alt+F10');
	end;

	ColorBack := 0;
	ColorFore := 6;

	// Setup palette
	//
	SetLength(Data, 16);
{	SetLength(Data, IDX_COL_ROW_CURRENTOTHER+1);
	SetData(IDX_COL_HL_ROW, 14, 'Highlight row');
	SetData(IDX_COL_HL_BEAT, 15, 'Highlight beat');
	SetData(IDX_COL_SEL_BG, 8, 'Selected background');
	SetData(IDX_COL_SEL_FG, 3, 'Selected foreground');
	SetData(IDX_COL_HL_SEL_ROW, 9, 'Selected highlight row');
	SetData(IDX_COL_HL_SEL_BEAT, 9, 'Selected highlight beat');
	SetData(IDX_COL_INVALID, 13, 'Invalid notes');
	SetData(IDX_COL_CUR_FG, 0, 'Cursor foreground');
	SetData(IDX_COL_CUR_BG, 3, 'Cursor background');
	SetData(IDX_COL_ROW, 0, 'Row number');
	SetData(IDX_COL_ROW_CURRENT, 3, 'Current row number, playing');
	SetData(IDX_COL_ROW_CURRENTOTHER, 6, 'Current row number');}
end;

procedure TPatternEditor.SetModified(B: Boolean = True; Force: Boolean = False);
var
	S: String;
begin
	if B then
	begin
		if (Module.Info.PatternCount <= CurrentPattern) and
			(not Module.IsPatternEmpty(CurrentPattern)) then
		begin
			Module.Info.PatternCount := CurrentPattern;
			Editor.UpdateInfoLabels(True);
		end;
	end;

	if (not Force) and (B = Module.Modified) then Exit;

	S := Module.Info.Title;
	if S = '' then
		S := '(Untitled)';
	S := 'Propulse' + ' - ' + S;
	if B then S := S + ' (*)';

	if Window <> nil then
		Window.SetTitle(S);
end;

procedure TPatternEditor.SaveModule(const Filename: String = '');
var
	Dir, Fn: String;
begin
	if Filename <> '' then
		Fn := Filename
	else
	begin
		Fn := Editor.lblFilename.Caption;
		Dir := ExtractFilePath(Module.Info.Filename);

		if (Fn = '') or (Dir = '') then
		begin
			FileRequester.Show(True, Options.Dirs.Modules);
			Exit;
		end
		else
			Fn := IncludeTrailingPathDelimiter(Dir) + Fn;
	end;

	if LowerCase(ExtractFileExt(Fn)) = '.wav' then
	begin
		if ModalDialog.Dialog = nil then
			Dialog_Render(False, Fn);
	end
	else
	begin
		Module.SaveToFile(Fn);
		Module.SetModified(False, True);
		Editor.lblFilename.SetCaption(ExtractFilename(Fn));
		FileRequester.SetDirectory(ExtractFilePath(Fn));
	end;

	ChangeScreen(TCWEScreen(LogScreen));
end;

function TPatternEditor.IsValidPosition(Pattern, Channel, Row: Byte): Boolean;
begin
	Result :=
		InRange(Pattern, 0, MAX_PATTERNS{-1}) and
		InRange(Channel, 0, AMOUNT_CHANNELS-1) and
		InRange(Row,     0, 63);
end;

function TPatternEditor.GetNote(Pattern, Channel, Row: Byte): PNote;
begin
	if IsValidPosition(Pattern, Channel, Row) then
		Result := @Module.Notes[Pattern, Channel, Row]
	else
		Result := nil;
end;

function TPatternEditor.SetNote(Pattern, Channel, Row: Byte; Note: TNote;
	Masked: Boolean = False): Boolean;
var
	DestNote: PNote;
begin
	Result := IsValidPosition(Pattern, Channel, Row);
	if Result then
	begin
		if not Masked then
			Module.Notes[Pattern, Channel, Row] := Note
		else
		begin
			DestNote := @Module.Notes[Pattern, Channel, Row];

			case Cursor.Column of

				COL_NOTE, COL_OCTAVE:
				begin
					DestNote.Period := Note.Period;
					DestNote.Text   := Note.Text;
				end;

				COL_SAMPLE_1, COL_SAMPLE_2:
					DestNote.Sample := Note.Sample;

				COL_VOLUME_1, COL_VOLUME_2,
				COL_COMMAND,
				COL_PARAMETER_1, COL_PARAMETER_2:
				begin
					DestNote.Command   := Note.Command;
					DestNote.Parameter := Note.Parameter;
					DestNote.CmdText   := Note.CmdText;
				end;

			end;

			DestNote.ParseText;
			DestNote.GetText;
		end;

		if not Locked then
			Module.SetModified;
	end;
end;

procedure TPatternEditor.InsertNote(Pattern, Channel, Row: Byte; WholePattern: Boolean = False);
var
	Y, ch1, ch2: Integer;
begin
	if WholePattern then
	begin
		ch1 := 0;
		ch2 := AMOUNT_CHANNELS-1;
	end
	else
	begin
		ch1 := Channel;
		ch2 := Channel;
	end;

	for Channel := ch1 to ch2 do
	begin
		for Y := 63 downto Max(Row, 1) do
			Module.Notes[Pattern, Channel, Y] :=
				Module.Notes[Pattern, Channel, Y-1];
		SetNote(Pattern, Channel, Row, EmptyNote);
	end;

	if not Locked then
		Module.SetModified;
end;

procedure TPatternEditor.DeleteNote(Pattern, Channel, Row: Byte; WholePattern: Boolean = False);
var
	Y, ch1, ch2: Integer;
begin
	if WholePattern then
	begin
		ch1 := 0;
		ch2 := AMOUNT_CHANNELS-1;
	end
	else
	begin
		ch1 := Channel;
		ch2 := Channel;
	end;

	for Channel := ch1 to ch2 do
	begin
		for Y := Row to 62 do
			Module.Notes[Pattern, Channel, Y] :=
				Module.Notes[Pattern, Channel, Y+1];
		Module.Notes[Pattern, Channel, 63] := EmptyNote;
	end;

	if not Locked then
		Module.SetModified;
end;

procedure TPatternEditor.NoteTranspose(Pattern, Channel, Row: Byte; Semitones: ShortInt);
var
	Note: PNote;
	np: Integer;
begin
	Note := @Module.Notes[Pattern, Channel, Row];
	if Semitones > 0 then
		np := 1
	else
		np := -1;

	case Cursor.Column of

		COL_NOTE, COL_OCTAVE:
		begin
			np := GetPeriodTableOffset(Note.Period);
			if np >= 0 then
			begin
				np := np + Semitones;
				if InRange(np, 0, 36) then
				begin
					Note.Period := PeriodTable[np];
					Note.Text   := GetNoteText(Note.Period);
				end;
			end;
		end;

		COL_SAMPLE_1, COL_SAMPLE_2:
		begin
			if Note.Sample > 0 then
				Note.Sample := Min(Max(Note.Sample + np, 1), 31);
		end;

		COL_VOLUME_1, COL_VOLUME_2:
		begin
			if Note.Command = $C then
			begin
				if np > 0 then
					Note.Parameter := Min(Note.Parameter + 1, 64)
				else
				if Note.Parameter > 0 then
					Note.Parameter := Note.Parameter - 1;
			end;
		end;

		COL_COMMAND,
		COL_PARAMETER_1, COL_PARAMETER_2:
		begin
			if ((Note.Parameter <> 0) or (Note.Command <> 0)) and
				(Note.Command <> $C) then
					Note.Parameter := Note.Parameter + np;
		end;

	end;
end;

procedure TPatternEditor.Transpose(Semitones: ShortInt);
var
	x, y: Integer;
begin
	if (Selection.Right >= 0) and (Selection.Bottom >= 0) then
	begin
		for x := Max(Selection.Left, 0) to Selection.Right do
		for y := Max(Selection.Top, 0)  to Selection.Bottom do
			NoteTranspose(CurrentPattern, x, y, Semitones);
	end
	else
		NoteTranspose(CurrentPattern, Cursor.Channel, Cursor.Row, Semitones);
	Module.SetModified;
end;

procedure TPatternEditor.BufferClear(R: TRect; Masked: Boolean = False);
var
	x, y: Integer;
begin
	Locked := True;
	for x := R.Left to R.Right do
		for y := R.Top to R.Bottom do
			SetNote(CurrentPattern, x, y, EmptyNote, Masked);
	Locked := False;
	Module.SetModified;
end;

procedure TPatternEditor.BufferCopy(R: TRect);
var
	x, y: Integer;
begin
	MessageText('Selection copied to memory.');
	for x := R.Left to R.Right do
		for y := R.Top to R.Bottom do
			ClipBuf.Notes[x-R.Left, y-R.Top] := Module.Notes[CurrentPattern, x, y];
	ClipBuf.Size.X := R.Right  - R.Left;
	ClipBuf.Size.Y := R.Bottom - R.Top;
	//Clipbuf.ToClipboard;
end;

procedure TPatternEditor.BufferPaste(Mix: PasteMode; Masked: Boolean = False);
var
	x, y: Integer;
	Note: PNote;
begin
	Locked := True;

	case Mix of

		pstInsert:
			for x := 0 to ClipBuf.Size.X do
			for y := 0 to ClipBuf.Size.Y do
			begin
				InsertNote(CurrentPattern, Cursor.Channel + x, Cursor.Row + y);
				SetNote(CurrentPattern, Cursor.Channel + x, Cursor.Row + y,
					ClipBuf.Notes[x, y], Masked);
			end;

		pstOverwrite:
			for x := 0 to ClipBuf.Size.X do
			for y := 0 to ClipBuf.Size.Y do
				SetNote(CurrentPattern, Cursor.Channel + x, Cursor.Row + y,
					ClipBuf.Notes[x, y], Masked);

		pstMix:
			for x := 0 to ClipBuf.Size.X do
			for y := 0 to ClipBuf.Size.Y do
			begin
				Note := GetNote(CurrentPattern, Cursor.Channel + x, Cursor.Row + y);
				if (Note <> nil) and (Note.Period = EmptyNote.Period) then
					SetNote(CurrentPattern, Cursor.Channel + x, Cursor.Row + y,
						ClipBuf.Notes[x, y], Masked);
			end;

	else
		Locked := False;
		Exit;
	end;

	Locked := False;
	Module.SetModified;
end;

// Copies current selection to the last pattern in module
// Returns number of rows in prepared pattern
//
function TPatternEditor.PrepareSelectionForRender: Byte;
const
	DestPattern = MAX_PATTERNS;
var
	x, y: Integer;
	R: TRect;
begin
	if (Selection.Left < 0) or (Selection.Top < 0) then
		Selection := Types.Rect(0, 0, AMOUNT_CHANNELS-1, 63);

	for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
			SetNote(DestPattern, x, y, EmptyNote);

	R := Selection;
	Result := Min(R.Bottom - R.Top + 1, 63);

	for x := R.Left to R.Right do
	for y := R.Top to R.Bottom do
		SetNote(DestPattern, x, y - R.Top, Module.Notes[CurrentPattern, x, y]);
end;

procedure TPatternEditor.BlockSwap;
var
	p, sx, sy, dx, dy: Integer;
	TempNote: TNote;
	dummyRect, Src, Dest: TRect;
begin
	if (Selection.Left < 0) or (Selection.Top < 0) then
	begin
		Editor.MessageText('No selection!');
		Exit;
	end;

	Src := Selection;
	Inc(Src.Right);
	Inc(Src.Bottom);
	Dest := Types.Bounds(Cursor.Channel, Cursor.Row,
		RectWidth(Src), RectHeight(Src));

	// does Dest overlap with Selection?
	if IntersectRect(dummyRect, Dest, Src) then
	begin
		Editor.MessageText('Swap blocks overlap!');
		Exit;
	end;

	// is Dest out of pattern bounds?
	Dec(Dest.Right);
	Dec(Dest.Bottom);
	if (Dest.Bottom >= 64) or (Dest.Right >= AMOUNT_CHANNELS) then
	begin
		Editor.MessageText('Destination out of pattern bounds!');
		Exit;
	end;

	p := CurrentPattern;
	dy := Dest.Top;

	for sy := Selection.Top to Selection.Bottom do
	begin
		dx := Dest.Left;
		for sx := Selection.Left to Selection.Right do
		begin
			TempNote := Module.Notes[p, sx, sy];
			Module.Notes[p, sx, sy] := Module.Notes[p, dx, dy];
			Module.Notes[p, dx, dy] := TempNote;
			Inc(dx);
		end;
		Inc(dy);
	end;

	{Editor.MessageText(format('sel=%d,%d-%d,%d  dest=%d,%d-%d,%d', [
		Src.Left, Src.Top, Src.Right, Src.Bottom,
		Dest.Left, Dest.Top, Dest.Right, Dest.Bottom]));}
	Module.SetModified;
end;

procedure TPatternEditor.BlockDoubleOrHalve(DoubleIt: Boolean);
var
	p, x, y, yy, t: Integer;
begin
	p := CurrentPattern;
	t := Selection.Top;

	for x := Selection.Left to Selection.Right do
	begin
		if DoubleIt then
			for y := Selection.Bottom-t downto 0 do // double
			begin
				yy := y * 2 + t;
				if (yy < 64) and (y+t < 64) then
					Module.Notes[p, x, yy] := Module.Notes[p, x, y+t];
				if yy < 63 then
					Module.Notes[p, x, yy+1] := EmptyNote;
			end
		else
			for y := 0 to Selection.Bottom-t do // halve
				if y mod 2 = 0 then
					Module.Notes[p, x, (y div 2) + t] := Module.Notes[p, x, y+t];
	end;

	Module.SetModified;
end;

procedure TPatternEditor.BlockSlideEffect;
var
	p, x, y, h, t, FX1, FX2: Integer;
	step: Single;
begin
	p := CurrentPattern;
	t := Selection.Top;
	h := Selection.Bottom - t;
	if h <= 0 then Exit;

	for x := Selection.Left to Selection.Right do
	begin
		FX1 := Module.Notes[p, x, t].Parameter;
		FX2 := Module.Notes[p, x, Selection.Bottom].Parameter - FX1;
		if FX2 = 0 then
			step := 0
		else
			step := FX2 / h;
		for y := 0 to h do
			Module.Notes[p, x, y+t].Parameter := Round(FX1 + (step * y));
	end;

	Module.SetModified;
end;

procedure TPatternEditor.BlockWipeEffects;
var
	x, y: Integer;
begin
	for x := Selection.Left to Selection.Right do
		for y := Selection.Top to Selection.Bottom do
		begin
			Module.Notes[CurrentPattern, x, y].Command   := 0;
			Module.Notes[CurrentPattern, x, y].Parameter := 0;
		end;
	Module.SetModified;
end;

procedure TPatternEditor.BlockSetSample;
var
	x, y: Integer;
begin
	for x := Selection.Left to Selection.Right do
		for y := Selection.Top to Selection.Bottom do
		begin
			if Module.Notes[CurrentPattern, x, y].Sample <> 0 then
				Module.Notes[CurrentPattern, x, y].Sample := CurrentSample;
		end;
	Module.SetModified;
end;

procedure TPatternEditor.ReplaceSample(iFrom, iTo: Byte);
var
	p, x, y: Integer;
begin
	if (iFrom = iTo) then Exit;
	for p := 0 to Module.Info.PatternCount do
	for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
		begin
			if Module.Notes[p, x, y].Sample = iFrom then
				Module.Notes[p, x, y].Sample := iTo;
		end;
	Module.SetModified;
end;

procedure TPatternEditor.Advance;
begin
	if (Cursor.Row + NoteStep) > 63 then Exit;
	Inc(Cursor.Row, NoteStep);
end;

// validate cursor coords
procedure TPatternEditor.ValidateCursor;
begin
	if Cursor.Row > 63 then
		Cursor.Row := 63;

	if Cursor.Row < ScrollPos then
		ScrollPos := Cursor.Row
	else
	if Cursor.Row >= (ScrollPos + 31) then
		ScrollPos := Cursor.Row - 31;

	Cursor.X := EditColumnX[Cursor.Column] + (Cursor.Channel * PATTERN_CHAN_WIDTH);
	Cursor.Note := @Module.Notes[CurrentPattern, Cursor.Channel, Cursor.Row];
end;

procedure TPatternEditor.MessageText(const S: String);
begin
	Editor.MessageText(S);
end;

procedure TPatternEditor.SelectChannel(Ch: Integer);
begin
	if Ch < 0 then
		Ch := 0
	else
		Ch := Min(Ch, AMOUNT_CHANNELS-1);
	Cursor.Channel := Ch;
	Editor.UpdateInfoLabels(True);
end;

{procedure TPatternEditor.DrawEditMaskIndicators;
begin
end;}

// todo make this less unwieldy
//
function TPatternEditor.KeyDown;
var
	i, n, o: Integer;
	prevColumn: EditColumn;
	r: TRect;
	Sht: TShiftState;
	Sc, Scs: EditorKeyNames;
	AllowEditing: Boolean;
label
	Done;
begin
	if Key = 0 then Exit(False);

	AllowEditing := not FollowPlayback; //(Module.PlayMode <> PLAY_SONG);
	Result := False;
	prevColumn := Cursor.Column;

	// Continue playback from current position
	//
	if Key = SDLK_F7 then //keyPlaybackPlayFrom
	begin
		o := -1;
		for i := 0 to Module.Info.OrderCount-1 do
			if Module.OrderList[i] = CurrentPattern then
			begin
				o := i;
				Break;
			end;
		if o >= 0 then // continue from orderlist
			Module.Play(o, Cursor.Row)
		else
			Module.PlayPattern(CurrentPattern, Cursor.Row);
		Exit(True);
	end;

	// Set cursor step
	//
	//keySelectModifierSkipValue
	if ssAlt in Shift then
	begin
		o := Pos(Chr(Key), KeyboardNumbers) - 1;
		if o in [0..9] then
		begin
			if o = 9 then o := 16;
			NoteStep := o;
			MessageText('Cursor step set to ' + IntToStr(o) + '.');
			Result := True;
			goto Done;
		end;
	end;

	Sc := EditorKeyNames(Shortcuts.Find(EditorKeys, Key, Shift));
	//window.Caption := format('Key=%d  Sc=%d', [Key, Integer(sc)]);

	// Some keys are handled differently when playing song
	//
	if FollowPlayback {Module.PlayMode = PLAY_SONG} then
	case Sc of

		keyNoteUseLast: // space
			Sc := keyChannelToggle;

		keyMoveBackwards,
		keyMoveLeft,
		keyMoveUp:
		begin
			SelectChannel(Cursor.Channel - 1);
			Exit(True);
		end;

		keyMoveForwards,
		keyMoveRight,
		keyMoveDown:
		begin
			SelectChannel(Cursor.Channel + 1);
			Exit(True);
		end;

	end;

	// Setup block marking
	//
	if (not AllowEditing) or (Sc <> keyNONE) or (Shift <> [ssShift]) then
		Marking := False
	else
	begin
		if not Marking then
		begin
			MarkPos := Point(Cursor.Channel, Cursor.Row);
			Marking := True;
		end;

		if Marking then
		begin
			Sht := Shift;
			Exclude(Sht, ssShift);
			Scs := EditorKeyNames(Shortcuts.Find(EditorKeys, Key, Sht));

			if Scs in [keyMoveUp..keyMoveEnd] then
			begin
				Sc := Scs;
				Shift := Sht;
			end;
		end;
	end;

	// Insert/play notes into pattern
	//
	if Cursor.Column = COL_NOTE then
	case Sc of

		keyNotePlay:	// 4
		begin
			if Cursor.Note.Period > 0 then
				Module.PlayNote(Cursor.Note, Cursor.Channel);
				{Module.PlaySample(GetPeriodTableOffset(Cursor.Note.Period),
					Cursor.Note.Sample, Cursor.Channel);}
			Result := True;
		end;

		keyNotePlayRow:	// 8
		begin
			for n := 0 to AMOUNT_CHANNELS-1 do
			begin
				Module.Channel[n].Paula.Kill;
				Module.PlayNote(@Module.Notes[CurrentPattern, n, Cursor.Row], n);
				{with Module.Notes[CurrentPattern, n, Cursor.Row] do
					if Period > 0 then
						Module.PlaySample(GetPeriodTableOffset(Period), Sample, n);}
			end;
			Result := True;
		end;

		keyNoteC_lo..keyNoteB_hi:
		begin
			Marking := False;
			// insert data into pattern
			n := Integer(Sc) - Integer(keyNoteC_lo) + (Integer(HighOctave) * 12);
			if n < 36 then // go no higher than B-3
			begin
				// only insert note if no caps lock pressed
				if (AllowEditing) and (not ModKeys(ssShift)) then
				begin
					with Cursor.Note^ do
					begin
						Period := PeriodTable[n];
						Text   := GetNoteText(Period);
						if EditMask[EM_SAMPLE] then
							Sample := CurrentSample;

						if EditMask[EM_VOLUME] then
						begin
							if LastNote.Command = $C then
							begin
								Command   := LastNote.Command;
								Parameter := LastNote.Parameter;
							end
							else
							begin
								Command   := $0;
								Parameter := $0;
							end;
						end;
						if EditMask[EM_EFFECT] then
						begin
							Command   := LastNote.Command;
							Parameter := LastNote.Parameter;
						end;

						LastNote.Period := Period;
						LastNote.Text   := Text;
						LastNote.Sample := Sample;
					end;

					Module.PlayNote(@LastNote, Cursor.Channel);
					Advance;
					Module.SetModified;
				end;
				Result := True;
			end;
		end;

	end;

	if Result then
		goto Done;

	// Handle cursor movement, editing etc.
	//
	case Sc of

		keyMoveUp:
		begin
			Result := True;
			Cursor.Row := Max(0, Cursor.Row - NoteStep);
		end;

		keyMoveDown:
		begin
			Result := True;
			Advance;
		end;

		keyMoveLeft:
		begin
			Result := True;
			if Marking then
				SelectChannel(Cursor.Channel - 1)
			else
			if (Cursor.Column > COL_NOTE) or (Cursor.Channel > 0) then
			begin
				if (Cursor.Column = COL_NOTE) then
				begin
					SelectChannel(Cursor.Channel - 1);
					Cursor.Column := COL_PARAMETER_2;
				end
				else
					Dec(Cursor.Column);
			end;
		end;

		keyMoveRight:
		begin
			Result := True;
			if Marking then
				SelectChannel(Cursor.Channel + 1)
			else
			begin
				Inc(Cursor.Column);

				if (Cursor.Column >= COL_NEXTCHANNEL) then
				begin
					if Cursor.Channel < AMOUNT_CHANNELS-1 then
					begin
						SelectChannel(Cursor.Channel + 1);
						Cursor.Column := COL_NOTE;
					end
					else
						Cursor.Column := COL_PARAMETER_2;
				end;
			end;
		end;

		keyMovePgUp:
		begin
			Result := True;
			if AllowEditing then
			begin
				if Cursor.Row >= 63 then
					Cursor.Row := 48
				else
					Cursor.Row := Max(Cursor.Row - 16, 0);
			end;
		end;

		keyMovePgDn:
		begin
			Result := True;
			if AllowEditing then
				Cursor.Row := Min(Cursor.Row + 16, 63);
		end;

		keyMoveUpAlt:
		if AllowEditing then
		begin
			ScrollPos := Max(ScrollPos-1, 0);
			Cursor.Row := Min(ScrollPos+31, Cursor.Row);
			Result := True;
		end;

		keyMoveDownAlt:
		if AllowEditing then
		begin
			Inc(ScrollPos);
			Cursor.Row := Max(ScrollPos, Cursor.Row);
			Result := True;
		end;

		keyMoveLeftAlt:
		if AllowEditing then
		begin
			SelectChannel(Cursor.Channel - 1);
			Result := True;
		end;

		keyMoveRightAlt:
		if AllowEditing then
		begin
			SelectChannel(Cursor.Channel + 1);
			Result := True;
		end;

		keyMoveTop:
		begin
			if AllowEditing then
				Cursor.Row := 0;
			Result := True;
		end;

		keyMoveBottom:
		begin
			if AllowEditing then
				Cursor.Row := 63;
			Result := True;
		end;

		keyMoveHome:
		begin
			Result := True;
			if AllowEditing then
			begin
				if Options.Tracker.AltHomeEndBehavior then
				begin
					// Propulse behavior
					if Cursor.Row > ScrollPos then
						Cursor.Row := ScrollPos
					else
					begin
						if Cursor.Row > 0 then
							Cursor.Row := 0
						else
						begin
							SelectChannel(0);
							Cursor.Column := COL_NOTE;
						end;
					end;
				end
				else
				begin
					// Impulse Tracker behavior
					if Cursor.Column = COL_NOTE then
					begin
						if Cursor.Channel > 0 then
							SelectChannel(0)
						else
							Cursor.Row := 0;
					end
					else
						Cursor.Column := COL_NOTE;
				end;
			end;
		end;

		keyMoveEnd:
		if AllowEditing then
		begin
			Result := True;
			if Options.Tracker.AltHomeEndBehavior then
			begin
				// PoroTracker behavior
				if Cursor.Row < (ScrollPos + 31) then
					Cursor.Row := (ScrollPos + 31)
				else
					Cursor.Row := 63;
			end
			else
			begin
				// Impulse Tracker behavior
				if Cursor.Column < COL_PARAMETER_2 then
					Cursor.Column := COL_PARAMETER_2
				else
				begin
					if Cursor.Channel >= AMOUNT_CHANNELS-1 then
						Cursor.Row := 63
					else
						SelectChannel(AMOUNT_CHANNELS-1);
				end;
			end;
		end;

		keyMoveBackwards:
		begin
			// jump backwards
			if (Cursor.Column > COL_NOTE) then
				Cursor.Column := COL_NOTE
			else
			SelectChannel(Cursor.Channel - 1);
			Result := True;
		end;

		keyMoveForwards:
		begin
			// jump to next channel
			SelectChannel(Cursor.Channel + 1);
			Cursor.Column := COL_NOTE;
			Result := True;
		end;

		// Insert/Delete

		keyNoteInsert:
		begin
			Result := True;
			if AllowEditing then
				InsertNote(CurrentPattern, Cursor.Channel, Cursor.Row, False);
		end;

		keyNoteDelete:
		begin
			Result := True;
			if AllowEditing then
				DeleteNote(CurrentPattern, Cursor.Channel, Cursor.Row, False);
		end;

		keyNoteInsertRow:
		if AllowEditing then
		begin
			InsertNote(CurrentPattern, Cursor.Channel, Cursor.Row, True);
			Result := True;
		end;

		keyNoteDeleteRow:
		if AllowEditing then
		begin
			DeleteNote(CurrentPattern, Cursor.Channel, Cursor.Row, True);
			Result := True;
		end;

		keyNoteClear:		//190: // period; clear note
		if AllowEditing then
		begin
			case Cursor.Column of

				COL_NOTE, COL_OCTAVE:
				begin
					Cursor.Note.Period := 0;
					Cursor.Note.Sample := 0; // ???
					Cursor.Note.Text := GetNoteText(Cursor.Note.Period);
				end;

				COL_SAMPLE_1, COL_SAMPLE_2:
					Cursor.Note.Sample := 0;

				COL_VOLUME_1, COL_VOLUME_2:
				begin
					Cursor.Note.Command := 0;
					Cursor.Note.Parameter := 0;
				end;

				COL_COMMAND:
				begin
					Cursor.Note.Command := 0;
					if Options.Tracker.ITCommands then
						Cursor.Note.Parameter := 0; // don't want no J command!
				end;

				COL_PARAMETER_1, COL_PARAMETER_2:
					Cursor.Note.Parameter := 0;
			end;

			Module.SetModified;
			Advance;
			Result := True;
		end;

		keyNoteGetLast:	// enter
		begin
			Result := True;
			with GetNote(CurrentPattern, Cursor.Channel, Cursor.Row)^ do
			begin
				LastNote.Sample := Sample;
				LastNote.Period := Period;
				LastNote.Command   := Command;
				LastNote.Parameter := Parameter;
				LastNote.Text := Text;
				LastNote.CmdText := CmdText;
				Editor.SetSample(Sample);
			end;
		end;

		keyNoteUseLast:		// Space
		begin
			case Cursor.Column of

				COL_NOTE:
					with Cursor.Note^ do
					begin
						Period := LastNote.Period;

						if EditMask[EM_SAMPLE] then
							Sample := LastNote.Sample;
						if EditMask[EM_VOLUME] then
						begin
							if LastNote.Command = $C then
							begin
								Command   := LastNote.Command;
								Parameter := LastNote.Parameter;
							end
							else
							begin
								Command   := $0;
								Parameter := $0;
							end;
						end;
						if EditMask[EM_EFFECT] then
						begin
							Command   := LastNote.Command;
							Parameter := LastNote.Parameter;
							CmdText   := LastNote.CmdText;
							ParseText;
						end;

						Text := LastNote.Text;
						Module.PlayNote(Cursor.Note, Cursor.Channel);
					end;

				COL_SAMPLE_1, COL_SAMPLE_2:
					Cursor.Note.Sample := LastNote.Sample;

				COL_VOLUME_1, COL_VOLUME_2:
					with Cursor.Note^ do
					begin
						Command   := $C; // ???
						Parameter := Min(64, LastNote.Parameter);
					end;

				COL_COMMAND:
				begin
					if Cursor.Note.Command = $C then
						Cursor.Note.Parameter := Min(64, Cursor.Note.Parameter)
					else
					if Options.Tracker.ITCommands then
					begin
						Cursor.Note.Command := LastNote.Command;
						Cursor.Note.CmdText[1] := LastNote.CmdText[1];
						Cursor.Note.ParseText;
						Cursor.Note.GetText;
					end
					else
						Cursor.Note.Command := LastNote.Command;
				end;

				COL_PARAMETER_1, COL_PARAMETER_2:
				begin
					if Options.Tracker.ITCommands then
					begin
						{if Cursor.Note.Command = 0 then
							Cursor.Note.CmdText[1] := 'J';
						Cursor.Note.CmdText :=
							Cursor.Note.CmdText[1] + Copy(LastNote.CmdText, 2, 2);
						Cursor.Note.ParseText;}
						if Cursor.Note.Command = 0 then
							Cursor.Note.Command := LastNote.Command;
						Cursor.Note.CmdText := LastNote.CmdText;

						Cursor.Note.ParseText;
					end
					else
						Cursor.Note.Parameter := LastNote.Parameter;
				end;
			end;

			Module.SetModified;
			Advance;
			Result := True;
		end;

		// Block

		keyBlockMarkWhole: // Mark entire column/pattern
		begin
			Result := True;
			r := Bounds(Cursor.Channel, 0, 0, 63);
			if (r.Left = Selection.Left) and (r.Right  = Selection.Right) and
				(r.Top = Selection.Top)  and (r.Bottom = Selection.Bottom) then
					Selection := Types.Rect(0, 0, AMOUNT_CHANNELS-1, r.Bottom)
			else
				Selection := r;
		end;

		keyBlockMarkStart:
		begin
			Result := True;
			Selection.Left   := Cursor.Channel;
			Selection.Top    := Cursor.Row;
			Selection.Right  := Max(Cursor.Channel, Selection.Right);
			Selection.Bottom := Max(Cursor.Row, Selection.Bottom);
		end;

		keyBlockMarkEnd:	// Mark end of block (fixme maybe)
		begin
			Result := True;
			i := Selection.Right;
			n := Cursor.Channel;
			Selection.Left   := Min(i, n);
			Selection.Right  := Max(i, n);
			i := Cursor.Row;
			n := Selection.Bottom;
			Selection.Bottom := Max(i, n);
			Selection.Top    := Min(i, n);
		end;

		keyBlockUnmark:		// Unmark block/Release clipboard memory
		begin
			Result := True;
			Selection := Types.Rect(-1, -1, -1, -1);
		end;

		keyBlockCopy:		// Copy block into clipboard
		begin
			Result := True;
			BufferCopy(Selection);
		end;

		keyBlockCut,		// Cut block
		keyBlockCutMasked:
		begin
			Result := True;
			if AllowEditing then
			begin
				BufferCopy(Selection);
				BufferClear(Selection, (Sc = keyBlockCutMasked));
			end;
		end;

		keyBlockPaste,		// Alt-P, Alt-V
		keyBlockPasteMasked:
		begin
			Result := True;
			if AllowEditing then
				BufferPaste(pstInsert, (Sc = keyBlockPasteMasked));
		end;

		keyBlockOverwrite,
		keyBlockOverwriteMasked:
		begin
			Result := True;
			if AllowEditing then
				BufferPaste(pstOverwrite, (Sc = keyBlockOverwriteMasked));
		end;

		keyBlockMix,		// Mix each row from clipboard with pattern data
		keyBlockMixMasked:
		begin
			Result := True;
			if AllowEditing then
				BufferPaste(pstMix, (Sc = keyBlockMixMasked));
		end;

		keyBlockDouble:
		begin
			Result := True;
			if AllowEditing then
				BlockDoubleOrHalve(True);
		end;

		keyBlockHalve:
		begin
			Result := True;
			if AllowEditing then
				BlockDoubleOrHalve(False);
		end;

		keyBlockSetSample:
		begin
			Result := True;
			if AllowEditing then
				BlockSetSample;
		end;

		keyBlockSwap:
		begin
			Result := True;
			if AllowEditing then
				BlockSwap;
		end;

		keyBlockSlideWipeEffect:
		begin
			Result := True;
{			if AllowEditing then
			begin
				if PreviousKeys.Counter > 1 then
					BlockWipeEffects
				else
					BlockSlideEffect;
			end; !!! }
		end;

		// Transpose

		keyTransposeSemitoneUp:
		begin
			Result := True;
			Transpose(+1);
		end;

		keyTransposeSemitoneDown:
		begin
			Result := True;
			Transpose(-1);
		end;

		keyTransposeOctaveUp:
		begin
			Result := True;
			Transpose(+12);
		end;

		keyTransposeOctaveDown:
		begin
			Result := True;
			Transpose(-12);
		end;

		// Channel muting

		keyChannelToggle:
		begin
			Result := True;
			Editor.ToggleChannel(Cursor.Channel);
		end;

		keyChannelSolo:
		begin
			Result := True;
			o := 0; // amount of enabled channels
			for i := 0 to AMOUNT_CHANNELS-1 do
				if Module.Channel[i].Enabled then
					Inc(o);
			if (o = 1) and (Module.Channel[Cursor.Channel].Enabled) then
			begin
				// enable all channels (unsolo)
				for i := 0 to AMOUNT_CHANNELS-1 do
					Module.Channel[i].Enabled := True;
			end
			else
			begin
				// mute all but current channel (solo)
				for i := 0 to AMOUNT_CHANNELS-1 do
					Module.Channel[i].Enabled := (i = Cursor.Channel);
			end;
			Editor.UpdateInfoLabels(True);
		end;

		// Misc

		keySelectSamplePrev:
		begin
			Editor.SetSample(CurrentSample - 1);
			Result := True;
		end;

		keySelectSampleNext:
		begin
			Editor.SetSample(CurrentSample + 1);
			Result := True;
		end;

		keyToggleEditMask:
		begin
			Result := True;
			if AllowEditing then
			begin
				o := -1;
				case Cursor.Column of

					COL_SAMPLE_1, COL_SAMPLE_2:
						o := EM_SAMPLE;

					COL_VOLUME_1, COL_VOLUME_2:
						o := EM_VOLUME;

					COL_COMMAND,
					COL_PARAMETER_1, COL_PARAMETER_2:
						o := EM_EFFECT;

				end;
				if o >= EM_SAMPLE then
				begin
					EditMask[o] := not EditMask[o];
					Editor.UpdateInfoLabels(True);
					//DrawEditMaskIndicators;
				end;
			end;
		end;

	else

		Marking := False;

		// insert data into pattern
		if AllowEditing then
		case Cursor.Column of

			COL_OCTAVE:
			begin
				o := Pos(Chr(Key), KeyboardNumbers) - 1;
				if o in [1..3] then
				begin
					Result := True;
					with Cursor.Note^ do
					if Period > 0 then
					begin
						n := GetPeriodTableOffset(Period) mod 12;
						if n >= 0 then
						begin
							Period := PeriodTable[n + ((o - 1) * 12)];
							Text   := GetNoteText(Period);
							LastNote.Period := Period;
							LastNote.Text := Text;
							Module.SetModified;
							Advance;
						end;
					end;
				end;
			end;

			COL_SAMPLE_1:  // sample number 1st digit
			begin
				o := Pos(Chr(Key), KeyboardNumbers) - 1;
				if o in [0..3] then
				begin
					Result := True;
					Cursor.Note.Sample := Min((o * 10) + (Cursor.Note.Sample mod 10), 31);
					LastNote.Sample := Cursor.Note.Sample;
					Editor.SetSample(Cursor.Note.Sample);
					Module.SetModified;
					Inc(Cursor.Column);
				end;
			end;

			COL_SAMPLE_2:  // sample number 2nd digit
			begin
				o := Pos(Chr(Key), KeyboardNumbers) - 1;
				if o >= 0 then
				begin
					Cursor.Note.Sample := Min((Cursor.Note.Sample div 10 * 10) + o, 31);
					LastNote.Sample := Cursor.Note.Sample;
					Editor.SetSample(Cursor.Note.Sample);
					Module.SetModified;
					Dec(Cursor.Column);
					Advance;
					Result := True;
				end;
			end;

			COL_VOLUME_1:	// volume 1st digit
			begin
				o := Pos(Chr(Key), KeyboardNumbers) - 1;
				if o in [0..6] then
				begin
					Cursor.Note.Command := $C;
					Cursor.Note.Parameter := Min( (o * 10) + (Cursor.Note.Parameter mod 10), 64);
					Cursor.Note.GetText;
					LastNote.Command := $C;
					LastNote.Parameter := Cursor.Note.Parameter;
					Module.SetModified;
					Inc(Cursor.Column);
					Result := True;
				end;
			end;

			COL_VOLUME_2:	// volume 2nd digit
			begin
				o := Pos(Chr(Key), KeyboardNumbers) - 1;
				if o >= 0 then
				begin
					Cursor.Note.Command := $C;
					Cursor.Note.Parameter := Min( (Cursor.Note.Parameter div 10 * 10) + o, 64);
					Cursor.Note.GetText;
					LastNote.Command := $C;
					LastNote.Parameter := Cursor.Note.Parameter;
					Module.SetModified;
					Dec(Cursor.Column);
					Advance;
					Result := True;
				end;
			end;

			COL_COMMAND:	// effect type
			begin
				o := Pos(Chr(Key), CmdChars) - 1;
				if o >= 0 then
				begin
					if Options.Tracker.ITCommands then
						i := Pos(Chr(Key), CmdCharsITExt) - 1
					else
						i := -1;

					if i >= 0 then
					begin
						Cursor.Note.CmdText[1] := CmdCharsITExt[i+1];
						Cursor.Note.ParseText;
					end
					else
						Cursor.Note.Command := o;

					if o = $C then
						Cursor.Note.Parameter := Min(64, Cursor.Note.Parameter);
					if i < 0 then
						Cursor.Note.GetText;

					LastNote.Command := Cursor.Note.Command;
					LastNote.CmdText := Cursor.Note.CmdText;
					LastNote.Text := Cursor.Note.Text;

					Module.SetModified;
					Advance;
					Result := True;
				end;
			end;

			COL_PARAMETER_1,	// effect 1st digit
			COL_PARAMETER_2:	// effect 2nd digit
			if Cursor.Note.Command <> $C then
			begin
				o := Pos(Chr(Key), KeyboardHexNumbers) - 1;
				if o >= 0 then
				begin
					if Cursor.Column = COL_PARAMETER_1 then
					begin
						if Options.Tracker.ITCommands then
						begin
							Cursor.Note.CmdText[2] := AnsiChar(KeyboardHexNumbers[o+1]);
							Cursor.Note.ParseText;
						end
						else
						begin
							Cursor.Note.Parameter := (o * $10) + (Cursor.Note.Parameter mod $10);
							Cursor.Note.GetText;
						end;
						Inc(Cursor.Column);
					end
					else
					begin
						if Options.Tracker.ITCommands then
						begin
							Cursor.Note.CmdText[3] := AnsiChar(KeyboardHexNumbers[o+1]);
							Cursor.Note.ParseText;
						end
						else
						begin
							Cursor.Note.Parameter := (Cursor.Note.Parameter div $10 * $10) + o;
							Cursor.Note.GetText;
						end;
						Cursor.Column := COL_PARAMETER_1;
						Advance;
					end;
					LastNote.Parameter := Cursor.Note.Parameter;
					LastNote.Text := Cursor.Note.Text;
					LastNote.CmdText := Cursor.Note.CmdText;
					Module.SetModified;
					Result := True;
				end;
			end;

		end; // case Column

	end;

	if not Result then Exit;

Done:

	if Marking then
	begin
		Selection.Left  := Min(Cursor.Channel, MarkPos.X);
		Selection.Right := Max(Cursor.Channel, MarkPos.X);
		Selection.Top   := Min(Cursor.Row, MarkPos.Y);
		Selection.Bottom:= Max(Cursor.Row, MarkPos.Y);
	end;

	if AllowEditing then
		ValidateCursor;

	if prevColumn <> Cursor.Column then
		Editor.UpdateInfoLabels(True);

	Paint;
end;

function TPatternEditor.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
const
	ScrollStep = 2;
begin
	if FollowPlayback then
		SelectChannel(Cursor.Channel - WheelDelta)
	else
	begin
		if WheelDelta > 0 then
			ScrollPos := Max(ScrollPos - Abs(WheelDelta*ScrollStep), 0)
		else
		if WheelDelta < 0 then
			ScrollPos := Min(ScrollPos + Abs(WheelDelta*ScrollStep), 63 - Height);
	end;
	Paint;
	Result := True;
end;

function TPatternEditor.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
var
	Chan: Byte;
begin
	if P.X < 3 then Exit(False);

	P.X := P.X - 3;
	Chan := P.X div (Width div 4);
	Result := True;

	case Button of

		mbLeft:
			if Chan in [0..AMOUNT_CHANNELS-1] then
			begin
				Cursor.Row := P.Y + ScrollPos;
				Cursor.Channel := Chan;
				ValidateCursor;
				Editor.UpdateInfoLabels(False);
			end;

		mbMiddle:
			if Chan in [0..AMOUNT_CHANNELS-1] then
			begin
				Module.Channel[Chan].Enabled := not
					Module.Channel[Chan].Enabled;
				Editor.UpdateInfoLabels(False);
			end;

		mbRight:
			Result := False; // unhandled

	end;
end;

procedure sPut(var S: AnsiString; Index: Byte; const D: AnsiString); inline;
var
	i: Integer;
begin
	for i := 1 to Min(Length(S), Length(D)) do
		S[Index+i-1] := D[i];
end;

// not fixed for height <> 32!
procedure TPatternEditor.Paint;
const
	WIDTH_ROWNUM = 3;
	SEMP1 = CHR_PERIOD + '00';
	SEMP2 = CHR_PERIOD + CHR_2PERIODS;
var
	i, c, CX, CY, row, fg, bg: Integer;
	s: AnsiString;
	Note: PNote;
	FirstInvalidNote: Byte;
//	UseVolumeColumn: Boolean;
begin
	if (Drawing) or (not Assigned(Module)) then Exit;

//	UseVolumeColumn := Options.Display.ShowVolumeColumn;

	if Options.Tracker.NoteB3Warning then
		FirstInvalidNote := 36
	else
		FirstInvalidNote := 37;

	Drawing := True;
	Console.BeginUpdate;

	{$IFDEF TIMING}
	Time.Start;
	{$ENDIF}

	if ScrollPos > 32 then
		ScrollPos := 32;

	for i := 0 to Height do
	begin
		if (ScrollPos+i) = Module.PlayPos.Row then
		begin
			if Module.PlayPos.Pattern = CurrentPattern then
				c := COL_ROW_CURRENT
			else
				c := COL_ROW_CURRENTOTHER;
		end
		else
			c := COL_ROW;
		Console.Write(
			TextVals[ScrollPos+i],
			Rect.Left, Rect.Top + i, c);
	end;

	for c := 0 to AMOUNT_CHANNELS - 1 do
	for i := 0 to Height do
	begin
		row := ScrollPos + i;

		CX := Rect.Left + WIDTH_ROWNUM + (c * PATTERN_CHAN_WIDTH);
		CY := Rect.Top + i;

		Note := @Module.Notes[CurrentPattern, c, row];
		s := '    '; // 4 spaces

		if Note.Period = 0 then
			sPut(s, 1, CHR_3PERIODS)
		else
			sPut(s, 1, NoteText[Note.Text]);

		if 	InRange(c,   Selection.Left, Selection.Right)  and
			InRange(row, Selection.Top,  Selection.Bottom) then
		begin
			if row mod 16 = 0 then
				bg := COL_HL_SEL_ROW	// highlight sel. rows bg
			else
			if row mod 4 = 0 then
				bg := COL_HL_SEL_BEAT	// highlight sel. beats bg
			else
				bg := COL_SEL_BG;		// normal sel. bg
			fg := COL_SEL_FG;			// selected text
		end
		else
		begin
			if row mod 16 = 0 then
				bg := COL_HL_ROW		// highlight rows bg 9
			else
			if row mod 4 = 0 then
				bg := COL_HL_BEAT		// highlight beats bg 9
			else
				bg := ColorBack;		// normal bg 8
			fg := ColorFore;			// normal text
		end;

		if Note.Text >= FirstInvalidNote then
			bg := COL_INVALID;

		Console.Write(s, CX, CY, fg, bg);


		if (Note.Sample = 0) then
			Console.Write(CHR_2PERIODS, CX + 4, CY, fg, bg)
		else
			Console.Write(TextVals[Note.Sample], CX + 4, CY, fg, bg);


		s[1] := ' ';
		s[4] := ' ';

		if {(UseVolumeColumn) and} (Note.Command = $C) then
		begin
			// volume column + empty effect column
			sPut(s, 2, TextVals[Note.Parameter]);
			Console.Write(s, CX + 6, CY, fg, bg);
			Console.Write(CHR_3PERIODS, CX + 10, CY, 15, bg);
		end
		else
		begin
			// empty volume column
			//if UseVolumeColumn then
			sPut(s, 2, CHR_2PERIODS);
			Console.Write(s, CX + 6, CY, fg, bg);

			if (Note.Command = 0) and (Note.Parameter = 0) then
			begin
				if Options.Tracker.ShowEmptyParamZeroes then
					Console.Write(SEMP1, CX + 10, CY, fg, bg)
				else
					Console.Write(SEMP2, CX + 10, CY, fg, bg);
			end
			else
			begin
				if Options.Tracker.ITCommands then
					Console.Write(Note.CmdText, CX + 10, CY, fg, bg) //Note.GetText
				else
					Console.Write(CmdChars[Note.Command+1] // strings start at [1]
						+ HexVals[Note.Parameter], CX + 10, CY, fg, bg);
			end;
		end;

	end;

	// paint edit cursor if editor is active
	if (Focused) and {(Module.PlayMode <> PLAY_SONG)} (not FollowPlayback) then
	begin
		row := Rect.Top + Cursor.Row - ScrollPos;
		if (row >= Rect.Top) and (row < Rect.Bottom) then
			Console.SetColor(
				Cursor.X + Rect.Left + WIDTH_ROWNUM, row,
				COL_CUR_FG, COL_CUR_BG);
	end;

	Console.EndUpdate;
	Drawing := False;

	{$IFDEF TIMING}
	Time.Stop;
	Window.Caption := FloatToStr(Time.MillisecondsFloat);
	{$ENDIF}
end;

{ TClipPattern }

// Notes: array [0..AMOUNT_CHANNELS-1, 0..63] of TNote;
// Size:  TPoint;

// Schism clipboard format:
//
// Pasted Pattern - IT
// |G-521...... <- 1 channel
// |F-526...A05|B-517...D02|........T77|C#516...F02| <-- multiple channels
// |NNNss...Exx <- format for 1 channel

(*
procedure TClipPattern.FromClipboard;
begin

end;

procedure TClipPattern.ToClipboard;
var
	WasIT, WasPZ: Boolean;
	patt, ch, row: Integer;
	Note: PNote;
	S: String;
begin
	WasIT := Options.Tracker.ITCommands;
	WasPZ := Options.Tracker.ShowEmptyParamZeroes;

	Options.Tracker.ITCommands := True;
	Options.Tracker.ShowEmptyParamZeroes := False;

	S := '';

	for ch := 0 to Size.X do
	for row := 0 to Size.Y do
	begin
		Note := @Notes[ch, row];
		if (Note.Command <> 0) or (Note.Parameter <> 0) then
			Note.GetText;
		S := S + Note.CmdText + sLineBreak;
	end;

	ClipBoard.AsText := S;
	Options.Tracker.ITCommands := WasIT;
	Options.Tracker.ShowEmptyParamZeroes := WasPZ;
end;
*)

initialization

	CmdChars := CmdCharsPT;

end.
