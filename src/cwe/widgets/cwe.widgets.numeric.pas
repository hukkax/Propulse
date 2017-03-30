//
// ConsoleWidgetEngine: numeric widgets
// by hukka 2016
//
unit CWE.Widgets.Numeric;

interface

uses
	Classes, Types, SysUtils,
	CWE.Core, CWE.Dialogs,
	CWE.Widgets.Text;

const
	AllowedNumberKeys = '0123456789';

	ACTION_ASKED_STRING = 1;

type
	TValQuery = class
		class var
			Ctrl: TCWEControl;
		class procedure
			DialogCallback(ID: Word; ModalResult: TDialogButton; Tag: Integer;
							Data: Variant; Dlg: TCWEDialog);
	end;

	TCWEBaseNumeric = class(TCWEControl)
	private
		FMin,
		FMax,
		FPosition:	Integer;
		procedure 	SetMin(const Value: Integer); virtual;
		procedure 	SetMax(const Value: Integer); virtual;
		procedure 	SetPosition(const Value: Integer); overload; virtual;
		procedure 	SetPosition(const Value: AnsiString); overload; virtual;
	public
		property	Min: Integer read FMin write SetMin;
		property	Max: Integer read FMax write SetMax;
		property	Position: Integer read FPosition write SetPosition;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;

		procedure	Init(const Data: array of const);
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWESlider = class;

	TSetValueKind = (VAL_POS, VAL_MIN, VAL_MAX);

	TCWENumericEdit = class(TCWEBaseNumeric)
	private
		MaxLength: 	Byte;	// in chars
		DontSendValUpdates: Boolean;

		procedure 	SetPosition(const Value: Integer); override;
		procedure 	SetMax(const Value: Integer); override;
	public
		Cursor: 	TPoint;
		ReportAnyChange: Boolean; // if False, only triggers change callback on Return key

		SliderCtrl:	TCWESlider;

		procedure 	DoSetValue(Sender: TCWEControl; Kind: TSetValueKind; Value: Integer);

		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;

		procedure 	Paint; override;
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWESlider = class(TCWEBaseNumeric)
	const
		SL_RECT_SLIDER = 0;
		SL_RECT_THUMB  = 1;
	private
		Rects:		array[0..2] of TRect;
		HoveredZone,
		PrevHoveredZone: ShortInt;
		CapturePos: TPoint;
		DontSendValUpdates: Boolean;

		procedure 	SetPosition(const Value: Integer); override;
		procedure 	SetMax(const Value: Integer); override;
	public
		LabelCtrl:	TCWELabel;
		EditCtrl:	TCWENumericEdit;

		function	MouseLeave: Boolean; override;
		procedure 	MouseMove(X, Y: Integer; P: TPoint); override;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;

		procedure 	DoSetValue(Sender: TCWEControl; Kind: TSetValueKind; Value: Integer);

		function 	CreateLabelControl: TCWELabel;
		procedure	Paint; override;
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;


implementation

uses
	Math, TextMode, ShortcutManager, SDL2,
	Dialog.ValueQuery, CWE.MouseCursor;


// ==========================================================================
{ TCWEBaseNumeric }
// ==========================================================================

// Optional 1-3 integer values specify initial Min/Max/Position values
//
constructor TCWEBaseNumeric.Create;
begin
	inherited;

	WantKeyboard := True;
	WantMouse := True;
	Init([0, 0, 0]);
end;

procedure TCWEBaseNumeric.Init(const Data: array of const);
var
	i, c, Val: Integer;
begin
	c := 0;
	for i := Low(Data) to High(Data) do
	case TVarRec(Data[i]).VType of
		vtInteger:
		begin
			Val := Integer(Data[i].VInteger);
			case c of
				0: FMin := Val;
				1: FMax := Val;
				2: FPosition := Val;
			end;
			Inc(c);
		end;
	end;
end;

function TCWEBaseNumeric.KeyDown;
var
	Sc: ControlKeyNames;
	Amount: Integer;
begin
	Result := True;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, []));

	Amount := 1;
	if Shift = [ssCtrl] then
		Amount := 10
	else
	if Shift = [ssShift] then
		Amount := (Max-Min) div 100
	else
	if Shift = [ssAlt] then
		Amount := (Max-Min) div 10;

	case Sc of

		ctrlkeyLEFT,
		ctrlkeyMINUS:
			Position := Position - Amount;

		ctrlkeyRIGHT,
		ctrlkeyPLUS:
			Position := Position + Amount;

		ctrlkeyUP:
			Screen.BrowseControls(True);

		ctrlkeyDOWN:
			Screen.BrowseControls(False);


	else
		Result := inherited KeyDown(Key, Shift);
	end;
end;

function TCWEBaseNumeric.TextInput(var Key: Char): Boolean;
begin
	Result := False;
	if Pos(Key, AllowedNumberKeys) > 0 then
	begin
		if ModalDialog.Dialog <> nil then
			Exit;
		Result := True;
		TValQuery.Ctrl := Self;
		AskString(ACTION_ASKED_STRING, 'Input value:',
			Key, True, TValQuery.DialogCallback);
	end;
end;

procedure TCWEBaseNumeric.SetMax(const Value: Integer);
begin
	FMax := Value;
	FPosition := Math.Min(FPosition, FMax);
	Change;
end;

procedure TCWEBaseNumeric.SetMin(const Value: Integer);
begin
	FMin := Value;
	FPosition := Math.Max(FPosition, FMin);
	Change;
end;

procedure TCWEBaseNumeric.SetPosition(const Value: Integer);
var
	Val: Integer;
begin
	Val := Value;
	Val := Math.Max(Val, FMin);
	Val := Math.Min(Val, FMax);
	FPosition := Val;
	Change;
end;

procedure TCWEBaseNumeric.SetPosition(const Value: AnsiString);
var
	Val: Integer;
begin
	if TryStrToInt(Value, Val) then
		SetPosition(Val);
end;

// ==========================================================================
{ TCWENumericEdit }
// ==========================================================================

constructor TCWENumericEdit.Create;
begin
	inherited;

	WantMouse := True;
	WantKeyboard := True;
	WantHover := False;

	ReportAnyChange := False;

	ColorBack := 0;
	ColorFore := 5;
	MaxLength := 1;

	SetData(0, TConsole.COLOR_LIGHT, 'Focused text');
	SetData(1, TConsole.COLOR_TEXT,  'Focused background');

	Cursor.X := -1;
end;

function TCWENumericEdit.KeyDown;
var
	AtEnd: Boolean;
	Sc: ControlKeyNames;
begin
	AtEnd := (Cursor.X >= Length(Caption));
	Result := True;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	if (ssCtrl in Shift) and (Key = SDLK_BACKSPACE) then
	begin
		Caption := '';
		Cursor.X := 0;
	end
	else
	case Sc of

		ctrlkeyLEFT:
			Cursor.X := Math.Max(Cursor.X-1, 0);

		ctrlkeyRIGHT:
			Cursor.X := Math.Min(Cursor.X+1, MaxLength);

		ctrlkeyPreviousCtrl,
		ctrlkeyUP:
			Screen.BrowseControls(True);

		ctrlkeyNextCtrl,
		ctrlkeyDOWN:
			Screen.BrowseControls(False);

		ctrlkeyDELETE:
			if not AtEnd then
				Delete(Caption, Cursor.X+1, 1);

		ctrlkeyHOME:
			Cursor.X := 0;

		ctrlkeyEND:
			Cursor.X := MaxLength;

		ctrlkeyRETURN:
			Change(True); // trigger change notify event

	else
		Exit(False);
	end;

	Key := 0;
	Paint;
end;

function TCWENumericEdit.TextInput(var Key: Char): Boolean;
begin
	Result := False;
	if (Ord(Key) < 32) or (ModKeys(ssCtrl)) or
		(Pos(Key, AllowedNumberKeys) < 1) then Exit;

	Caption[Cursor.X+1] := AnsiChar(Key);
	if Cursor.X < MaxLength then
		Inc(Cursor.X);

	Result := True;
	SetPosition(StrToInt(Caption));
end;

function TCWENumericEdit.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Button <> mbLeft then Exit(False);

	Result := True;
	Cursor.X := Math.Min(P.X, Length(Caption));
	Paint;
end;

procedure TCWENumericEdit.Paint;
var
	C, B: Byte;
begin
	if not Screen.Active then Exit;

	DrawBorder;

	if Cursor.X >= MaxLength then
		Cursor.X := MaxLength-1;

	if Cursor.X < 0 then
		Cursor.X := 0
	else
	if Cursor.X > MaxLength then
		Cursor.X := MaxLength;

	if (Focused) or (Hovered) then
	begin
		C := Data[0].Value;
		B := Data[1].Value;
	end
	else
	begin
		C := ColorFore;
		B := ColorBack;
	end;

	Console.FillRect(Rect, ' ', C, B);
	Console.Write(Copy(Caption, 1, Rect.Right-Rect.Left), Rect.Left, Rect.Top);

	if Focused then
	begin
		C := Rect.Left + Cursor.X;
		if C < Rect.Right then
			Console.SetColor(C, Cursor.Y + Rect.Top, TConsole.COLOR_TEXT, TConsole.COLOR_LIGHT);
	end;
end;

procedure TCWENumericEdit.SetMax(const Value: Integer);
begin
	inherited;

	MaxLength := Length(IntToStr(Max));

	if Assigned(SliderCtrl) then
		SliderCtrl.DoSetValue(Self, VAL_MAX, Value);

	SetPosition(Position);
end;

procedure TCWENumericEdit.SetPosition(const Value: Integer);
begin
	inherited;

	Caption := Format('%.*d', [MaxLength, Position]);
	Paint;

	if (not DontSendValUpdates) and (Assigned(SliderCtrl)) then
		SliderCtrl.DoSetValue(Self, VAL_POS, Value);
end;

procedure TCWENumericEdit.DoSetValue(Sender: TCWEControl;
	Kind: TSetValueKind; Value: Integer);
begin
	DontSendValUpdates := (Sender = SliderCtrl);

	case Kind of
		VAL_POS:	Position := Value;
		VAL_MIN:	Min := Value;
		VAL_MAX:	Max := Value;
	end;

	DontSendValUpdates := False;
end;

// ==========================================================================
{ TCWESlider }
// ==========================================================================

constructor TCWESlider.Create;
begin
	inherited;

	SetBorder(True, False, True, True);
	WantKeyboard := True;
	WantMouse := True;
	WantHover := True;
	WantPixelPrecision := True;
end;

function TCWESlider.CreateLabelControl: TCWELabel;
begin
	LabelCtrl := TCWELabel.Create(Parent,
		'', 'Label for ' + Self.ID,
		Bounds(Rect.Right - Screen.Rect.Left + 1, Rect.Top - Screen.Rect.Top,
			Length(IntToStr(Max)), 1));

	Result := LabelCtrl;
	Result.ColorFore := ColorFore;
	Result.ColorBack := ColorBack;
//	Result.Alignment := ALIGN_RIGHT;
end;

procedure TCWESlider.MouseMove(X, Y: Integer; P: TPoint);
var
	i: Integer;
	R: TRect;
begin
	// user is currently moving the thumb
	if Capturing then
	begin
		R := Rects[SL_RECT_SLIDER];
		i := Trunc( (MouseCursor.Pos.X - R.Left - CapturePos.X)
			 / (R.Right - R.Left - Console.Font.Width) * (Max - Min) );
		SetPosition(i + Min);
		Paint;
		Exit;
	end;

	if Hovered then
	for i := High(Rects) downto 0 do
	begin
		if PtInRect(Rects[i], MouseCursor.Pos) then
		begin
			if i = PrevHoveredZone then Exit; // prevent needless redraws
			HoveredZone := i;
			PrevHoveredZone := i;
			Paint;
			Exit;
		end;
	end;

	if HoveredZone >= 0 then
	begin
		HoveredZone := -1;
		if PrevHoveredZone >= 0 then
			Paint;
		PrevHoveredZone := -1;
	end;
end;

function TCWESlider.MouseLeave: Boolean;
begin
	HoveredZone := -1;
	PrevHoveredZone := -1;
	Result := True;
end;

function TCWESlider.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Button = mbMiddle then Exit(False);

	Result := True;
	case HoveredZone of

		SL_RECT_THUMB:
		begin
			Screen.MouseInfo.Control := Self;
			Screen.MouseInfo.Capturing := True;
			Paint;
			CapturePos := Types.Point(
				MouseCursor.Pos.X - Rects[SL_RECT_THUMB].Left,
				MouseCursor.Pos.Y - Rects[SL_RECT_THUMB].Top);
		end;

		SL_RECT_SLIDER:	// adjust value by clicking on gutter
		begin
			if MouseCursor.Pos.X < Rects[SL_RECT_THUMB].Left then
			begin
				if Position > Min then Position := Position - 1;
			end
			else
			if MouseCursor.Pos.X >= Rects[SL_RECT_THUMB].Right then
			begin
				if Position < Max then Position := Position + 1;
			end;
			Paint;
		end;

	else
		Result := False;
	end;
end;

function TCWESlider.MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Button = mbMiddle then Exit(False);
	HoveredZone := -1;
	PrevHoveredZone := -1;
	Paint;
	Result := True;
end;

function TCWESlider.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
var
	V: Integer;
begin
	V := Position + WheelDelta;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Position := V;
	Paint;
	Result := True;
end;

procedure TCWESlider.Paint;
var
	W, GH, X1, X2, Y1, Y2: Integer;
	IH: Single;
	R: TRect;
begin
	if not Screen.Active then Exit;

	W := Console.Font.Width;
	GH := Console.Font.Height;

	Rects[SL_RECT_SLIDER] := Types.Rect(
		Rect.Left*W, Rect.Top*GH, (Rect.Right)*W, Rect.Bottom*GH);

	inherited;

	Console.FillRect(Rect, ' ', TConsole.COLOR_3DLIGHT, TConsole.COLOR_BLANK);
	if Position > Max then Position := Max;

	GetPixelRect(R);

	W := Console.Font.Width;	 	// size of thumb
	X1 := R.Left;
	X2 := R.Right - W;
	Y1 := R.Top;
	Y2 := R.Bottom;
	GH := X2 - X1;					// pixel width of the gutter area
	if (Max > 0) and (Min < Max) then
		IH := GH / (Max - Min)	 	// pixel width of one item
	else
		IH := 0;
	GH := Round(IH * (Position - Min));

	X2 := Math.Min(X1 + W + GH, R.Right);
	Inc(X1, GH);

	Rects[SL_RECT_THUMB] := Types.Rect(X1, Y1, X2, Y2);

	if (Capturing) or (Focused) then
		W := TConsole.COLOR_LIGHT
	else
	if PtInRect(Rects[SL_RECT_THUMB], MouseCursor.Pos) then
		W := TConsole.COLOR_LIGHT
	else
	if (Hovered) then
		W := TConsole.COLOR_3DLIGHT
	else
		W := TConsole.COLOR_PANEL;

	if (X1 >= Rects[SL_RECT_SLIDER].Left) and (Y1 >= Rects[SL_RECT_SLIDER].Top) then
		Console.Bitmap.FillRectS(X1, Y1, X2, Y2, Console.Palette[W]);
end;

procedure TCWESlider.SetMax(const Value: Integer);
begin
	inherited;
	if (not DontSendValUpdates) and (Assigned(EditCtrl)) then
		EditCtrl.DoSetValue(Self, VAL_MAX, Value);
end;

procedure TCWESlider.SetPosition(const Value: Integer);
var
	S: AnsiString;
begin
	inherited;
	if (not DontSendValUpdates) and (Assigned(EditCtrl)) then
		EditCtrl.DoSetValue(Self, VAL_POS, Value);

	if Assigned(LabelCtrl) then
	begin
		if Pos('%', Caption) > 0 then
			S := Format(Caption, [Position])
		else
		begin
			S := IntToStr(Position);
			if (Position > 0) and (Min < 0) then
				S := '+' + S
			else
			if Position = 0 then
				S := S + ' '; // !!! FIXME hack, needs full repaint instead
		end;

		LabelCtrl.Caption := S;
		LabelCtrl.Paint;
	end;

end;

procedure TCWESlider.DoSetValue(Sender: TCWEControl;
	Kind: TSetValueKind; Value: Integer);
begin
	DontSendValUpdates := (Sender = EditCtrl);

	case Kind of
		VAL_POS:	Position := Value;
		VAL_MIN:	Min := Value;
		VAL_MAX:	Max := Value;
	end;

	DontSendValUpdates := False;
end;

// ==========================================================================
{ TValQuery }
// ==========================================================================

class procedure TValQuery.DialogCallback(ID: Word;
	ModalResult: TDialogButton; Tag: Integer; Data: Variant;
	Dlg: TCWEDialog);
var
	Ct: TCWEControl;
begin
	if Dlg = nil then Exit;
	if ModalResult <> btnOK then Exit;

	case ID of

		ACTION_ASKED_STRING:
		begin
			if not Assigned(Ctrl) then Exit;

			Ct := Dlg.Dialog.FindControl('Edit');
			if Ct = nil then Exit;

			if (Ctrl is TCWESlider) then
				TCWESlider(Ctrl).SetPosition(TCWEEdit(Ct).Caption);
		end;

	end;
end;


end.

