//
// ConsoleWidgetEngine
// by hukka 2016-2017
//
unit CWE.Core;

interface

uses
	Classes, Types, SysUtils,
	Generics.Collections,
	ShortcutManager,
	TextMode;

type
	TMouseButton = (mbLeft, mbRight, mbMiddle);

	TCWEScreen  = class;
	TCWEControl = class;
	TCWEControlClass = class of TCWEControl;

	TCWENotifyEvent		= 	procedure (Sender: TCWEControl)
							of Object;
	TCWENotifyEventID	= 	procedure (Sender: TCWEControl; ID: Integer)
							of Object;
	TCWETextInputEvent	=	function (Sender: TCWEControl;
							var Key: Char): Boolean
							of Object;
	TCWEKeyDownEvent	= 	procedure (Sender: TCWEControl;
							var Key: Integer; Shift: TShiftState; var Handled: Boolean)
							of Object;
	TCWEMouseWheelEvent	= 	function (Sender: TCWEControl;
							Shift: TShiftState; DirDown: Boolean; P: TPoint): Boolean
							of Object;
	TCWEMouseDownEvent	= 	function (Sender: TCWEControl;
							Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean
							of Object;
	TCWEMouseUpEvent	= 	function (Sender: TCWEControl;
							Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean
							of Object;
	TCWEMouseMoveEvent	= 	function (Sender: TCWEControl; X, Y: Integer; P: TPoint): Boolean
							of Object;
	TCWEMouseEnterEvent	= 	function (Sender: TCWEControl): Boolean
							of Object;
	TCWEMouseLeaveEvent	= 	function (Sender: TCWEControl): Boolean
							of Object;


	TControlData = record
		Text:  AnsiString;
		Value: Byte;
	end;

	TControlBorder = packed record
		Enabled,
		Fat,
		Sunken,
		Pixel:		Boolean;
	end;

	ControlKeyNames = (
		ctrlkeyNONE,					ctrlkeyNextCtrl,	ctrlkeyPreviousCtrl,
		ctrlkeyUP,		ctrlkeyDOWN,	ctrlkeyLEFT,		ctrlkeyRIGHT,
		ctrlkeyRETURN,	ctrlkeyTAB,		ctrlkeySPACE,		ctrlkeyBACKSPACE,
		ctrlkeyINSERT,	ctrlkeyDELETE,	ctrlkeyHOME,		ctrlkeyEND,
		ctrlkeyPGUP,	ctrlkeyPGDN,	ctrlkeyMINUS,		ctrlkeyPLUS
	);

	TMouseInfo = record
		Pos,
		Prev:		TPoint;
		Control:	TCWEControl;
		Capturing:	Boolean;	// Control wants all mouse events while button held down
		Buttons:	array[TMouseButton] of Boolean;
	end;

	TTimerCallback = record
		Enabled:	Boolean;
		Control:	TCWEControl;
		ID:			Integer;
		Callback:	TCWENotifyEventID;
		Counter,
		Interval:	Word;
	end;

	TCWEControl = class
	protected
		FOnChange:		TCWENotifyEvent;
		FOnTextInput:	TCWETextInputEvent;
		FOnKeyDown: 	TCWEKeyDownEvent;
		FOnMouseWheel: 	TCWEMouseWheelEvent;
		FOnMouseDown: 	TCWEMouseDownEvent;
		FOnMouseUp:		TCWEMouseUpEvent;
		FOnMouseMove:	TCWEMouseMoveEvent;
		FOnMouseEnter:	TCWEMouseEnterEvent;
		FOnMouseLeave:	TCWEMouseLeaveEvent;

		Screen:			TCWEScreen;
		class var
			Console:	TConsole;
	public
		Rect:			TRect;
		Width,
		Height:			Word;

		Enabled,					// Can control be activated/clicked?
		WantPixelPrecision,			// Want mouse movement events on each pixel?
		WantMouse,					// Want any mouse events?
		//WantMouseCapture,			// Want all mouse events while mouse button down?
		WantKeyboard,				// Want any keyboard events?
		WantHover:		Boolean;	// Want to be notified when mouse hovering?
		MouseCoords:	TPoint;		// Pixel coordinates of mouse inside control

		Parent:			TCWEControl;
		Controls:		TObjectList<TCWEControl>;

		ColorBack,
		ColorFore:		Integer;
		Border:			TControlBorder;

		IsProtected:	Boolean;

		Caption,
		ID:				AnsiString;

		Data:		array of TControlData;	// Custom data

		procedure	SetData(Index: Integer; Val: Byte; const Text: AnsiString);

		procedure	SetBorder(Enabled, Fat, Sunken, Pix: Boolean);
		procedure 	DrawBorder; overload;
		procedure 	DrawBorder(R: TRect; BgCol: Integer = -1); overload;

		function	Focused: Boolean; inline;
		function	Hovered: Boolean; inline;
		function	Capturing: Boolean; inline;

		procedure	SetTimerCallback(cbCallback: TCWENotifyEventID;
					cbID: Integer; cbInterval: Word);
		procedure	DisableTimerCallback;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					SetProtect: Boolean = False); virtual;
		destructor	Destroy; override;
		//procedure	Init(const Data: array of const); virtual; abstract;
		{procedure	AddControl(var Ctrl: TCWEControl;
					const Caption: AnsiString; const Bounds: TRect);}

		procedure	SetColors(Fg, Bg: Byte);
		procedure 	SetBounds(const Bounds: TRect); virtual;
		procedure	SetSize(X, Y: Word); virtual;

		function 	GetPixelSize: TPoint;
		procedure 	GetPixelRect(var R: TRect);
		procedure	Protect;

		function	GetControlAt(X, Y: Word): TCWEControl;

		procedure 	Change(TriggerChange: Boolean = True);

		property	OnChange:     TCWENotifyEvent     read FOnChange     write FOnChange;
		property	OnTextInput:  TCWETextInputEvent  read FOnTextInput  write FOnTextInput;
		property	OnKeyDown:    TCWEKeyDownEvent    read FOnKeyDown    write FOnKeyDown;
		property	OnMouseWheel: TCWEMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
		property	OnMouseDown:  TCWEMouseDownEvent  read FOnMouseDown  write FOnMouseDown;
		property	OnMouseUp:    TCWEMouseUpEvent    read FOnMouseUp    write FOnMouseUp;
		property	OnMouseMove:  TCWEMouseMoveEvent  read FOnMouseMove  write FOnMouseMove;
		property	OnMouseEnter: TCWEMouseEnterEvent read FOnMouseEnter write FOnMouseEnter;
		property	OnMouseLeave: TCWEMouseLeaveEvent read FOnMouseLeave write FOnMouseLeave;

		procedure	Paint; virtual;
		procedure 	PaintTo(R: TRect); virtual;

		function	ProcessShortcut(Sc: ControlKeyNames): Boolean; virtual;

		function	TextInput(var Key: Char): Boolean; virtual;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; virtual;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; virtual;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; virtual;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; virtual;
		procedure 	MouseMove(X, Y: Integer; P: TPoint); virtual;
		function	MouseEnter: Boolean; virtual;
		function	MouseLeave: Boolean; virtual;
	end;

	TCWEBorder = class(TCWEControl)
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					Protect: Boolean = False); override;
		procedure	Paint; override;
	end;

	TCWEScreen = class(TCWEControl)
	class var
		MouseInfo: 		TMouseInfo;
	private
		FOnShow:		TCWENotifyEvent;
	public
		ActiveControl:	TCWEControl;		// Mouse button held down over control
		HoveredControl: TCWEControl;
		AllControls: 	TObjectList<TCWEControl>;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); reintroduce; virtual;
		destructor	Destroy; override;

		property	OnShow: TCWENotifyEvent read FOnShow write FOnShow;
		function	OnContextMenu: Boolean; virtual;

		procedure	Show; virtual;
		procedure 	Leave; virtual;
		function	Active: Boolean;
		procedure	Paint; override;

{		procedure	AddControl(var Ctrl: TCWEControl;
					const Caption, ID: AnsiString; const Bounds: TRect;
					&Protected: Boolean = False);}

		function 	ActivateControl(Ctrl: TCWEControl): Integer;
		procedure 	BrowseControls(Previous: Boolean = False);
		function	FindControl(const ID: AnsiString): TCWEControl;

		procedure	HandleCommand(const Cmd: Cardinal); virtual;

		// Event handlers
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		procedure 	MouseMove(X, Y: Integer; P: TPoint); override;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;
	end;

	procedure InitCWE;
	procedure ChangeScreen(var NewScreen: TCWEScreen);
	function  IsShiftPressed: Boolean; inline;
    function  ModKeys(Key: TShiftStateEnum): Boolean; inline;

var
	Console: 		TConsole;
	Screens: 		TObjectList<TCWEScreen>;
	CurrentScreen: 	TCWEScreen;
	ControlKeys: 	TKeyBindings;
	TimerCallback: 	TTimerCallback;


implementation

uses
	MainWindow, Math, SDL.Api.Types,
	CWE.Widgets.Text, CWE.Widgets.Scrollers, CWE.Dialogs,
	Screen.Editor, ProTracker.Editor; // !!! so dumb

// ==========================================================================
// Utility
// ==========================================================================

procedure ChangeScreen(var NewScreen: TCWEScreen);
begin
	// don't change screens while a modal dialog is showing
	if (ModalDialog.Dialog <> nil) and (CurrentScreen = ModalDialog.Dialog)
		and (NewScreen <> ModalDialog.Dialog) then
			Exit;

	if NewScreen <> CurrentScreen then
	begin
		if Assigned(CurrentScreen) then
			CurrentScreen.Leave;

		if NewScreen = nil then
			NewScreen := Editor;

		// don't return MouseUp event when e.g. a button click
		// changes the active screen
		NewScreen.MouseInfo.Control := nil;

		// DisableDisplayUpdates := True; !!!
		CurrentScreen := NewScreen;
	end;

	NewScreen.MouseInfo.Pos  := Types.Point(0, 0);
	NewScreen.MouseInfo.Prev := Types.Point(0, 0);

	Console.FillRect(CurrentScreen.Rect, ' ', 15, TConsole.COLOR_PANEL);

	if CurrentScreen = Editor then
		Editor.SetSample(CurrentSample);

	CurrentScreen.Paint;
	CurrentScreen.Show;

	Window.PlayModeChanged;
	//DisableDisplayUpdates := False;
	//UpdateDisplay;
	//ChangeMousePointer;
end;

function IsShiftPressed: Boolean;
begin
	Result := ModKeys(ssShift);
end;

function ModKeys(Key: TShiftStateEnum): Boolean;
begin
    Result := Key in GetShiftState;
end;

procedure InitCWE;
begin
	TimerCallback.Enabled := False;
	TimerCallback.Counter := 0;

	with Shortcuts do
	begin
		ControlKeys := SetContext('Widgets');

		Bind(ctrlkeyNextCtrl,     'NextControl',     	'Tab');
		Bind(ctrlkeyPreviousCtrl, 'PreviousControl', 	'Shift+Tab');
		Bind(ctrlkeyUP,			'',						'Up');
		Bind(ctrlkeyDOWN,		'',						'Down');
		Bind(ctrlkeyLEFT,		'',						'Left');
		Bind(ctrlkeyRIGHT,		'',						'Right');
		Bind(ctrlkeyRETURN,		'Return',				'Return');
		Bind(ctrlkeyTAB,		'',						'Tab');
		Bind(ctrlkeySPACE,		'',						'Space');
		Bind(ctrlkeyINSERT,		'',						'Insert');
		Bind(ctrlkeyDELETE,		'',						'Delete');
		Bind(ctrlkeyHOME,		'',						'Home');
		Bind(ctrlkeyEND,		'',						'End');
		Bind(ctrlkeyPGUP,		'',						'PageUp');
		Bind(ctrlkeyPGDN,		'',						'PageDown');
		Bind(ctrlkeyBACKSPACE,	'Backspace',			'Backspace');
		Bind(ctrlkeyMINUS,		'Minus',				'KeyPad -');
		Bind(ctrlkeyPLUS,		'Plus',					'KeyPad +');
	end;

	with TCWEScrollbar do
	begin
		COLOR_SB_GUTTER := 1;
		COLOR_SB_THUMB  := 3;
		COLOR_SB_BUTTON := 2;
		COLOR_SB_ICON   := 0;
		COLOR_SB_HOVER  := 11;
		COLOR_SB_ACTIVE := 11;
	end;
end;

{ TCWEScreen }

constructor TCWEScreen.Create(var Con: TConsole; const sCaption, sID: AnsiString);
begin
	inherited Create(nil, sCaption, sID,
		Types.Rect(0, 0, Con.Width, Con.Height), True);

	Console := Con;
	Screen := Self;

	Controls := TObjectList<TCWEControl>.Create(True);
	AllControls := TObjectList<TCWEControl>.Create(False);

	ActiveControl  := nil;
	HoveredControl := nil;
end;

destructor TCWEScreen.Destroy;
begin
	AllControls.Free;

	inherited Destroy;
end;

function TCWEScreen.OnContextMenu: Boolean;
begin
	Result := True;
end;

procedure TCWEScreen.Show;
var
	Ctrl: TCWEControl;
begin
	if Assigned(FOnShow) then
		FOnShow(Self);
	for Ctrl in Controls do
		if (Ctrl is TCWEScrollbar) then
			(Ctrl as TCWEScrollbar).AdjustBounds;
end;

procedure TCWEScreen.Leave;
begin
	DisableTimerCallback;
end;

function TCWEScreen.FindControl(const ID: AnsiString): TCWEControl;
begin
	for Result in AllControls do
		if Result.ID = ID then Exit;
	Result := nil;
end;

procedure TCWEScreen.BrowseControls(Previous: Boolean = False);
var
//	Ctrl: TCWEControl;
	i, cc: Integer;
begin
	if (ActiveControl = nil) then
	begin
		ActiveControl := Controls[0];
//		Exit;
	end;

	i := Controls.IndexOf(ActiveControl);
	if i < 0 then Exit;

	for cc := 1 to Controls.Count do
	begin
		if Previous then
			Dec(i)
		else
			Inc(i);

		if i >= Controls.Count then
			i := 0
		else
		if i < 0 then
			i := Controls.Count-1;

		if Controls[i].WantKeyboard then
		begin
			ActiveControl := Controls[i];
			Paint;
			Exit;
		end;
	end;
end;

// change focused control with Tab/Shift-Tab
//
function TCWEScreen.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
var
	Sc: ControlKeyNames;
begin
	if (Controls = nil) or (Controls.Count < 1) then Exit(False);

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	case Sc of

		ctrlkeyNextCtrl:
		begin
			BrowseControls(False);
			Result := True;
		end;

		ctrlkeyPreviousCtrl:
		begin
			BrowseControls(True);
			Result := True;
		end;

	else
		if ActiveControl <> nil then
			Result := ActiveControl.KeyDown(Key, Shift)
		else
			Result := False;
	end;
end;

function TCWEScreen.TextInput(var Key: Char): Boolean;
begin
	if ActiveControl <> nil then
		Result := ActiveControl.TextInput(Key)
	else
		Result := False;
end;

function TCWEScreen.ActivateControl(Ctrl: TCWEControl): Integer;
begin
	Result := -1;
	if (AllControls = nil) or (AllControls.Count < 1) then Exit;

	ActiveControl := Ctrl;
	Paint;
end;

function TCWEScreen.Active: Boolean;
begin
	Result := (CurrentScreen = Self);
end;

{procedure TCWEScreen.AddControl;
var
	BR: TRect;
begin
	Ctrl.ID := ID;
	Ctrl.Caption := Caption;
	Ctrl.Protected := &Protected;

	BR := Bounds;
	OffsetRect(BR, Rect.Left, Rect.Top);
	Ctrl.SetBounds(BR);
end;}

procedure TCWEScreen.HandleCommand(const Cmd: Cardinal);
begin
	// TODO: HandleCommand (?)
end;

procedure TCWEScreen.MouseMove(X, Y: Integer; P: TPoint);

	procedure DoMouseLeave(const Ctrl: TCWEControl);
	begin
		HoveredControl := nil;
		with Ctrl do
		if WantHover then
		begin
			MouseLeave;
			Paint;	// redraw control without hover effect
		end;
	end;

var
	OldCtrl: TCWEControl;
begin
	if (P.X = MouseInfo.Prev.X) and (P.Y = MouseInfo.Prev.Y) then
		if (HoveredControl = nil) or (not HoveredControl.WantPixelPrecision) then
			Exit;

	X := X - Rect.Left * Console.Font.Width;
	Y := Y - Rect.Top  * Console.Font.Height;

	MouseInfo.Prev := P;
	MouseInfo.Pos  := P;

	if (MouseInfo.Control <> nil) and (MouseInfo.Capturing) then
	begin
		MouseInfo.Control.MouseMove(X, Y, Types.Point(
			P.X - MouseInfo.Control.Rect.Left,
			P.Y - MouseInfo.Control.Rect.Top));
		Exit;
	end;
{	else
	if (HoveredControl <> nil) and (HoveredControl.WantHover) then
	begin
		HoveredControl.MouseMove(X, Y, Point(
			P.X - HoveredControl.Rect.Left,
			P.Y - HoveredControl.Rect.Top));
		Exit;
	end;}

	// Just update visual state of currently active control
	//
	if MouseInfo.Buttons[mbLeft] then
	begin
		OldCtrl := GetControlAt(P.X, P.Y);
		if OldCtrl = MouseInfo.Control then
			ActiveControl := OldCtrl
		else
			ActiveControl := nil;
		if MouseInfo.Control <> nil then
			MouseInfo.Control.Paint;
		Exit;
	end;

	OldCtrl := HoveredControl;

	if (P.X < 0) or (P.Y < 0) then
	begin
		if (OldCtrl <> nil) then
			DoMouseLeave(OldCtrl);
		MouseInfo.Control := nil;
	end
	else
	begin
		MouseInfo.Control := GetControlAt(P.X, P.Y);
		//Window.Caption := Format('%d,%d', [MouseInfo.Pos.X, MouseInfo.Pos.Y]);

		if (OldCtrl <> nil) and (OldCtrl <> MouseInfo.Control) then
			DoMouseLeave(OldCtrl);

		if (MouseInfo.Control <> nil) then
		begin
			//Window.Caption := MouseInfo.Control.ID;
			if not MouseInfo.Buttons[mbLeft] then
			begin
				HoveredControl := MouseInfo.Control;
				if HoveredControl.WantHover then
				begin
					HoveredControl.MouseEnter;
					HoveredControl.Paint;
				end;
			end;
			MouseInfo.Control.MouseMove(X, Y, Types.Point(
				P.X - MouseInfo.Control.Rect.Left,
				P.Y - MouseInfo.Control.Rect.Top));
		end;
	end;
end;

function TCWEScreen.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
var
	Ctrl: TCWEControl;
begin
	Ctrl := MouseInfo.Control;

	if (Ctrl <> nil) and ((Ctrl.WantMouse) or (Assigned(Ctrl.OnMouseWheel))) then
	begin
		P.X := P.X - Ctrl.Rect.Left;
		P.Y := P.Y - Ctrl.Rect.Top;
		Result := Ctrl.MouseWheel(Shift, WheelDelta, P);
	end
	else
		Result := False;
end;

function TCWEScreen.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
var
	Ctrl: TCWEControl;
label
	CtrlMouseDown;
begin
	Result := False;

	MouseInfo.Buttons[Button] := True;
	MouseMove(X, Y, P);

	Ctrl := HoveredControl;
	if (Ctrl = nil) then Exit;

	X := X - Rect.Left - Ctrl.Rect.Left * Console.Font.Width;
	Y := Y - Rect.Top  - Ctrl.Rect.Top  * Console.Font.Height;

	// never focus scrollbars
	if (not (Ctrl is TCWEScrollbar)) then
	begin
		// url labels want mousedown but don't want keyboard focus
		if (Ctrl is TCWEURLLabel) then
			goto CtrlMouseDown
		else
		if (not Assigned(Ctrl.OnMouseDown)) and
			(Ctrl.WantKeyboard = False) then
		begin
			if (not (Ctrl is TCWEButton)) or
				(TCWEButton(Ctrl).Shortcut.Key = 0) then
					Exit;
		end;

		ActiveControl := Ctrl;
		MouseInfo.Control := Ctrl;
	end;
	{if (Ctrl.WantMouse) or (Assigned(Ctrl.OnMouseDown)) then
	begin
		ActiveControl := Ctrl;
		MouseInfo.Control := Ctrl;
	end
	else
		Exit;}

CtrlMouseDown:
	P.X := P.X - Ctrl.Rect.Left;
	P.Y := P.Y - Ctrl.Rect.Top;
	Result := Ctrl.MouseDown(Button, X, Y, P);

	// stupid hack zone
	// mousedown handler might have freed the screen (in case of modal dialog)
	if Self = CurrentScreen then
		Paint
	else
		MouseInfo.Control := nil; // don't call mouseup event
end;

function TCWEScreen.MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := False;
	if Self <> CurrentScreen then Exit;

	MouseInfo.Buttons[Button] := False;
	MouseInfo.Capturing := False;

	if MouseInfo.Control <> nil then
	begin
		if (Button = mbLeft) and (MouseInfo.Control.WantKeyboard) then
			ActivateControl(MouseInfo.Control);
		if MouseInfo.Control.WantMouse then
			Result := MouseInfo.Control.MouseUp(Button, X, Y, P);
{		else
		if 	(MouseInfo.Control is TCWEButton) and
			(TCWEButton(MouseInfo.Control).Shortcut <> 0) then
			ActivateControl(MouseInfo.Control); // dumb kludge}
	end;
end;

procedure TCWEScreen.Paint;
begin
	if Active then
	begin
		Console.BeginUpdate;
		inherited Paint;
		Console.EndUpdate;
	end;
end;

{ TCWEControl }

constructor	TCWEControl.Create(Owner: TCWEControl;
	const sCaption, sID: AnsiString; const Bounds: TRect;
	SetProtect: Boolean = False);
var
	BR: TRect;
begin
	inherited Create;

	Parent := Owner;
	Data := nil;
	Controls := nil;

	ID := sID;
	Caption := sCaption;
	IsProtected := SetProtect;

	if (Owner <> nil) and (Owner.Controls = nil) then
	begin
		Owner.Controls := TObjectList<TCWEControl>.Create(True);
	end;

	Controls := nil;
	FOnChange     := nil;
	FOnTextInput  := nil;
	FOnKeyDown    := nil;
	FOnMouseWheel := nil;
	FOnMouseDown  := nil;
	FOnMouseUp    := nil;
	FOnMouseMove  := nil;
	FOnMouseEnter := nil;
	FOnMouseLeave := nil;

	BR := Bounds;

	if Owner = nil then
		Screen := nil
	else
	if (Owner is TCWEScreen) then
		Screen := TCWEScreen(Owner)
	else
		Screen := Owner.Screen;

	if (Screen <> nil) then
	begin
//		OffsetRect(BR, Owner.Rect.Left, Owner.Rect.Top);

		Console := Screen.Console;
		Screen.Controls.Add(Self);
		if not (Self is TCWEScreen) then
			Screen.AllControls.Add(Self);
	end;

	SetBounds(BR);

	ColorBack := -1;//TConsole.COLOR_PANEL;
	ColorFore := TConsole.COLOR_TEXT;
	SetBorder(False, False, True, False);

	WantMouse := False;
	WantKeyboard := False;
	WantHover := False;
	WantPixelPrecision := False;
end;

destructor TCWEControl.Destroy;
begin
	Controls.Free;

	inherited Destroy;
end;

procedure TCWEControl.SetColors(Fg, Bg: Byte);
begin
	ColorFore := Fg;
	ColorBack := Bg;
end;

procedure TCWEControl.SetBounds(const Bounds: TRect);
begin
	Rect := Bounds;
	if Parent <> nil then
		OffsetRect(Rect, Parent.Rect.Left, Parent.Rect.Top);
	//ClientRect := Types.Rect(Bounds.Left+1, Bounds.Top+1, Bounds.Right-1, Bounds.Bottom-1);
	Width  := Max(Bounds.Right  - Bounds.Left - 1, 1);
	Height := Max(Bounds.Bottom - Bounds.Top  - 1, 1);
end;

procedure TCWEControl.SetSize(X, Y: Word);
begin
	Width  := X;
	Height := Y;
	Rect := Bounds(Rect.Left, Rect.Top, Width, Height);
end;

function TCWEControl.GetPixelSize: TPoint;
begin
	Result := Types.Point(
		(Rect.Right - Rect.Left) * Console.Font.Width,
		(Rect.Bottom - Rect.Top) * Console.Font.Height);
end;

procedure TCWEControl.GetPixelRect(var R: TRect);
begin
	R.Left   := Rect.Left   * Console.Font.Width;
	R.Right  := Rect.Right  * Console.Font.Width;
	R.Top    := Rect.Top    * Console.Font.Height;
	R.Bottom := Rect.Bottom * Console.Font.Height;
end;

{procedure TCWEControl.AddControl;
var
	R: TRect;
begin
	if Controls = nil then
		Controls := TObjectList<TCWEControl>.Create(True);

	Ctrl.Caption := Caption;
	R := Bounds;
	OffsetRect(R, Rect.Left, Rect.Top);
	Ctrl.SetBounds(R);
	Controls.Add(Ctrl);
end;}

procedure TCWEControl.Change(TriggerChange: Boolean = True);
begin
	if (TriggerChange) and (Assigned(FOnChange)) then
		FOnChange(Self);
	Paint;
end;

function TCWEControl.Focused: Boolean;
begin
	if Screen <> nil then
		Result := (Self = Screen.ActiveControl)
	else
		Result := False;
end;

function TCWEControl.Hovered: Boolean;
begin
	if Screen <> nil then
		Result := (Self = Screen.HoveredControl)
	else
		Result := False;
end;

function TCWEControl.Capturing: Boolean;
begin
	if Screen <> nil then
		Result := (Screen.MouseInfo.Capturing) and (Screen.MouseInfo.Control = Self)
	else
		Result := False;
end;

function TCWEControl.GetControlAt(X, Y: Word): TCWEControl;
var
	Ctrl: TCWEControl;
	P: TPoint;
begin
	Result := nil;
	if (Controls = nil) or (Controls.Count < 1) then Exit;

	P := Types.Point(X, Y);

	for Ctrl in Controls do
	begin
		if PtInRect(Ctrl.Rect, P) then
			if (Ctrl.WantMouse) or (Ctrl.WantHover) then
				Result := Ctrl;
	end;

	if (Result = Self) and (Self is TCWEScreen) then
		Result := nil;

	// todo: GetControlAt: shouldn't be done automatically
	if (Self is TCWEScreen) then	// mouse is likely hovering
	begin
		(Self as TCWEScreen).HoveredControl := Result;
		{if Result <> nil then
			Result.Paint;}
	end;
end;

function TCWEControl.ProcessShortcut(Sc: ControlKeyNames): Boolean;
begin
	Result := False;
end;

function TCWEControl.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
begin
	if Assigned(FOnKeyDown) then
		FOnKeyDown(Self, Key, Shift, Result)
	else
		Result := False;
end;

function TCWEControl.TextInput(var Key: Char): Boolean;
begin
	if Assigned(FOnTextInput) then
		Result := FOnTextInput(Self, Key)
	else
		Result := False;
end;

function TCWEControl.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Assigned(FOnMouseDown) then
		Result := FOnMouseDown(Self, Button, X, Y, P)
	else
		Result := False;
end;

function TCWEControl.MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Assigned(FOnMouseUp) then
		Result := FOnMouseUp(Self, Button, X, Y, P)
	else
		Result := False;
end;

function TCWEControl.MouseEnter: Boolean;
begin
	if Assigned(FOnMouseEnter) then
		Result := FOnMouseEnter(Self)
	else
		Result := False;
end;

function TCWEControl.MouseLeave: Boolean;
begin
	if Assigned(FOnMouseLeave) then
		Result := FOnMouseLeave(Self)
	else
		Result := False;

	MouseCoords := Point(-1, -1);
end;

procedure TCWEControl.MouseMove(X, Y: Integer; P: TPoint);
begin
	X := X - Rect.Left * Console.Font.Width;
	Y := Y - Rect.Top  * Console.Font.Height;

	MouseCoords := Point(X, Y);

	if Assigned(FOnMouseMove) then
		FOnMouseMove(Self, X, Y, P);
end;

function TCWEControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
var
	Key: Integer;
begin
	if WheelDelta = 0 then Exit(False);

	if Assigned(FOnMouseWheel) then
	begin
		Result := FOnMouseWheel(Self, Shift, (WheelDelta < 0), P);
		Exit;
	end;

	if WheelDelta < 0 then
		Key := SDLK_DOWN
	else
	if WheelDelta > 0 then
		Key := SDLK_UP
	else
		Exit(False);

	Result := KeyDown(Key, Shift);
end;

// forces control to paint on any screen and position
// (will not paint subcontrols on nonactive screens!)
procedure TCWEControl.PaintTo(R: TRect);
var
	OrigScreen: TCWEScreen;
	OrigRect: TRect;
	OrigHeight: Word;
begin
	OrigScreen := Screen;
	OrigRect := Rect;
	OrigHeight := Height;

	Rect := R; //Bounds(X, Y, RectWidth(OrigRect), RectHeight(OrigRect));
	Height := R.Bottom - R.Top - 1;
	Screen := CurrentScreen;

	Paint;

	Screen := OrigScreen;
	Rect := OrigRect;
	Height := OrigHeight;
end;

procedure TCWEControl.Protect;
begin
	IsProtected := True;
end;

procedure TCWEControl.SetBorder(Enabled, Fat, Sunken, Pix: Boolean);
begin
	Border.Enabled := Enabled;
	Border.Fat     := Fat;
	Border.Sunken  := Sunken;
	Border.Pixel   := Pix;
end;

procedure TCWEControl.SetData(Index: Integer; Val: Byte; const Text: AnsiString);
begin
	if (Data = nil) then
	begin
		Index := 0;
		SetLength(Data, 1);
	end
	else
	if (Index < 0) then
		Index := High(Data) + 1;

	if Index > High(Data) then
		SetLength(Data, Index + 1);

	Data[Index].Value := Val;
	if Text <> '' then
		Data[Index].Text := Text;
end;

procedure TCWEControl.SetTimerCallback(cbCallback: TCWENotifyEventID;
	cbID: Integer; cbInterval: Word);
begin
	with TimerCallback do
	begin
		Enabled := False;
		Control := Self;
		Counter := 0;
		Interval := cbInterval;
		ID := cbID;
		Callback := cbCallback;
		Enabled := True;
	end;
end;

procedure TCWEControl.DisableTimerCallback;
begin
	with TimerCallback do
	begin
		Enabled := False;
		Callback := nil;
		Control := nil;
		ID := 0;
		Counter := 0;
	end;
end;

procedure TCWEControl.DrawBorder;
begin
	if Border.Enabled then
		DrawBorder(Rect, ColorBack);
end;

procedure TCWEControl.DrawBorder(R: TRect; BgCol: Integer = -1);
begin
	if not Border.Enabled then Exit;

	if Border.Pixel then
		Console.FrameRectPx(R, Border.Sunken, Border.Fat, BgCol)
	else
		Console.FrameRect(R, Border.Sunken, Border.Fat, BgCol);
end;

procedure TCWEControl.Paint;
var
	Ctrl: TCWEControl;
begin
	if (Screen = nil) or (not Screen.Active) then Exit;

	DrawBorder;

	// Paint all owned controls
	if (Controls <> nil) and (Controls.Count > 0) then
		for Ctrl in Controls do
			Ctrl.Paint;
end;

{ TCWEBorder }

constructor TCWEBorder.Create;
begin
	inherited;

	SetBorder(True, True, True, True);
	SetData(0, TConsole.COLOR_3DLIGHT, 'Light color');
	SetData(1, TConsole.COLOR_3DDARK,  'Dark color');

	WantMouse := False;
	WantKeyboard := False;
	WantHover := False;
end;

procedure TCWEBorder.Paint;
var
	Bg: Integer;
begin
	if not Border.Enabled then Exit;

	Bg := ColorBack;
	if Bg > 15 then Bg := -1; // !!! dumb hack

	if Border.Pixel then
		Console.FrameRectPx(Rect, Border.Sunken, Border.Fat, Bg, Data[0].Value, Data[1].Value)
	else
		Console.FrameRect  (Rect, Border.Sunken, Border.Fat, Bg, Data[0].Value, Data[1].Value);
end;

end.
