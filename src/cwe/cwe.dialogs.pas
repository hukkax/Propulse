unit CWE.Dialogs;

interface

uses
	Classes, Types, SysUtils,
	TextMode, CWE.Core, CWE.Widgets.Text,
	ConfigurationManager;

const
	DIALOG_CONTEXTMENU = 666;

type
	TDialogButton = ( btnCancel = 0, btnOK, btnYes, btnNo );
	TDialogButtons = set of TDialogButton;
	TCWEDialog = class;

	TButtonClickedEvent = procedure (ID: Word; ModalResult: TDialogButton; Tag: Integer;
									Data: Variant; Dlg: TCWEDialog) of Object;

	TCWEDialogScreen = class(TCWEScreen)
	public
		function	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		procedure	MouseMove(X, Y: Integer; P: TPoint); override;
	end;

	TCWEDialog = class
	const
		BTN_WIDTH = 8;
	private
		Data: 			Variant;
		Titlebar: 		TCWELabel;
	public
		PreviousScreen:	TCWEScreen;

		Dialog: 		TCWEDialogScreen;
		ButtonCallback:	TButtonClickedEvent;
		ConfigManager:	TConfigurationManager;

		ID: 			Word;
		Dragging:		Boolean;
		DragPos,
		WindowPos:		TPoint;
		PrevControl:	TCWEControl;

		function	TitlebarMouseDown(Sender: TCWEControl;
					Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;

		procedure 	ButtonClickHandler(Sender: TCWEControl);

		procedure 	Show;
		procedure 	Close;

		procedure 	SetBounds(const Bounds: TRect);

		function 	CreateDialog(aID: Word; R: TRect; const Caption: AnsiString;
					Movable: Boolean = True): TCWEDialogScreen;
		function 	CreateConfigManager: TConfigurationManager;
		function 	AddResultButton(Button: TDialogButton; const sCaption: AnsiString;
					X, Y: Integer; Default: Boolean = False): TCWEButton;

		procedure	MessageDialog(aID: Word; const Caption, Text: AnsiString; Buttons: TDialogButtons;
					DefaultButton: TDialogButton; Callback: TButtonClickedEvent;
					MoreData: Variant);
		procedure	ShowMessage(const Caption, Text: AnsiString);
	end;

var
	ModalDialog: 	TCWEDialog;
	DialogBooleans: array[0..9] of Boolean;
	DialogIntegers: array[0..9] of Integer;
	DialogBytes:    array[0..9] of Byte;


implementation

uses
	Math, SDL.Api.Types;


function TCWEDialog.TitlebarMouseDown(Sender: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := True;
	Dragging := True;
	DragPos.X := P.X + Dialog.Rect.Left;
	DragPos.Y := P.Y + Dialog.Rect.Top;
end;

procedure TCWEDialogScreen.MouseMove(X, Y: Integer; P: TPoint);
var
	ctrl: TCWEControl;
	ox, oy: Integer;
begin
	inherited;

	if not ModalDialog.Dragging then
	begin
		if (ActiveControl <> nil) and (ActiveControl.WantKeyboard) then
			ModalDialog.PrevControl := ActiveControl;
		Exit;
	end;

	if (P.X = ModalDialog.DragPos.X) and (P.Y = ModalDialog.DragPos.Y) then Exit;
	if (P.X < 0) or (P.Y < 0) then Exit;

	ox := P.X - ModalDialog.DragPos.X;
	oy := P.Y - ModalDialog.DragPos.Y;

	ModalDialog.DragPos := P;

	OffsetRect(Rect, ox, oy);

	for ctrl in Controls do
		OffsetRect(ctrl.Rect, ox, oy);

	CurrentScreen := ModalDialog.PreviousScreen;
	Console.FillRect(CurrentScreen.Rect, ' ', 15, 2);
	CurrentScreen.Paint;
	CurrentScreen.Show;

	CurrentScreen := Self;
	Console.FillRect(Rect, ' ', 15, 2);
	Paint;
end;

function TCWEDialogScreen.MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := inherited;

	if (Button = mbLeft) and (ModalDialog.Dragging) then
	begin
		ModalDialog.Dragging := False;
		Show;

		if ModalDialog.PrevControl <> nil then
		begin
			ModalDialog.Dialog.ActivateControl(ModalDialog.PrevControl);
			ModalDialog.PrevControl := nil;
		end;

		Result := True;
	end;

	{if (ModalDialog.ID = DIALOG_CONTEXTMENU) and (Button = mbMiddle) and (not Result) then
		ModalDialog.Close;}
end;

function TCWEDialogScreen.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
var
	ctrl: TCWEControl;
	btn: TCWEButton;
begin
	if Key = SDLK_ESCAPE then
	begin
		for ctrl in Controls do
			if (ctrl is TCWEButton) then
			begin
				btn := ctrl as TCWEButton;
				if btn.ModalResult = Ord(btnCancel) then
				begin
					ModalDialog.ButtonClickHandler(ctrl);
					Exit(True);
				end;
			end;
		ModalDialog.Close;
		Exit(True);
	end
	else
	if Key = SDLK_RETURN then
	begin
		if not (ActiveControl is TCWEButton) then
		for ctrl in Controls do
			if (ctrl is TCWEButton) then
			begin
				btn := ctrl as TCWEButton;
				if btn.ModalResult in [Ord(btnOK), Ord(btnYes)] then
				begin
					ModalDialog.ButtonClickHandler(ctrl);
					Exit(True);
				end;
			end;
	end;

	Result := inherited KeyDown(Key, Shift);
	//Result := True;
end;

procedure TCWEDialog.ButtonClickHandler(Sender: TCWEControl);
var
	Btn: TCWEButton;
	Tag: Integer;
	DB: TDialogButton;
begin
	if not (Sender is TCWEButton) then Exit;
	Dialog.ActiveControl := nil;

	Btn := TCWEButton(Sender);
	DB := TDialogButton(Btn.ModalResult);
	Tag := Btn.Tag;

	// first call the handler with the context of the originating dialog
	if Assigned(ButtonCallback) then
		ButtonCallback(ID, DB, Tag, Data, Self);

	Close;

	// call the handler again with the modal dialog now closed
	// so the handler can do dialog-related stuff if it wants
	if Assigned(ButtonCallback) then
		ButtonCallback(ID, DB, Tag, Data, nil);
end;

function TCWEDialog.CreateDialog(aID: Word; R: TRect;
	const Caption: AnsiString; Movable: Boolean = True): TCWEDialogScreen;
begin
	if Dialog <> nil then Exit(nil);

	if not (CurrentScreen is TCWEDialogScreen) then
		PreviousScreen := CurrentScreen;

	Result := TCWEDialogScreen.Create(Console, Caption, Caption);
	Dialog := Result;

	Result.SetBounds(R);
	Result.SetBorder(True, True, False, True);
	ID := aID;

	Titlebar := TCWELabel.Create(Dialog, Caption, 'sCaption',
		Bounds(0, 0, Dialog.Width + 1, 1));
	Titlebar.SetColors(3, 1);
	Titlebar.Alignment := ALIGN_CENTER;
	if Movable then
	begin
		Titlebar.OnMouseDown := TitlebarMouseDown;
		Titlebar.WantMouse := True;
	end;
end;

function TCWEDialog.CreateConfigManager: TConfigurationManager;
begin
	ConfigManager.Free;
	ConfigManager := TConfigurationManager.Create;
	Result := ConfigManager;
end;

procedure TCWEDialog.SetBounds(const Bounds: TRect);
begin
	Dialog.SetBounds(Bounds);
	Titlebar.SetBounds(Types.Bounds(0, 0, Dialog.Width + 1, 1));
end;

procedure TCWEDialog.Show;
begin
	ChangeScreen(TCWEScreen(Dialog));
	if (Dialog.ActiveControl = Dialog) or (Dialog.ActiveControl = nil) then
		Dialog.BrowseControls;
end;

procedure TCWEDialog.Close;
begin
	Dragging := False;
	CurrentScreen := PreviousScreen;
	Dialog.MouseInfo.Control := nil;
	ChangeScreen(PreviousScreen);
	PreviousScreen.MouseInfo.Control := nil;
	FreeAndNil(Dialog);
end;

function TCWEDialog.AddResultButton(Button: TDialogButton; const sCaption: AnsiString;
	X, Y: Integer; Default: Boolean = False): TCWEButton;
begin
	Result := TCWEButton.Create(Dialog, sCaption, 'b' + Trim(sCaption),
			Bounds(X, Y, Max(BTN_WIDTH, Length(sCaption)), 1));
	if Default then
		Dialog.ActiveControl := Result;

	Result.ModalResult := Ord(Button);
	Result.OnChange := ButtonClickHandler;
end;

procedure TCWEDialog.MessageDialog;
var
	X, Y, BW: Integer;
	Lbl: TCWELabel;
	foo: TDialogButton;

	procedure AddButton(Button: TDialogButton; const sCaption: AnsiString);
	begin
		if Button in Buttons then
		begin
			AddResultButton(Button, sCaption, X, Y, (Button = DefaultButton));
			Inc(X, BTN_WIDTH + 2);
		end;
	end;

begin
	if Dialog <> nil then Exit;

	// Calculate total width of all buttons
	BW := 0;
	for foo in Buttons do
		Inc(BW, BTN_WIDTH + 2);

	// Adjust dialog width to accommodate captions and buttons
	X := Max(Length(Caption), Length(Text)) + 2;
	X := Max(X, BW);
	if (X mod 2) <> 0 then Inc(X);

	X := (Console.Width div 2) - (X div 2);
	Y := Console.Height div 2;

	Dialog := CreateDialog(aID, Types.Rect(X, Y-2, Console.Width-X, Y+4), Caption);

	Lbl := TCWELabel.Create(Dialog, Text, 'sText',
		Bounds(0, 2, Dialog.Width+1, 1));
	Lbl.Alignment := ALIGN_CENTER;

	X := (Dialog.Width div 2) - (BW div 2) + 2;
	Y := Dialog.Height - 1;

	AddButton(btnOK,     'OK');
	AddButton(btnYes,    'Yes ');
	AddButton(btnNo,     'No');
	AddButton(btnCancel, 'Cancel');

	Data := MoreData;
	ButtonCallback := Callback;

	Show;
end;

procedure TCWEDialog.ShowMessage(const Caption, Text: AnsiString);
begin
	MessageDialog(0, Caption, Text, [btnOK], btnOK, nil, 0);
end;


initialization

	ModalDialog := TCWEDialog.Create;
	ModalDialog.ConfigManager := nil;
	ModalDialog.Dialog := nil;

finalization

	ModalDialog.Dialog.Free;
	ModalDialog.ConfigManager.Free;
	ModalDialog.Free;

end.
