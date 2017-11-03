// ==========================================================================
// Ask user for a value: numeric with a slider, or select from a list
// ==========================================================================
unit Dialog.ValueQuery;

interface

uses
	Types, Classes,
	CWE.Core,
	CWE.Dialogs;

	procedure AskString(ID: Word; const Caption: AnsiString; const Text: AnsiString;
		NumbersOnly: Boolean;
		Callback: TButtonClickedEvent);
	procedure AskValue(ID: Word; const Caption: AnsiString; Min, Max, Pos: Integer;
		Callback: TButtonClickedEvent);
	procedure AskList(ID: Word; const Caption: AnsiString; Index: Word;
		const Items: array of AnsiString;
		Callback: TButtonClickedEvent);

	function GetAskedValue(var V: Integer): Boolean;


implementation

uses
	SysUtils,
	CWE.Widgets.Numeric,
	CWE.Widgets.Text;

procedure AskString;
const
	W = 21;
	H = 8;
var
	Dlg: TCWEScreen;
	Edit: TCWEEdit;
begin
	Dlg := ModalDialog.CreateDialog(ID, Bounds(
		(Console.Width div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 Caption, False);

	Edit := TCWEEdit.Create(Dlg, '', 'Edit',
		Types.Rect(1, 3, W-1, 4), True);
	Edit.SetCaption(Text);
	Edit.SetBorder(True, False, True, True);
	if NumbersOnly then
		Edit.AllowedChars := AllowedNumberKeys;

	with ModalDialog do
	begin
		AddResultButton(btnOK, 'OK', 1, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-9, H-2);
		ButtonCallback := Callback;
		Dialog.ActivateControl(Edit);
		Show;
	end;
end;

procedure AskValue;
const
	W = 34;
	H = 6;
var
	Dlg: TCWEScreen;
	Slider: TCWESlider;
	Edit: TCWENumericEdit;
	L: Integer;
begin
	L := Length(IntToStr(Max));

	Dlg := ModalDialog.CreateDialog(ID, Bounds(
		(Console.Width div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 Caption, False);

	Slider := TCWESlider.Create(Dlg, '', 'Slider',
		Types.Rect(1, 2, W-2-L, 3), True);
	Slider.Min := Min;
	Slider.Max := Max;
	Slider.Position := Pos;
	//Slider.CreateLabelControl;

	Edit := TCWENumericEdit.Create(Dlg, '', 'Edit',
		Types.Rect(W-1-L, 2, W-1, 3), True);
	Edit.Min := Min;
	Edit.Max := Max;
	Edit.Position := Pos;
	Edit.SetBorder(True, False, True, True);

	Slider.EditCtrl := Edit;
	Edit.SliderCtrl := Slider;

	with ModalDialog do
	begin
		AddResultButton(btnOK, 'OK', 1, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-9, H-2);
		ButtonCallback := Callback;
		Dialog.ActivateControl(Slider);
		Show;
	end;
end;

function GetAskedValue(var V: Integer): Boolean;
var
	ctrl: TCWEControl;
begin
	Result := False;
	if (ModalDialog = nil) or (ModalDialog.Dialog = nil) then Exit;

	ctrl := ModalDialog.Dialog.FindControl('Slider'); // ugh
	if ctrl <> nil then
	begin
		V := TCWESlider(ctrl).Position;
		Result := True;
	end;
end;

procedure AskList;
const
	Margin = 2;
var
	W, H, LW, LH: Integer;
	Dlg: TCWEScreen;
	List: TCWEList;
begin
	LW := 10;
	for H := 0 to High(Items) do
		if Length(Items[H]) > LW then
			LW := Length(Items[H]);
	W := LW + (Margin*2);

	// size list to fit its contents and dialog to fit list
	LH := High(Items)+1;
	H := LH + 7;
	if H > (Console.Height - 2) then
		H := Console.Height - 2;

	Dlg := ModalDialog.CreateDialog(ID, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 Caption);

	List := TCWEList.Create(Dlg, '', 'List', Bounds(Margin, 3, LW, LH), True);
	List.CanCloseDialog := True;
	List.Scrollbar.Visible := (List.Items.Count > List.Height);

	for LH := 0 to High(Items) do
		List.AddItem(Items[LH]);
	if Index in [0..List.Items.Count-1] then
		List.ItemIndex := Index;

	with ModalDialog do
	begin
		AddResultButton(btnOK, 'OK', Margin, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-6-Margin-2, H-2);
		ButtonCallback := Callback;
		Dialog.ActivateControl(List);
		Show;
	end;
end;

end.

