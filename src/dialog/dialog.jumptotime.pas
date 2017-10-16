// ==========================================================================
// Jump to a specific time in module
// ==========================================================================
unit Dialog.JumpToTime;

interface

uses
	Types, Classes,
	CWE.Core, CWE.Dialogs;

type
	TJumpToTimeDialog = class
	public
		Minutes,
		Seconds:	Byte;
		procedure DialogCallback(ID: Word; Button: TDialogButton;
			Tag: Integer; Data: Variant; Dlg: TCWEDialog);
	end;

	procedure Dialog_JumpToTime;


implementation

uses
	Screen.Config, Screen.Editor,
	ProTracker.Player;

var
	JumpToTimeDialog: TJumpToTimeDialog;


procedure TJumpToTimeDialog.DialogCallback(ID: Word; Button: TDialogButton;
	Tag: Integer; Data: Variant; Dlg: TCWEDialog);
begin
	if (Dlg = nil) and (Button = btnOK) then
	with JumpToTimeDialog do
	begin
		Module.JumpToTime(Minutes, Seconds);
		Editor.SeekTo(Module.PlayPos.Order, Module.PlayPos.Row);
	end;
end;

procedure Dialog_JumpToTime;
const
	W = 20;
	H = 7;
var
	Dlg: TCWEScreen;
	List: TCWEConfigList;
	Sect: AnsiString;
begin
	Dlg := ModalDialog.CreateDialog(0, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'Jump To Time', True);

	List := TCWEConfigList.Create(Dlg, '', 'Jumplist',
		Types.Rect(1, 2, W-1, H-3), True);
	List.ColumnWidth[1] := 8;
	List.Border.Pixel := True;
	List.Scrollbar.Visible := False;

	with ModalDialog do
	begin
		CreateConfigManager;
		Sect := 'Time';

		ConfigManager.AddByte(Sect, '',
			@JumpToTimeDialog.Minutes, JumpToTimeDialog.Minutes).
			SetInfo('Minutes', 0, 99, []);
		ConfigManager.AddByte(Sect, '',
			@JumpToTimeDialog.Seconds, JumpToTimeDialog.Seconds).
			SetInfo('Seconds', 0, 59, []);

		List.Init(ConfigManager);

		AddResultButton(btnOK,     'OK',     1, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-9, H-2);
		ButtonCallback := JumpToTimeDialog.DialogCallback;

		Dialog.ActiveControl := List;
		Show;
	end;
end;

initialization

	JumpToTimeDialog := TJumpToTimeDialog.Create;

finalization

	JumpToTimeDialog.Free;

end.

