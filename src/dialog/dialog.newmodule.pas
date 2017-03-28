// ==========================================================================
// Initialize a new module
// ==========================================================================
unit Dialog.NewModule;

interface

uses
	Types, Classes, CWE.Core, CWE.Dialogs;

const
	CLEAR_PATTERNS = 0;
	CLEAR_SAMPLES  = 1;
	CLEAR_ORDERS   = 2;


	procedure NewModule(DoDialog: Boolean);
	procedure Dialog_NewModule;


implementation

uses
	MainWindow, SysUtils,
	ProTracker.Util,
	ProTracker.Player,
	ProTracker.Sample,
	ProTracker.Editor,
	Screen.Editor,
	Screen.Config;


procedure NewModule(DoDialog: Boolean);
var
	p, ch, y: Integer;
begin
	if DoDialog then
		Dialog_NewModule
	else
	begin
		Module.Stop;

		if DialogBooleans[CLEAR_PATTERNS] then
		begin
			Module.Pause(True);
			for p := 0 to MAX_PATTERNS do
				for ch := 0 to AMOUNT_CHANNELS-1 do
					for y := 0 to 63 do
						Module.Notes[p, ch, y] := EmptyNote;
			Module.Info.PatternCount := 1;
			Module.Info.Tempo := 6;
			Module.Info.BPM := 125;
			Module.CurrentSpeed := Module.Info.Tempo;
			Module.CurrentBPM := Module.Info.BPM;
			Module.Info.Filename := '';
			CurrentPattern := 0;
		end;

		if DialogBooleans[CLEAR_SAMPLES] then
		begin
			Module.Pause(True);
			Module.Samples.Clear;
			for p := 0 to 31 do
				Module.Samples.Add(TSample.Create);
		end;

		if DialogBooleans[CLEAR_ORDERS] then
		begin
			for p := 0 to 127 do
				Module.OrderList[p] := 0;
			Module.Info.OrderCount := 1;
			Module.Info.RestartPos := 0;
		end;

		with Module do
		begin
			Reset;
			Info.Title := '';
			Info.Filesize := 0;
			Pause(False);
		end;

		Editor.Reset;
		Editor.ActiveControl := PatternEditor;
	end;
end;

procedure Dialog_NewModule;
const
	W = 22;
	H = 10;
var
	Dlg: TCWEScreen;
	List: TCWEConfigList;
begin
	Dlg := ModalDialog.CreateDialog(ACTION_NEWMODULE, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'New Module');

	List := TCWEConfigList.Create(Dlg, '', 'New',
		Types.Rect(2, 3, W-2, H-4), True);
	List.ColumnWidth[1] := 6;
	List.Scrollbar.Visible := False;

	with ModalDialog do
	begin
		CreateConfigManager;

		ConfigManager.AddBoolean('New', '',
			@DialogBooleans[CLEAR_PATTERNS], True).
			SetInfo('Patterns', 0, 1, ['Keep', 'Clear']);
		ConfigManager.AddBoolean('New', '',
			@DialogBooleans[CLEAR_SAMPLES], True).
			SetInfo('Samples', 0, 1, ['Keep', 'Clear']);
		ConfigManager.AddBoolean('New', '',
			@DialogBooleans[CLEAR_ORDERS], True).
			SetInfo('Orderlist', 0, 1, ['Keep', 'Clear']);

		List.Init(ConfigManager);

		AddResultButton(btnOK,     'OK',     2, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-10, H-2);
		ButtonCallback := Window.DialogCallback;

		Dialog.ActiveControl := List;
		Show;
	end;
end;

end.

