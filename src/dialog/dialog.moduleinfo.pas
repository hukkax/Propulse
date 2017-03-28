// ==========================================================================
// Display module playback length and data sizes
// ==========================================================================
unit Dialog.ModuleInfo;

interface

uses
	Types, Classes,
	CWE.Core,
	CWE.Dialogs;

	procedure Dialog_ModuleInfo;


implementation

uses
	SysUtils,
	CWE.Widgets.Text,
	ProTracker.Util,
	ProTracker.Player,
	MainWindow;


procedure Dialog_ModuleInfo;
var
	Dlg: TCWEScreen;
	SizeFmt: String;
	sizeSamp, sizePatt: Cardinal;
	List: TCWETwoColumnList;
const
	W = 27;
	H = 12-2;
begin
	Dlg := ModalDialog.CreateDialog(0, Bounds(
		(Console.Width div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'Module Info');

	List := TCWETwoColumnList.Create(Dlg, '', 'Module Info',
		Types.Rect(1, 2, W-1, H-3), True);
	List.ColumnWidth[1] := 14;
	List.Scrollbar.Visible := False;
	List.Border.Pixel := True;

	sizeSamp := 0;
	for sizePatt := 0 to Module.Samples.Count-1 do
		Inc(sizeSamp, Module.Samples[sizePatt].Length * 2);
	sizePatt := 1084 + ((Module.Info.PatternCount + 1) * 1024);

	if Options.Display.SizesAsDecimal then
		SizeFmt := '%7d (%dK)'
	else
		SizeFmt := '%7x (%dK)';

	with ModalDialog do
	begin
		List.Items.Add(TCWEListItem.Create('Total song time: ' +
			FormatDateTime('hh:nn:ss', Module.GetLength / SecsPerDay) ));
		List.Items.Add(TCWEListItem.Create('', LISTITEM_HEADER));
		List.Items.Add(TCWEListItem.Create(
			Format('Song size:' + COLUMNSEPARATOR + SizeFmt,
			[sizePatt, Round( sizePatt / 1024 )]) ));
		List.Items.Add(TCWEListItem.Create(
			Format('Samples:' + COLUMNSEPARATOR + SizeFmt,
			[sizeSamp, Round( sizeSamp / 1024 )]) ));
		List.Items.Add(TCWEListItem.Create(
			Format('Total:' + COLUMNSEPARATOR + SizeFmt,
			[sizePatt + sizeSamp, Round( (sizePatt + sizeSamp) / 1024 )]) ));

		AddResultButton(btnOK, 'OK', W div 2 - 3, H-2, True);
		ButtonCallback := Window.DialogCallback;
		Show;
	end;
end;


end.

