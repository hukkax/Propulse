unit Screen.Help;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text;

type
	THelpScreen = class(TCWEScreen)
	public
		Memo:		TCWEMemo;

		procedure	Show(Context: AnsiString); reintroduce;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;

var
	Help: THelpScreen;


implementation

uses
	ProTracker.Util,
	Layout;


constructor THelpScreen.Create;
begin
	inherited;

	RegisterScreenLayout(Self, 'HelpViewer');

	Memo := TCWEMemo.Create(Self, '', 'Text View',
		Types.Rect(1, 1, Console.Width-2, Console.Height-1), True);

	Memo.ColorFore := 6;
	ActiveControl := Memo;

	LoadLayout(Self);
end;

procedure THelpScreen.Show(Context: AnsiString);
var
	sl: TStringList;
	S: AnsiString;
begin
	//Log('Help.Show("%s")', [Context]);
	Memo.Lines.Clear;

	sl := TStringList.Create;
	sl.LoadFromFile(GetDataFile('help.txt'));

	for S in sl do
		if Copy(S, 1, 1) <> ';' then
			Memo.Add(S);

	sl.Free;

	if Memo.JumpToSection(Context) < 0 then
		Memo.ScrollTo(0);

	ChangeScreen(TCWEScreen(Self));
end;

end.

