unit Screen.Help;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text;

type
	THelpScreen = class(TCWEScreen)
	private
		Timestamp:	LongInt;
	public
		Memo:		TCWEMemo;

		function 	HelpFileName: String;
		function 	LoadHelp: Boolean;
		procedure	Show(Context: AnsiString); reintroduce;

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;

var
	Help: THelpScreen;


implementation

uses
	SysUtils,
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

	LoadHelp;
end;

function THelpScreen.HelpFileName: String;
begin
	Result := GetDataFile('help.txt');
end;

function THelpScreen.LoadHelp: Boolean;
var
	sl: TStringList;
	S: AnsiString;
	Fn: String;
begin
	Memo.Lines.Clear;

	Fn := HelpFileName;
	Result := FileExists(Fn);

	if Result then
	begin
		Timestamp := FileAge(Fn);
		sl := TStringList.Create;
		sl.LoadFromFile(Fn);
		for S in sl do
			if Copy(S, 1, 1) <> ';' then
				Memo.Add(S);
		sl.Free;
	end
	else
	begin
		Timestamp := 0;
		Memo.Add('<h1>Help file not found!');
	end;
end;

procedure THelpScreen.Show(Context: AnsiString);
var
	Fn: String;
begin
	Fn := HelpFileName;
	if (FileExists(Fn)) and (FileAge(Fn) <> Timestamp) then
		LoadHelp; // reload help file if it's been modified

	if Memo.JumpToSection(Context) < 0 then
		Memo.ScrollTo(0);
	ChangeScreen(TCWEScreen(Self));
end;

end.

