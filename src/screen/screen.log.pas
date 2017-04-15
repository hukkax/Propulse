unit Screen.Log;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text;

type
	TLogScreen = class(TCWEScreen)
	private
		LastLoggedEmpty: Boolean;
	public
		Memo:		TCWEMemo;

		procedure	Log(const Msg: AnsiString);

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;

var
	LogScreen: TLogScreen;


implementation

uses
	Layout, ProTracker.Util, SysUtils, MainWindow;

procedure TLogScreen.Log(const Msg: AnsiString);
var
	S: AnsiString;
begin
	if Msg = '-' then
	begin
		if not LastLoggedEmpty then
			Memo.Add(StringOfChar(#205, Memo.Width+1), 15);
		LastLoggedEmpty := True;
	end
	else
	if Copy(Msg, 1, 1) = '$' then
	begin
		S := Copy(Msg, 3, Length(Msg));
		if not ((LastLoggedEmpty) and (S = '')) then
			Memo.Add(' ' + S, StrToInt(Copy(Msg, 1, 2)));
	end
	else
	begin
		S := Msg;
		if not ((LastLoggedEmpty) and (S = '')) then
			Memo.Add(' ' + S);
	end;

	LastLoggedEmpty := (S = '');
	if Active then
	begin
		Paint;
		Window.ProcessFrame;
	end;
end;

constructor TLogScreen.Create(var Con: TConsole; const sCaption, sID: AnsiString);
begin
	inherited;

	RegisterScreenLayout(Self, 'MessageLog');

	Memo := TCWEMemo.Create(Self, '', 'Message Log',
		Types.Rect(1, 1, Console.Width-2, Console.Height-1), True);
	RegisterLayoutControl(Memo, CTRLKIND_BOX, False, True, True);

	ActiveControl := Memo;

	LoadLayout(Self);
	OnLog := Self.Log;
end;


end.
