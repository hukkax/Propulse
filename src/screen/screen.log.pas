unit Screen.Log;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text;

type

	{ TLogScreen }

 TLogScreen = class(TCWEScreen)
	private
	public
		Memo:		TCWEMemo;

		procedure Log(const Msg: AnsiString);

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;

var
	LogScreen: TLogScreen;


implementation


uses
	Layout, ProTracker.Util, SysUtils;


procedure TLogScreen.Log(const Msg: AnsiString);
begin
	if Copy(Msg, 1, 1) = '$' then
		Memo.Add(' ' + Copy(Msg, 3, Length(Msg)), StrToInt(Copy(Msg, 1, 2)))
	else
		Memo.Add(' ' + Msg);
	Paint;
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
