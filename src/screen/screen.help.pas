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
	S, PrevContext: AnsiString;
	C: AnsiChar;
	i, LinesInSection, Col: Integer;
	ContextOffset: Integer;
	Center: Boolean;
begin
	Memo.Lines.Clear;

	sl := TStringList.Create;
	sl.LoadFromFile(GetDataFile('help.txt'));

	{	<c>   = Center line
		<hr>  % Horizontal line
		<h1>  [ Center and draw box around line
		<h2>  * White text
		<h3>  # Red text
		<fX>    Set text color (X=0..F)
		<bX>    Set background color (X=0..F)
		<#X>    Create anchor
		<@X>    Link to anchor or URL
		      § Normal text (=Implemented in Propulse)
		;     | Hidden text (=Unimplemented)
	}

	Context := '@' + Context;
	PrevContext := '';
	ContextOffset := -1;
	LinesInSection := 0;

	for i := 0 to sl.Count-1 do
	begin
		S := sl[i];

		if Copy(S, 1, 1) = ';' then Continue;

		(*if S = '' then
		begin
			if ContextOffset >= 0 then
			begin
				Memo.Add('');
				Inc(LinesInSection);
			end;
			Continue;
		end;

		if S[1] = '@' then
		begin
			if (PrevContext = Context) and (LinesInSection in [1..Memo.Height-1]) then
			for Col := Memo.Height downto LinesInSection do
				Memo.Add('');

			PrevContext := '-';

			if (S = Context) or (S = '@') then
			begin
				ContextOffset := Memo.Lines.Count;
				if S = Context then
					PrevContext := S;
			end;

			LinesInSection := 0;
			Continue;
		end
		else
		if ContextOffset < 0 then Continue;

		Center := False;
		Col := -1;
		C := S[1];

		if Pos(C, ' |!:;+') > 0 then
			Continue;

		if C = '#' then
			Col := 4
		else
		if C = '*' then
			Col := 3
		else
		if C = '%' then
			S := '  ' + StringOfChar(#154, Memo.Rect.Right-Memo.Rect.Left-2)
		else
		if C = '=' then
		begin
			Center := True;
		end
		else
		if C = '[' then
		begin
			Col := 11;
			S := #131 + ' ' + Copy(S, 2, Length(S)) + ' ' + #132;
			Memo.Add(#128 + StringOfChar(#129, Length(S)-2) + #130, Col, True);
			Memo.Add(S, Col, True);
			Memo.Add(#133 + StringOfChar(#134, Length(S)-2) + #135, Col, True);
			Inc(LinesInSection, 3);
			Continue;
		end;

		Memo.Add(Copy(S, 2, Length(S)), Col, Center);
		Inc(LinesInSection);
		*)

		Memo.Add(S);
	end;

	sl.Free;

	if ContextOffset >= 0 then
		Memo.ScrollTo(ContextOffset);
	ChangeScreen(TCWEScreen(Self));
end;

end.

