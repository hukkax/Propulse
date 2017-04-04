unit Layout;

interface

uses
	CWE.Core, Types, Generics.Collections;

type
	TControlKind = (
		CTRLKIND_BOX,
		CTRLKIND_LABEL,
		CTRLKIND_BUTTON,
		CTRLKIND_BORDER_TOP,
		CTRLKIND_BORDER_BOTTOM,
		CTRLKIND_BORDER_LEFT,
		CTRLKIND_BORDER_RIGHT,
		CTRLKIND_BORDER_NW,
		CTRLKIND_BORDER_NE,
		CTRLKIND_BORDER_SW,
		CTRLKIND_BORDER_SE
	);

	TControlInfo = class
		ID,
		Caption:	String;
		Kind: 		TControlKind;
		Rect:		TRect;
		ResizableX,
		ResizableY:	Boolean;
		ColorFore,
		ColorBack:	ShortInt;
		Alignment:	Byte;
		Border:		TControlBorder;
		Control:	TCWEControl;
		MinSize: 	TPoint;
	end;

	TRegisteredControl = class
	private
		Kind: TControlKind;
	public
		Control: TCWEControl;
		Screen: TCWEScreen;

		ID: String;

		AllowDelete,
		AllowResizeX,
		AllowResizeY: Boolean;
		MinSize: TPoint;

		procedure GetControlInfo(var Info: TControlInfo);
		procedure ApplyControlInfo(const Info: TControlInfo);
	end;

const
	ControlKindText: array[TControlKind] of String = (
		'Box',				'Text',			'Button',
		'Border:Top',		'Border:Bottom',
		'Border:Left',		'Border:Right',
		'Border:NW Corner',	'Border:NE Corner',
		'Border:SW Corner',	'Border:SE Corner'
	);

var
	RegisteredControls: TObjectList<TRegisteredControl>;
	RegisteredScreens:  TObjectList<TCWEScreen>;

	procedure RegisterScreenLayout(const Screen: TCWEScreen; const ID: AnsiString);
	//procedure LoadLayout(const ID: AnsiString);
	procedure LoadLayout(const Screen: TCWEScreen);

	function RegisterLayoutControl(const Ctrl: TCWEControl;
		CtrlKind: TControlKind = CTRLKIND_BOX;
		InitMinSize: Boolean = False;
		ResizeX: Boolean = True; ResizeY: Boolean = True): TRegisteredControl;
	function RegisterLayoutControlClass(const Ctrl: TCWEControl;
		CtrlClass: TCWEControlClass;
		CtrlKind: TControlKind = CTRLKIND_BOX;
		InitMinSize: Boolean = False;
		ResizeX: Boolean = True; ResizeY: Boolean = True): Integer;
	function IsControlRegistered(const Ctrl: TCWEControl): Boolean;
	function FindRegisteredControl(const Ctrl: TCWEControl): TRegisteredControl;
	function Parse(const S: AnsiString): AnsiString;


implementation


uses
	Classes,
	IniFiles,
	SysUtils,
	ShortcutManager,
	ProTracker.Util,
	CWE.Widgets.Text;

var
	CurrScreen: TCWEScreen;


function StringToControlKind(const S: String): Integer;
var
	i: Integer;
	ss, cn: String;
begin
	ss := LowerCase(Trim(S));
	i := 0;
	for cn in ControlKindText do
	begin
		if LowerCase(cn) = ss then
			Exit(i);
		Inc(i);
	end;
	Result := -1;
end;

function Parse(const S: AnsiString): AnsiString;
var
	x1, x2: Integer;
	ps: AnsiString;
begin
	// %sc:SHORTCUT%
	x1 := Pos('%sc:', S);
	if x1 < 1 then
		Exit(S)
	else
	begin
		x2 := x1 + 4;
		while (x2 <= Length(S)) and (S[x2] <> '%') do
		begin
			ps := ps + S[x2];
			Inc(x2);
		end;
		if x1 > 1 then
			Result := Copy(S, 1, x1-1)
		else
			Result := '';
		ps := Shortcuts.GetShortcut('', ps);
		Result := Result + ps + Copy(S, x2+1, MaxInt);
	end;
end;

//procedure LoadLayout(const ID: AnsiString);
procedure LoadLayout(const Screen: TCWEScreen);
var
	Sect: String;
	Ini: TIniFile;

	function FixStr(const S: AnsiString): AnsiString; inline;
	begin
		Result := Parse(StringReplace(S, '\"', '"', [rfReplaceAll]));
	end;

	function ReadCoord(const ID: AnsiString; DefaultFrom: Integer): Integer;
	var
		S: String;
	begin
		S := Ini.ReadString(Sect, ID, IntToStr(DefaultFrom));
		if S[1] = '+' then
			Result := DefaultFrom + StrToInt(Copy(S, 2, MaxInt))
		else
		if S[1] = '-' then
			Result := DefaultFrom - StrToInt(Copy(S, 2, MaxInt))
		else
			Result := StrToInt(S);
	end;


var
	slSections: TStringList;
	i: Integer;
	Ctrl: TCWEControl;
	S: String;
	CtrlKind: TControlKind;
	CtrlCreated: Boolean;
	PrevRect: TRect;
	KB: TKeyBinding;
begin
//	Log('Load layout: "%s"', [Screen.ID]);

	if Screen = nil then
	begin
		Log(TEXT_ERROR + 'Screen is nil!');
		Exit;
	end;

	if Screen.ID = '' then
	begin
		Log(TEXT_WARNING + 'Empty ID!');
		Exit;
	end;

	S := DataPath + 'layout/' + Screen.ID + '.layout';

	if not FileExists(S) then
	begin
//		writeln(TEXT_WARNING + 'File not found: ' + ExtractFileName(S));
		Exit;
	end;

	{for i := 0 to Screens.Count-1 do
	begin
		Log(' > "'+screens[i].ID+'"');
		if Screens[i].ID = ID then
		begin
			Screen := Screens[i];
			Break;
		end;
	end;}

	Ini := TIniFile.Create(S);
	slSections := TStringList.Create;
	Ini.ReadSections(slSections);
	CtrlCreated := False;

	for Sect in slSections do
	begin
		Ctrl := nil;

		S := Ini.ReadString(Sect, 'Type', '');
		if S = '' then
		begin
			Log(TEXT_WARNING + '');
			Continue;
		end;

		i := StringToControlKind(S);
		if i < 0 then
		begin
			Log(TEXT_WARNING + '');
			Continue;
		end;

		CtrlKind := TControlKind(i);

		S := Ini.ReadString(Sect, 'ID', '');

		if S = '' then
		begin
			// Create new control
			case CtrlKind of

				CTRLKIND_BOX:
					Ctrl := TCWEBorder.Create(Screen,
						'', '', Types.Rect(1, 1, 2, 2));

				CTRLKIND_LABEL:
					Ctrl := TCWELabel.Create(Screen,
						'', '', Types.Rect(1, 1, 2, 2));

				CTRLKIND_BUTTON:
				begin
					Ctrl := TCWEButton.Create(Screen,
						'', '', Types.Rect(1, 1, 2, 2));
					Ctrl.WantKeyboard := False;
				end;

				CTRLKIND_BORDER_TOP,
				CTRLKIND_BORDER_BOTTOM,
				CTRLKIND_BORDER_LEFT,
				CTRLKIND_BORDER_RIGHT,
				CTRLKIND_BORDER_NW,
				CTRLKIND_BORDER_NE,
				CTRLKIND_BORDER_SW,
				CTRLKIND_BORDER_SE:
					Continue;

			end;

			CtrlCreated := (Ctrl <> nil);
		end
		else
		begin
			// Modify existing registered control
			CtrlCreated := False;
			i := 0;
			S := FixStr(S);
			for Ctrl in Screen.AllControls do
			begin
				//Log(' <'+ctrl.ID+'>');
				if Ctrl.ID = S then
				begin
					i := 1;
					Break;
				end;
			end;
			if i = 0 then
			begin
				Log(TEXT_ERROR + 'Cannot find control to bind: <%s>!', [S]);
				Continue;
			end;
		end;

		if Ctrl = nil then
			Continue;

		S := Ini.ReadString(Sect, 'Caption', '');
		if S <> '' then
			Ctrl.Caption := FixStr(S);

		Ctrl.Rect.Left   := ReadCoord('X1', PrevRect.Left);
		//Ini.ReadInteger(Sect, 'X1', Ctrl.Rect.Left);
		Ctrl.Rect.Top    := ReadCoord('Y1', PrevRect.Top);
		//Ini.ReadInteger(Sect, 'Y1', Ctrl.Rect.Top);
		Ctrl.Rect.Right  := ReadCoord('X2', PrevRect.Right);
		//Ini.ReadInteger(Sect, 'X2', Ctrl.Rect.Right);
		Ctrl.Rect.Bottom := ReadCoord('Y2', Ctrl.Rect.Top + 1);
		//Ini.ReadInteger(Sect, 'Y2', Ctrl.Rect.Top + 1);

		Ctrl.SetBounds(Ctrl.Rect);

		PrevRect.Left   := Ctrl.Rect.Left;
		PrevRect.Right  := Ctrl.Rect.Right;
		PrevRect.Top    := Ctrl.Rect.Top;
		PrevRect.Bottom := Ctrl.Rect.Bottom;

		Ctrl.ColorFore := Ini.ReadInteger(Sect, 'ColorText', Ctrl.ColorFore);
		Ctrl.ColorBack := Ini.ReadInteger(Sect, 'ColorBack', Ctrl.ColorBack);

		case CtrlKind of

			CTRLKIND_LABEL:
				TCWELabel(Ctrl).Alignment := Ini.ReadInteger(
					Sect, 'TextAlign', TCWELabel(Ctrl).Alignment);

			CTRLKIND_BUTTON:
			begin
				S := Ini.ReadString(Sect, 'Shortcut', '');
				if Pos('#', S) = 1 then
				begin
					KB := Shortcuts.FindByName(Copy(S, 2, Length(S)));
					if KB <> nil then
						TCWEButton(Ctrl).Shortcut := KB.Shortcut
					else
						Log(TEXT_WARNING + 'Layout %S: Invalid binding ' +
							'"%s" for button "%s"', [Screen.ID, S, Ctrl.Caption]);
				end
				else
					TCWEButton(Ctrl).Shortcut := TextToShortcut(S);
			end;

		end;

		S := LowerCase(Ini.ReadString(Sect, 'Border', ''));
		Ctrl.Border.Enabled := (S <> '');
		if Ctrl.Border.Enabled then
		begin
			Ctrl.Border.Fat    := (Pos('fat',    S) > 0);
			Ctrl.Border.Sunken := (Pos('sunken', S) > 0);
			Ctrl.Border.Pixel  := (Pos('pixel',  S) > 0);
		end;

		S := Trim(Ini.ReadString(Sect, 'Data', ''));
		if S <> '' then
		begin
			S := StringReplace(S, ' ', '', [rfReplaceAll]);
			for i := 1 to Length(S) div 2 do
			begin
				if (i-1) > High(Ctrl.Data) then Break;
				Ctrl.Data[i-1].Value := StrToInt('$' + Copy(S, i*2-1, 2));
			end;
		end;

		if CtrlCreated then
			RegisterLayoutControl(Ctrl, CtrlKind, False, True, True);
	end;

	slSections.Free;
	Ini.Free;
	Log('Layout loaded: %s', [Screen.ID]);
end;

procedure RegisterScreenLayout;
begin
	CurrScreen := Screen;
	Screen.ID := ID;
	if not RegisteredScreens.Contains(Screen) then
		RegisteredScreens.Add(Screen);
end;

function IsControlRegistered(const Ctrl: TCWEControl): Boolean;
var
	RC: TRegisteredControl;
begin
	for RC in RegisteredControls do
		if RC.Control = Ctrl then Exit(True);
	Result := False;
end;

function RegisterLayoutControl;
var
	RC: TRegisteredControl;
	S: String;
begin
	if IsControlRegistered(Ctrl) then Exit(nil);

	RC := TRegisteredControl.Create;

	RC.Control := Ctrl;
	RC.Screen := CurrScreen;

{	if Ctrl is TCWELabel 	then S := 'Label'
	else
	if Ctrl is TCWEButton 	then S := 'Button'
	else
	if Ctrl is TCWEEdit 	then S := 'Edit'
	else
	if Ctrl is TCWEList 	then S := 'List'
	else
	if Ctrl is TCWEMemo 	then S := 'Memo'
	else
}		S := '';

	RC.AllowDelete := not Ctrl.IsProtected;

	if Ctrl.ID = '' then
	begin
		if Ctrl.IsProtected then
			Ctrl.ID := '"' + Ctrl.Caption + '"';
	end;

	{if S <> '' then
		Ctrl.ID := S + ': ' + Ctrl.ID;}

	RC.ID := Ctrl.ID;
//	RC.ID := ReplaceText(Ctrl.ClassName, 'TCWE', '') + ':' + Ctrl.ID;

	RC.Kind := CtrlKind;

	if InitMinSize then
		RC.MinSize := Types.Point(RectWidth(Ctrl.Rect), RectHeight(Ctrl.Rect))
	else
		RC.MinSize := Types.Point(-1, -1);

	RC.AllowResizeX := ResizeX;
	if not ResizeX then
		RC.MinSize.X := RectWidth(Ctrl.Rect);

	RC.AllowResizeY := ResizeY;
	if not ResizeY then
		RC.MinSize.Y := RectHeight(Ctrl.Rect);

	RegisteredControls.Add(RC);
	Result := RC;
end;

function RegisterLayoutControlClass;
var
	C: TCWEControl;
begin
	Result := 0;
	for C in Ctrl.Controls do
		if C is CtrlClass then
			if RegisterLayoutControl(C,
				CtrlKind, InitMinSize, ResizeX, ResizeY) <> nil then
					Inc(Result);
end;

function FindRegisteredControl(const Ctrl: TCWEControl): TRegisteredControl;
var
	RC: TRegisteredControl;
begin
	for RC in RegisteredControls do
		if RC.Control = Ctrl then
			Exit(RC);
	Result := nil;
end;

procedure TRegisteredControl.GetControlInfo(var Info: TControlInfo);
begin
	Info.Control := Control;
	Info.Kind := Kind;
	Info.ID := ID;
	Info.Caption := Control.Caption;
	Info.Rect := Control.Rect;

	Info.ColorFore := Control.ColorFore;
	Info.ColorBack := Control.ColorBack;
	Info.Border := Control.Border;

	if (Control is TCWELabel) then
		Info.Alignment := (Control as TCWELabel).Alignment;

	Info.ResizableX := AllowResizeX;
	Info.ResizableY := AllowResizeY;
	if not AllowResizeX then
		Info.Rect.Right := Info.Rect.Left + MinSize.X;
	if not AllowResizeY then
		Info.Rect.Bottom := Info.Rect.Top + MinSize.Y;
	Info.MinSize := MinSize;
end;

procedure TRegisteredControl.ApplyControlInfo(const Info: TControlInfo);
begin
	if Control = nil then Exit;

	Control.Caption := Info.Caption;
	Control.Rect := Info.Rect;
	Control.ColorFore := Info.ColorFore;
	Control.ColorBack := Info.ColorBack;
	Control.Border := Info.Border;

	if Control is TCWELabel then
		(Control as TCWELabel).Alignment := Info.Alignment;
end;

initialization

	RegisteredControls := TObjectList<TRegisteredControl>.Create(True);
	RegisteredScreens  := TObjectList<TCWEScreen>.Create(False);

finalization

	RegisteredControls.Free;
	RegisteredScreens.Free;

end.
