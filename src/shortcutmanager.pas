unit ShortcutManager;

interface

{.$DEFINE DEBUG_KEYS}
{.$DEFINE DEBUG_SHORTCUTS}

uses
	Classes, Menus, Generics.Collections, SDL2;

type
	TShortCut = record
		Key:   Integer;
		Shift: TShiftState;
	end;

	TKeyBinding = class
	public
		ID: 		Word;
		Shortcut:	TShortCut;
		Name: 		String;
		constructor	Create(const aID: Word; const aName: String;
					aShortcut: TShortCut); overload;
	end;

	TKeyBindings = class
	public
		Section: 	String;
		Keys: 		TObjectList<TKeyBinding>;

		function	FindKey(const ID: Word): TKeyBinding; overload;
		function	FindKey(const sKey: String): TKeyBinding; overload;

		constructor Create; overload;
		destructor  Destroy; override;
	end;

	TShortcutManager = class
	private
		CurrentSection: TKeyBindings;
		function 	FindSection(const sSection: String): TKeyBindings;
	public
		Sections:	TObjectList<TKeyBindings>;

		procedure	Load(const Filename: String);
		procedure	Save(const Filename: String);

		function	GetShortcut(const sBindings, sKey: AnsiString;
					InParens: Boolean = False): AnsiString; overload;
		function 	GetShortcut(var Bindings: TKeyBindings; ID: Word;
					InParens: Boolean = False): AnsiString; overload;

		function	Find(var Bindings: TKeyBindings; Key: Integer; Shift: TShiftState): Word;
		function 	FindByName(const aName: String): TKeyBinding;
		function	SetContext(const sSection: String): TKeyBindings;

		function	Bind(const ID: Variant; const Name, KeyString: String): TShortCut; overload;
		function 	Bind(const ID: Variant; const Name: String;
					const Shortcut: TShortCut): TShortCut; overload;
		function 	Bind(const ID: Variant; const Name: String;
					const Shortcuts: array of TShortCut): TShortCut; overload;
		function 	Bind(const ID: Variant; const Name: String;
					const KeyStrings: array of String): TShortCut; overload;

		constructor Create; overload;
		destructor  Destroy; override;
	end;

var
	Shortcuts: TShortcutManager;

	function  ShortCut(Key: Integer; Shift: TShiftState = []): TShortCut;
	function  ShortCutToText(ShortCut: TShortCut): String;
	function  TextToShortCut(const ShortCutText: String): TShortCut;


implementation

uses
	StrUtils, SysUtils, IniFiles,
	ProTracker.Util;


function ShortCut(Key: Integer; Shift: TShiftState = []): TShortCut;
begin
	Result.Key := Key;
	Result.Shift := Shift;
end;

function ShortCutToText(ShortCut: TShortCut): String;
begin
	Result := SDL_GetKeyName(Shortcut.Key);
	if Shortcut.Shift <> [] then
	begin
		if ssShift in Shortcut.Shift then
			Result := 'Shift+' + Result;
		if ssCtrl  in Shortcut.Shift then
			Result := 'Ctrl+' + Result;
		if ssAlt   in Shortcut.Shift then
			Result := 'Alt+' + Result;
		if ssAltGr in Shortcut.Shift then
			Result := 'AltGr+' + Result;
		if ssMeta  in Shortcut.Shift then
			Result := 'Meta+' + Result;
		if ssCaps  in Shortcut.Shift then
			Result := 'Caps+' + Result;
		{if ssNum   in Shortcut.Shift then
			Result := 'NumLock+' + Result;}
	end
	else
		Result := Copy(' ' + Result, 2, MaxInt);
end;

function TextToShortCut(const ShortCutText: String): TShortCut;
var
	S: AnsiString;

	procedure GetShift(ShiftString: AnsiString; ShiftCode: TShiftStateEnum);
	begin
		ShiftString := ShiftString + '+';
		{$IFDEF DEBUG_KEYS}
		Log('  Search Shift "%s" in "%s"', [ShiftString, S]);
		{$ENDIF}
		if Pos(ShiftString, S) > 0 then
		begin
			{$IFDEF DEBUG_KEYS}
			Log('  Got shift: ' + ShiftString);
			{$ENDIF}
			Include(Result.Shift, ShiftCode);
			S := ReplaceStr(S, ShiftString, '');
		end;
	end;

begin
	Result.Shift := [];
	S := ShortCutText;

	if Pos('+', S) > 1 then
	begin
		{$IFDEF DEBUG_KEYS}
		Log('Parsing shortcut: ' + S);
		{$ENDIF}

		GetShift('Shift', 	ssShift);
		GetShift('Ctrl',	ssCtrl);
		GetShift('Alt', 	ssAlt);
		GetShift('AltGr', 	ssAltGr);
		GetShift('Meta',	ssMeta);
		GetShift('Caps', 	ssCaps);
		//GetShift('NumLock',	ssNum);
	end;

	Result.Key := SDL_GetKeyFromName(PAnsiChar(S));

	{$IFDEF DEBUG_KEYS}
	Log('  Key: "%s" => %d', [S, Result.Key]);
	Log('  ToText: ' + ShortCutToText(Result));
	{$ENDIF}
end;

{ TKeyBinding }

constructor TKeyBinding.Create(const aID: Word; const aName: String; aShortcut: TShortCut);
begin
	ID := aID;
	Shortcut.Key   := aShortcut.Key;
	Shortcut.Shift := aShortcut.Shift;
	if aName <> '' then
		Name := aName
	else
		Name := ShortCutToText(Shortcut);
end;

{ TKeyBindings }

constructor TKeyBindings.Create;
begin
	Keys := TObjectList<TKeyBinding>.Create(True);
end;

destructor TKeyBindings.Destroy;
begin
	Keys.Free;
	inherited;
end;

function TKeyBindings.FindKey(const ID: Word): TKeyBinding;
var
	KB: TKeyBinding;
begin
	for KB in Keys do
		if KB.ID = ID then
			Exit(KB);
	Result := nil;
end;

function TKeyBindings.FindKey(const sKey: String): TKeyBinding;
var
	KB: TKeyBinding;
begin
	for KB in Keys do
		if AnsiCompareStr(KB.Name, sKey) = 0 then
			Exit(KB);
	Result := nil;
end;

{ TShortcutManager }

constructor TShortcutManager.Create;
begin
	Sections := TObjectList<TKeyBindings>.Create(True);
	CurrentSection := nil;
end;

destructor TShortcutManager.Destroy;
begin
	Sections.Free;
	inherited;
end;

function TShortcutManager.Find(var Bindings: TKeyBindings;
	Key: Integer; Shift: TShiftState): Word;
var
	Sc: TShortCut;
	Kb: TKeyBinding;
begin
	Result := 0;
	if Bindings = nil then
		Bindings := CurrentSection;

	if Assigned(Bindings.Keys) then
	begin
		Sc := ShortCut(Key, Shift);
		{$IFDEF DEBUG_SHORTCUTS}
		writeln('-------------------------------------');
		writeln('Looking for: ', Sc.Key, shortcuttotext(sc));
		{$ENDIF}
		for Kb in Bindings.Keys do
		begin
			{$IFDEF DEBUG_SHORTCUTS}
			writeln(' * ', kb.Name, ' = ' , Kb.Shortcut.Key, shortcuttotext(kb.shortcut));
			{$ENDIF}
			if (Kb.Shortcut.Key = Sc.Key) and (Kb.Shortcut.Shift = Sc.Shift) then
			begin
				{$IFDEF DEBUG_SHORTCUTS}
				writeln('FOUND!');
				{$ENDIF}
				Exit(Kb.ID);
			end;
		end;
		{$IFDEF DEBUG_SHORTCUTS}
		writeln('not found.');
		{$ENDIF}
	end
	{$IFDEF DEBUG_SHORTCUTS}
	else
		writeln('keys not assigned!');
	{$ENDIF}
end;

function TShortcutManager.GetShortcut(const sBindings, sKey: AnsiString;
	InParens: Boolean = False): AnsiString;
var
	Bindings: TKeyBindings;
	KB: TKeyBinding;
begin
	Result := '';

	if sBindings = '' then
		Bindings := FindSection('Global')
	else
		Bindings := FindSection(sBindings);

	if Bindings <> nil then
	begin
		KB := Bindings.FindKey(sKey);
		if KB <> nil then
		begin
			Result := ShortCutToText(KB.Shortcut);
			if InParens then
				Result := ' (' + Result + ')';
		end;
	end;
end;

function TShortcutManager.GetShortcut(var Bindings: TKeyBindings; ID: Word;
	InParens: Boolean = False): AnsiString;
var
	KB: TKeyBinding;
begin
	Result := '';
	if Bindings <> nil then
	begin
		KB := Bindings.FindKey(ID);
		if KB <> nil then
		begin
			Result := {KB.AsText;} ShortCutToText(KB.Shortcut);
			if InParens then
				Result := ' (' + Result + ')';
		end;
	end;
end;

function TShortcutManager.FindSection(const sSection: String): TKeyBindings;
var
	Sect: TKeyBindings;
begin
	for Sect in Sections do
		if AnsiCompareStr(Sect.Section, sSection) = 0 then
			Exit(Sect);
	Result := nil;
end;

function TShortcutManager.FindByName(const aName: String): TKeyBinding;
var
	Bindings: TKeyBindings;
	KB: TKeyBinding;
begin
	for Bindings in Sections do
	begin
		KB := Bindings.FindKey(aName);
		if KB <> nil then
			Exit(KB);
	end;
	Result := nil;
end;

procedure TShortcutManager.Load(const Filename: String);
const
	STR_EMPTYBINDING = 'Ignored empty key binding for "%s/%s".';
var
	Ini: TIniFile;
	slSections, slKeys: TStringList;
	S, K, V: String;
	Sect: TKeyBindings;
	Binding: TKeyBinding;
	id, i: Integer;
begin
	Ini := TIniFile.Create(Filename);
	slSections := TStringList.Create;
	slKeys := TStringList.Create;

	Ini.ReadSections(slSections);

	for S in slSections do
	begin
		if S = 'ConfigFile' then Continue;

		Sect := SetContext(S); // add/update section
		slKeys.Clear;
		Ini.ReadSection(S, slKeys);
		id := 0;

		for K in slKeys do
		begin
			Binding := Sect.FindKey(K);
			while Binding <> nil do
			begin
				id := Binding.ID;
				i := Sect.Keys.IndexOf(Binding);
				if i >= 0 then
					Sect.Keys.Delete(i);
				Binding := Sect.FindKey(K);
			end;

			V := Ini.ReadString(S, K, '');
			if V <> '' then
				Bind(id, K, V)
			else
				Log(STR_EMPTYBINDING, [S, K]);
		end;
	end;

	slKeys.Free;
	slSections.Free;
	Ini.Free;
end;

procedure TShortcutManager.Save(const Filename: String);
var
	Ini, sl: TStringList;

	procedure IniWrite(const sKey, sValue: String);
	var
		y: Integer;
	begin
		for y := 0 to sl.Count-1 do
			if Pos(sKey, sl[y]) = 1 then
			begin
				sl[y] := sl[y] + ' | ' + sValue;
				Exit;
			end;
		sl.Add(sKey + sValue);
	end;

var
	Sect: TKeyBindings;
	Key:  TKeyBinding;
begin
	Ini := TStringList.Create;
	sl := TStringList.Create;

	Ini.Add('[ConfigFile]');
	Ini.Add('Version=' + VERSION);
	Ini.Add('');

	for Sect in Sections do
	begin
		Ini.Add('[' + Sect.Section + ']');
		sl.Clear;

		for Key in Sect.Keys do
			if (Key.Shortcut.Key <> 0) and (Key.Name <> '') then
				IniWrite(Key.Name + '=', ShortCutToText(Key.Shortcut));

		Ini.AddStrings(sl);
		Ini.Add('');
	end;

	sl.Free;
	Ini.SaveToFile(Filename);
	Ini.Free;
end;

function TShortcutManager.SetContext(const sSection: String): TKeyBindings;
begin
	CurrentSection := FindSection(sSection);
	if CurrentSection = nil then
	begin
		CurrentSection := TKeyBindings.Create;
		CurrentSection.Section := sSection;
		Sections.Add(CurrentSection);
	end;
	Result := CurrentSection;
end;

function TShortcutManager.Bind(const ID: Variant; const Name: String;
	const Shortcut: TShortCut): TShortCut;
var
	Key: TKeyBinding;
begin
	if (Shortcut.Key = 0) and (Shortcut.Shift = []) then
		Log(TEXT_WARNING + 'Empty shortcut when binding "%s/%s"!',
			[CurrentSection.Section, Name]);
	Result.Key := Shortcut.Key;
	Result.Shift := Shortcut.Shift;
	Key := TKeyBinding.Create(ID, Name, Result);
	CurrentSection.Keys.Add(Key);
end;

function TShortcutManager.Bind(const ID: Variant; const Name: String;
	const Shortcuts: array of TShortCut): TShortCut;
var
	Sc: TShortcut;
begin
	Result.Key := 0;
	Result.Shift := [];
	for Sc in Shortcuts do
		Result := Bind(ID, Name, Sc);
end;

function TShortcutManager.Bind(const ID: Variant; const Name, KeyString: String): TShortCut;
var
	Key: TKeyBinding;
	i: Integer;
begin
	if Pos(' | ', KeyString) < 1 then
	begin
		Result := TextToShortCut(KeyString);
		if (Result.Key = 0) and (Result.Shift = []) then
			Log(TEXT_WARNING + 'Invalid shortcut "%s" when binding "%s/%s"!',
				[KeyString, CurrentSection.Section, Name]);
		Key := TKeyBinding.Create(ID, Name, Result);
		CurrentSection.Keys.Add(Key);
	end
	else
	// multiple shortcuts separated by '|'
	with TStringList.Create do // multiple shortcuts are separated with '|'
	try
		Result.Key := 0;
		Result.Shift := [];
		Text := ReplaceText(KeyString, ' | ', #13#10);
		for i := 0 to Count-1 do
			Result := Bind(ID, Name, Strings[i]);
	finally
		Free;
	end;
end;

function TShortcutManager.Bind(const ID: Variant; const Name: String;
	const KeyStrings: array of String): TShortCut; overload;
var
	Sc: String;
begin
	Result.Key := 0; Result.Shift := [];
	for Sc in KeyStrings do
		Result := Bind(ID, Name, Sc);
end;

initialization

	Shortcuts := TShortcutManager.Create;

finalization

	Shortcuts.Free;

end.
