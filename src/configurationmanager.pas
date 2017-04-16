unit ConfigurationManager;

interface

uses
	Classes, Types, Generics.Collections, IniFiles;

const
	CN_YESNO: array[0..1] of AnsiString = ('No ', 'Yes');

type
	TSettingChangeCallback = procedure;

	TConfigItem = class
	public
		Section,
		Name,
		Caption,
		FormatString:	AnsiString;
		ValueNames: 	array of AnsiString;
		Min, Max,
		Step,
		LargeStep:		Integer;

		Callback:		TSettingChangeCallback;

		procedure 		SetInfo(const aCaption: AnsiString; aMin, aMax: Integer;
						const aValueNames: array of AnsiString;
						const aCallback: TSettingChangeCallback = nil;
						const aFormatString: AnsiString = '';
						aStep: Integer = 1; aLargeStep: Integer = 5);
		procedure 		SetInfoFromDir(const aCaption: AnsiString;
						const Dir, Extensions: String;
						const aCallback: TSettingChangeCallback = nil);
		procedure		ModifyValue(Amount: Integer); virtual;
		function		ValueToString: AnsiString; virtual;

		procedure		Load(const Ini: TIniFile); virtual;
		procedure		Save(const Ini: TIniFile); virtual;

		constructor		Create(const ItemSection, ItemName: String); overload;
		destructor		Destroy; override;
	end;

	TConfigItemBoolean = class(TConfigItem)
	public
		Value:			PBoolean;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Boolean;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemByte = class(TConfigItem)
	public
		Value:			PByte;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Byte;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemInteger = class(TConfigItem)
	public
		Value:			PInteger;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Integer;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemCardinal = class(TConfigItem)
	public
		Value:			PCardinal;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Cardinal;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemFloat = class(TConfigItem)
	public
		Value:			PSingle;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Single;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemString = class(TConfigItem)
	private
		CurrentIndex:	Integer;
	public
		Value:			PString;
		AllowEmpty:		Boolean;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: String;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;

		constructor		Create(const ItemSection, ItemName: String); overload;
	end;

	TConfigItemList = TObjectList<TConfigItem>;

	TConfigurationManager = class
		Items:		TConfigItemList;
		Filename:	String;

		constructor	Create;
		destructor	Destroy; override;

		function	AddBoolean	(const Section, Name: AnsiString; const Value: PBoolean;
								 const DefaultValue: Boolean = False): TConfigItemBoolean;
		function	AddByte		(const Section, Name: AnsiString; const Value: PByte;
								 const DefaultValue: Byte = 0): TConfigItemByte;
		function	AddInteger	(const Section, Name: AnsiString; const Value: PInteger;
								 const DefaultValue: Integer = 0): TConfigItemInteger;
		function	AddCardinal	(const Section, Name: AnsiString; const Value: PCardinal;
								 const DefaultValue: Cardinal = 0): TConfigItemCardinal;
		function 	AddFloat	(const Section, Name: AnsiString; const Value: PSingle;
								 const DefaultValue: Single = 0): TConfigItemFloat;
		function 	AddString	(const Section, Name: AnsiString; const Value: PString;
								 const DefaultValue: AnsiString = '';
								 AllowEmpty: Boolean = True): TConfigItemString;

		function	Load: Boolean;
		procedure	Save;
	end;


implementation

{$R-}

uses
	SysUtils,
	FileUtils;

{ TConfigurationManager }

constructor TConfigurationManager.Create;
begin
	Filename := '';
	Items := TConfigItemList.Create(True);
end;

destructor TConfigurationManager.Destroy;
begin
	Items.Free;
	inherited Destroy;
end;

function TConfigurationManager.Load: Boolean;
var
	Ini: TIniFile;
	Item: TConfigItem;
begin
	if (Filename = '') or (not FileExists(Filename)) then Exit(False);
	if Items.Count <= 0 then Exit(False);	// nothing to load

	Ini := TIniFile.Create(Filename);
	try
		for Item in Items do
			Item.Load(Ini);
	finally
		Ini.Free;
	end;
	Result := True;
end;

procedure TConfigurationManager.Save;
var
	Ini: TIniFile;
	Item: TConfigItem;
begin
	if Filename = '' then Exit;
	if Items.Count <= 0 then Exit;	// nothing to save

	Ini := TIniFile.Create(Filename);
	try
		for Item in Items do
			Item.Save(Ini);
	finally
		Ini.Free;
	end;
end;

function TConfigurationManager.AddBoolean;
begin
	Result := TConfigItemBoolean.Create(Section, Name);
	Result.Value := Value;
	PBoolean(Result.Value)^ := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddByte;
begin
	Result := TConfigItemByte.Create(Section, Name);
	Result.Value := Value;
	PByte(Result.Value)^ := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddInteger;
begin
	Result := TConfigItemInteger.Create(Section, Name);
	Result.Value := Value;
	PInteger(Result.Value)^ := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddCardinal;
begin
	Result := TConfigItemCardinal.Create(Section, Name);
	Result.Value := Value;
	PCardinal(Result.Value)^ := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddFloat;
begin
	Result := TConfigItemFloat.Create(Section, Name);
	Result.Value := Value;
	PSingle(Result.Value)^ := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddString;
begin
	Result := TConfigItemString.Create(Section, Name);
	Result.Value := Value;
	PString(Result.Value)^ := DefaultValue;
	Result.AllowEmpty := AllowEmpty;
	Items.Add(Result);
end;

{ TConfigItem }

constructor TConfigItem.Create(const ItemSection, ItemName: String);
begin
	inherited Create;

	Section := ItemSection;
	Name := ItemName;
	Caption := '';
	Min := 0;
	Max := 0;
	Callback := nil;
end;

destructor TConfigItem.Destroy;
begin
	inherited Destroy;
end;

procedure TConfigItem.SetInfo;
var
	i: Integer;
begin
	if aCaption = '§' then
		Caption := Name
	else
		Caption := aCaption;
	FormatString := aFormatString;
	SetLength(ValueNames, Length(aValueNames));
	for i := Low(aValueNames) to High(aValueNames) do
		ValueNames[i] := aValueNames[i];
	Min := aMin;
	Max := aMax;
	Step := aStep;
	LargeStep := aLargeStep;
	Callback := aCallback;
end;

procedure TConfigItem.SetInfoFromDir(const aCaption: AnsiString;
	const Dir, Extensions: String;
	const aCallback: TSettingChangeCallback = nil);
var
	Filelist: array of AnsiString;
	Sl: TStringList;
	i: Integer;
begin
	Sl := TStringList.Create;
	FileSearch(Dir, Extensions, Sl);
	Sl.Sort;
	SetLength(Filelist, Sl.Count);
	for i := 0 to Sl.Count-1 do
		Filelist[i] := ChangeFileExt(ExtractFilename(Sl[i]), '');

	SetInfo(aCaption, 0, Sl.Count-1, Filelist, aCallback);

	if (Self is TConfigItemString) then
	with (Self as TConfigItemString) do
	begin
		for i := 0 to Sl.Count-1 do
			if FileList[i] = Value^ then
			begin
				CurrentIndex := i;
				Break;
			end;
	end;

	Sl.Free;
end;

procedure TConfigItem.Load(const Ini: TIniFile);
begin
end;

procedure TConfigItem.Save(const Ini: TIniFile);
begin
end;

function TConfigItem.ValueToString: AnsiString;
begin
	Result := '';
end;

procedure TConfigItem.ModifyValue(Amount: Integer);
begin
{	if Assigned(Callback) then
		Callback;}
end;

{ TConfigItemString }

constructor TConfigItemString.Create(const ItemSection, ItemName: String);
begin
	inherited Create(ItemSection, ItemName);
	CurrentIndex := 0;
end;

procedure TConfigItemString.Load(const Ini: TIniFile);
var
	S: String;
begin
	S := Ini.ReadString(Section, Name, PString(Value)^);
	if (AllowEmpty) or (S <> '') then
		PString(Value)^ := S;
end;

procedure TConfigItemString.Save(const Ini: TIniFile);
begin
	Ini.WriteString(Section, Name, PString(Value)^);
end;

function TConfigItemString.GetValue: String;
begin
	Result := Value^;
end;

procedure TConfigItemString.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := CurrentIndex + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := Self.ValueNames[V];
	CurrentIndex := V;
	inherited;
end;

function TConfigItemString.ValueToString: AnsiString;
begin
	Result := Value^;
end;

{ TConfigItemFloat }

procedure TConfigItemFloat.Load(const Ini: TIniFile);
begin
	PSingle(Value)^ := Ini.ReadFloat(Section, Name, PSingle(Value)^);
end;

procedure TConfigItemFloat.Save(const Ini: TIniFile);
begin
	Ini.WriteFloat(Section, Name, PSingle(Value)^);
end;

function TConfigItemFloat.GetValue: Single;
begin
	Result := Value^;
end;

procedure TConfigItemFloat.ModifyValue(Amount: Integer);
var
	V: Single;
begin
	V := Value^ + (Amount / 10);
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := V;
	inherited;
end;

function TConfigItemFloat.ValueToString: AnsiString;
begin
	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := Format('%f', [Max - Value^]);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

{ TConfigItemCardinal }

procedure TConfigItemCardinal.Load(const Ini: TIniFile);
var
	S: String;
begin
	S := Ini.ReadString(Section, Name, '');
	if S <> '' then
		PCardinal(Value)^ := Cardinal(StrToInt(S));
end;

procedure TConfigItemCardinal.Save(const Ini: TIniFile);
begin
	Ini.WriteString(Section, Name, '$' + IntToHex(PCardinal(Value)^, 8));
end;

function TConfigItemCardinal.GetValue: Cardinal;
begin
	Result := Value^;
end;

procedure TConfigItemCardinal.ModifyValue(Amount: Integer);
var
	V: Cardinal;
begin
	V := Value^ + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := V;
	inherited;
end;

function TConfigItemCardinal.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

{ TConfigItemInteger }

procedure TConfigItemInteger.Load(const Ini: TIniFile);
begin
	PInteger(Value)^ := Ini.ReadInteger(Section, Name, PInteger(Value)^);
end;

procedure TConfigItemInteger.Save(const Ini: TIniFile);
begin
	Ini.WriteInteger(Section, Name, PInteger(Value)^);
end;

function TConfigItemInteger.GetValue: Integer;
begin
	Result := Value^;
end;

procedure TConfigItemInteger.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := Value^ + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := V;
	inherited;
end;

function TConfigItemInteger.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

{ TConfigItemByte }

procedure TConfigItemByte.Load(const Ini: TIniFile);
var
	i: Integer;
begin
	i := Ini.ReadInteger(Section, Name, PByte(Value)^);
	if (i >= 0) and (i <= 255) then
		PByte(Value)^ := i;
end;

procedure TConfigItemByte.Save(const Ini: TIniFile);
begin
	Ini.WriteInteger(Section, Name, PByte(Value)^);
end;

function TConfigItemByte.GetValue: Byte;
begin
	Result := Value^;
end;

procedure TConfigItemByte.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := PByte(Value)^ + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	PByte(Value)^ := Byte(V);
	inherited;
end;

function TConfigItemByte.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if (Max - Min) <= Length(ValueNames) then
		Result := ValueNames[Value^-Min]
	else
	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[Min]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1]
	else
	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);
end;

{ TConfigItemBoolean }

procedure TConfigItemBoolean.Load(const Ini: TIniFile);
begin
	PBoolean(Value)^ := Ini.ReadBool(Section, Name, PBoolean(Value)^);
end;

procedure TConfigItemBoolean.Save(const Ini: TIniFile);
begin
	Ini.WriteBool(Section, Name, PBoolean(Value)^);
end;

function TConfigItemBoolean.GetValue: Boolean;
begin
	Result := Value^;
end;

procedure TConfigItemBoolean.ModifyValue(Amount: Integer);
begin
	if Amount > 0 then
		Value^ := True
	else
	if Amount < 0 then
		Value^ := False;
	inherited;
end;

function TConfigItemBoolean.ValueToString: AnsiString;
begin
	if Value^ = True then
	begin
		if High(ValueNames) >= 1 then
			Result := ValueNames[1]
		else
			Result := 'True';
	end
	else
	begin
		if Length(ValueNames) > 0 then
			Result := ValueNames[0]
		else
			Result := 'False';
	end;
end;

end.

