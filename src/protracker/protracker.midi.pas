unit ProTracker.MIDI;

// Wrapper for Midi.pas for handling MIDI controllers
// hukka 2017

//{$mode Delphi}{$H+}
{$mode objfpc}
{.$DEFINE LOGGING}

interface

uses
	Classes, SysUtils, Generics.Collections,
	ShortcutManager, Graphics32, MIDIIO,
	ProTracker.Messaging, ProTracker.Player;

const
	MSG_SHORTCUT        = 200;
	MSG_MIDI_SELPATTERN	= 205;
	MSG_MIDI_SELSAMPLE	= 206;
		MSG_MIDI_FIRST = 200;
		MSG_MIDI_LAST  = 210;

	CTRLTYPE_NONE       = 0;
	CTRLTYPE_BUTTON     = 1;
	CTRLTYPE_ABSOLUTE   = 2;
	CTRLTYPE_RELATIVE   = 3;

	MIDICMD_KEY_OFF     = $80;
	MIDICMD_KEY_ON      = $90;
	MIDICMD_SLIDER      = $B0;

	MIDIACTION_SHIFT        = 'shift';
	MIDIACTION_SEL_PATTERN  = 'select.pattern';
	MIDIACTION_SEL_SAMPLE   = 'select.sample';
	MIDIACTION_PLAY_PATTERN = 'toggleplay.pattern';
	MIDIACTION_PLAY_SONG    = 'toggleplay.song';
	MIDIACTION_EDITMODE     = 'editmode';

	MIDI_FX_SCROLLTEXT		= 0;
	MIDI_FX_VU_VERTICAL     = 1;
	MIDI_FX_VU_HORIZONTAL   = 2;

type
	TMIDIHandler    = class;
	TMIDIController = class;
	TInputControl   = class;

	TInputControlCallback = procedure (var Ctrl: TInputControl;
	                        const Status, Data1, Data2: Byte) of Object;

	TActionType = (ACT_NONE, ACT_SHORTCUT, ACT_PARSED);

	TInputControlData = record
		ActionType: TActionType;
		Shortcut:   TKeyBinding;
		Data:       SmallInt;
		StringData: String;
	end;
	PInputControlData = ^TInputControlData;

	TInputControl = class
	public
		Controller: TMIDIController;
		Kind:       Byte;
		HasLED,
		LEDState:   Boolean;
		Callback:   TInputControlCallback;
		Actions:	array[0..1] of TInputControlData;
		Name:       AnsiString;

		procedure   DoAction(NoteOn: Boolean);
		function	GetActionIndex: Byte;
	end;

	TMidiInputOutput = class helper for TMidiDevices
	public
		function FindDevice(const Name: AnsiString): Integer;
		function OpenDevice(const Name: AnsiString): Integer;
	end;

	TMIDIScreenPixel = record
		Color,
		Index:      Byte;
	end;
	PMIDIScreenPixel = ^TMIDIScreenPixel;

	TMIDIScreen = class
		Controller: TMIDIController;
		Enabled:    Boolean;
		Width,
		Height:     Byte;
		Colors:     array of Byte;
		Pixels:     array of array of TMIDIScreenPixel;

		procedure  Clear;
		procedure  Update;
		procedure  SetPixel(X, Y, Col: Byte);
	end;

	TMIDIScreenEffect = class
	public
		Controller: TMIDIController;
	end;

	TScrollText = class(TMIDIScreenEffect)
	public
		Text:       AnsiString;
		Interval:   Byte;
		ScrollPix:  Byte;
		FontWidth:  Byte;
		ScrollChar: Cardinal;
		Pixels:     array of array of Boolean;
		Font:		TBitmap32;

		procedure	Update;
		function	LoadFont(const Filename: String): Boolean;

		constructor	Create;
		destructor  Destroy; override;
	end;

	TMIDIController = class
	private
		MIDIHandler:   TMIDIHandler;
	public
		Name:          String;
		InputIndex,
		OutputIndex:   Byte;
		InputName,
		OutputName:    AnsiString;
		ShiftDown:     Boolean;
		MIDINotes: record
			First:        Byte;
			IgnoreVolume: Boolean;
			Note: TNote;
		end;

		ScrollText: TScrollText;

		InputControls: array [0..$FFFF] of TInputControl;

		Screen:        TMIDIScreen;

		procedure 	MidiCallback_Button(var Ctrl: TInputControl; const Status, Data1, Data2: Byte);
		procedure 	MidiCallback_Slider(var Ctrl: TInputControl; const Status, Data1, Data2: Byte);

		procedure	UpdateScrollText;
		procedure 	UpdateVUMeter(Channel: Byte; Volume: Single);

		function    InitIO: Integer;
		function    InitMidiControl(const aIndex, aKind: Byte;
		            const aCallback: TInputControlCallback; const aName: AnsiString;
		            const aHasLED: Boolean = False; const aData: Byte = 0): TInputControl;
		function    SetCallbacks(const CtrlKind: Byte; const aCallback: TInputControlCallback): Byte;
		procedure 	FillKeyShortcuts;

		constructor Create;
		destructor  Destroy; override;
	end;

	TMIDIControllerList = specialize TObjectList<TMIDIController>;

	TMIDIHandler = class
	public
		Controllers:      TMIDIControllerList;
		InputControllers: array [0..255] of TMIDIController;
		Devices:          TMidiDevices;
		Input:            TMidiInput;
		Output:           TMidiOutput;

		procedure   OnMidiInput(const DeviceIndex: Integer; const Status, Data1, Data2: Byte);
		procedure 	UpdateVUMeter(Channel: Byte; Volume: Single);
		procedure 	InitShortcuts;
		procedure	SettingsChanged;

		function	ReadControllerDefs(const Path: String): Integer;
		function	ReadControllerDefinition(const Filename: String): Boolean;
		function	InitController(const Name: String): TMIDIController;
		function 	FindController(const Name: String): TMIDIController;

		constructor Create;
		destructor  Destroy; override;
	end;

var
	MIDI: TMIDIHandler;


implementation

uses
	IniFiles, Math, MainWindow,
	ProTracker.Util, ProTracker.Editor,
	Screen.Editor;


procedure ParseDelimited(const Sl: TStrings; const Value: String; const Delimiter: String);
var
	dx, delta: Integer;
	ns, txt: String;
begin
	delta := Length(Delimiter);
	txt := Value + Delimiter;
	Sl.BeginUpdate;
	Sl.Clear;
	try
		while Length(txt) > 0 do
		begin
			dx := Pos(Delimiter, txt);
			ns := Copy(txt, 0, dx-1);
			Sl.Add(ns);
			txt := Copy(txt, dx+delta, MaxInt);
		end;
	finally
		Sl.EndUpdate;
	end;
end;

{ TScrollText }

constructor TScrollText.Create;
begin
	Font := TBitmap32.Create;
	Text := ' apua APUA What '; //!!!
end;

destructor TScrollText.Destroy;
begin
	Font.Free;
	inherited Destroy;
end;

procedure TScrollText.Update;
var
	C: AnsiChar;
	i, X, Y: Integer;
begin
	Inc(Interval);
	if Interval < 30 then Exit;

	Interval := 0;
	Inc(ScrollPix);

	if ScrollPix >= FontWidth then
	begin
		// scroll buffer left by one char
		for Y := 0 to Controller.Screen.Height-1 do
		for X := 0 to Controller.Screen.Width-1 do
			Pixels[X, Y] := Pixels[X + FontWidth, Y];

		Inc(ScrollChar);
		if ScrollChar > Length(Text) then
			ScrollChar := 1;

		i := (Ord(Text[ScrollChar]) - 32) * FontWidth;

		for Y := 0 to Controller.Screen.Height-1 do
		for X := 0 to FontWidth-1 do
			Pixels[X + FontWidth, Y] := (Font.Pixel[X+i, Y] <> $FF000000);

		ScrollPix := 0;
	end;

	for Y := 0 to Controller.Screen.Height-1 do
	for X := 0 to Controller.Screen.Width-1 do
	begin
		if Pixels[X + ScrollPix, Y] then
			i := Controller.Screen.Colors[1]
		else
			i := 0;
		Controller.Screen.SetPixel(X, Y, i);
	end;
end;

function TScrollText.LoadFont(const Filename: String): Boolean;
var
	Fn: String;
begin
	Fn := IncludeTrailingPathDelimiter(DataPath + 'midi') +  Filename + '.pcx';
	log('LoadFont='+Fn);
	if not FileExists(Fn) then
	begin
		Log('File not found');
		Exit(False);
	end;

	Font.LoadFromFile(Fn);
	Result := (Font <> nil);
	if Result then
	begin
		FontWidth := Font.Width div 96;
		ScrollChar := 1;
		SetLength(Pixels, Controller.Screen.Width + FontWidth, Controller.Screen.Height);

		Result := (Font.Height >= Controller.Screen.Height);
	end;
end;

{ TInputControl }

procedure TInputControl.DoAction(NoteOn: Boolean);
begin
	with Actions[GetActionIndex] do
	begin
		if ActionType <> ACT_PARSED then
		begin
			{$IFDEF LOGGING}
			Log('MIDI: Wrong ActionType! StringData='+StringData);
			{$ENDIF}
			Exit;
		end;

		if StringData = MIDIACTION_SHIFT then
		begin
			Controller.ShiftDown := NoteOn;
			Exit;
		end
		else
			if not NoteOn then Exit;

		case StringData of

			MIDIACTION_SEL_SAMPLE:
				PostMessageValue(MSG_MIDI_SELSAMPLE, Data);

			MIDIACTION_SEL_PATTERN:
				PostMessageValue(MSG_MIDI_SELPATTERN, Data);

			MIDIACTION_PLAY_PATTERN:
				if Module.PlayMode <> PLAY_PATTERN then
					Module.PlayPattern(CurrentPattern)
				else
					Module.Stop;

			MIDIACTION_PLAY_SONG:
				if Module.PlayMode in [PLAY_STOPPED, PLAY_PATTERN] then
					Module.Play
				else
					Module.Stop;

			MIDIACTION_EDITMODE:
			;

		end; // case

	end; // Actions[]
end;

function TInputControl.GetActionIndex: Byte; inline;
begin
	if (Controller.ShiftDown) and
		((Actions[0].StringData <> 'shift') and (Actions[1].ActionType <> ACT_NONE)) then
		Result := 1
	else
		Result := 0;
end;

{ TMIDIController }

constructor TMIDIController.Create;
var
	i: Integer;
begin
	inherited;

	for i := 0 to High(InputControls) do
		InputControls[i] := nil;

	MIDINotes.First := 255; // disable

	Screen := TMIDIScreen.Create;
	Screen.Enabled := False;
	ScrollText := nil;
end;

destructor TMIDIController.Destroy;
var
	i: Integer;
begin
	if Assigned(ScrollText) then ScrollText.Free;
	Screen.Clear;
	Screen.Free;

	for i := 0 to High(InputControls) do
		if InputControls[i] <> nil then
			InputControls[i].Free;

	inherited Destroy;
end;

procedure TMIDIController.MidiCallback_Button(var Ctrl: TInputControl; const Status, Data1, Data2: Byte);
var
	i, Stat: Integer;
	Key: TKeyBinding;
begin
	if Ctrl = nil then
	begin
		{$IFDEF LOGGING}
		Log('MIDI: ButtonHandler: Ctrl=nil!');
		{$ENDIF}
		Exit;
	end;

	Stat := Status and $F0;

	case Stat of

		MIDICMD_KEY_ON:
			if Ctrl.HasLED then
			begin
				Ctrl.LEDState := not Ctrl.LEDState;
				if Ctrl.LEDState then i := 127 else i := 0;
				MIDIHandler.Output.Send(OutputIndex, MIDICMD_KEY_ON, Data1, i);
			end;

		MIDICMD_KEY_OFF:
			;

		$B0:
			if Data2 = 0 then Exit;

		else
			{$IFDEF LOGGING}
			Log(Format('MIDI: ButtonHandler: Unhandled "%s" %x %x %x', [Ctrl.Name, Status, Data1, Data2]));
			{$ENDIF}
			Exit;
	end;

	i := Ctrl.GetActionIndex;
	if (Stat <> MIDICMD_KEY_OFF) and (Ctrl.Actions[i].ActionType = ACT_SHORTCUT) then
		PostMessagePtr(MSG_SHORTCUT, @Ctrl.Actions[i].Shortcut)
	else
		Ctrl.DoAction(Stat = MIDICMD_KEY_ON);
end;

procedure TMIDIController.MidiCallback_Slider(var Ctrl: TInputControl; const Status, Data1, Data2: Byte);
{var
	S: AnsiString;
	i: Integer;}
begin
{	if Ctrl = nil then Exit;

	S := '[Slider/pot callback] ' + Ctrl.Name;

	if Status = MIDICMD_SLIDER then
	case Ctrl.Kind of

		CTRLTYPE_RELATIVE:
		begin
			i := Data2;
			if i > 63 then
				i := -(128 - i);
			S := S + Format(' (Relative) Value change: %d', [i]);
		end;

		CTRLTYPE_ABSOLUTE:
			S := S + Format(' (Absolute) Value: %d', [Data2]);
	end;
}
end;

procedure TMIDIController.UpdateScrollText; inline;
begin
	if ScrollText <> nil then
		ScrollText.Update;
end;

procedure TMIDIController.UpdateVUMeter(Channel: Byte; Volume: Single);
var
	X, Y, j: Integer;
	C: Byte;
begin
	if not Screen.Enabled then Exit;

	case Options.Midi.DisplayEffect of

		MIDI_FX_SCROLLTEXT:
			UpdateScrollText;

		MIDI_FX_VU_HORIZONTAL:
		begin
			if Screen.Height < 4 then Exit;

			j := Screen.Width;
			Y := Channel + 1;
			X := Trunc(j * Volume);

			if X >= j-1 then
				C := Screen.Colors[0]
			else
			if X = j-2 then
				C := Screen.Colors[1]
			else
				C := Screen.Colors[2];

			for j := 0 to X do // draw
				Screen.SetPixel(j, Y, C);
			if X < Screen.Width-1 then
				for j := X to Screen.Width-1 do // clear
					Screen.SetPixel(j, Y, 0);
		end;

		MIDI_FX_VU_VERTICAL:
		begin
			j := Screen.Width;
			if j < 4 then Exit;

			X := Channel * (j div 4); // spacing
			Y := Screen.Height - Trunc((Screen.Height + 1) * Volume);
			for j := 0 to Y-1 do // clear
				Screen.SetPixel(X, j, 0);
			for j := Y to Screen.Height-1 do // draw
				if (j >= 0) and (j < Screen.Height) then
					Screen.SetPixel(X, j, Screen.Colors[j]);
		end;

	end;
end;

function TMIDIController.InitIO: Integer;
begin
	{$IFDEF LOGGING}
	//Log('MIDI: Controller.InitIO: ' + Name);
	{$ENDIF}

	InputIndex := MIDIHandler.Input.OpenDevice(InputName);
	OutputIndex := MIDIHandler.Output.OpenDevice(OutputName);

	{$IFDEF LOGGING}
	if InputIndex >= 0 then
		Log(Format('MIDI: Opened input device %d (%s)', [InputIndex, InputName]));
	if OutputIndex >= 0 then
		Log(Format('MIDI: Opened output device %d (%s)', [OutputIndex, OutputName]));
	{$ENDIF}

	ShiftDown := False;
	Screen.Controller := Self;

	Result := InputIndex;
	if Result >= 0 then
	begin
		Screen.Clear;
		FillKeyShortcuts;
	end;
end;

procedure TMIDIController.FillKeyShortcuts;
var
	IC: TInputControl;
	i, ac: Integer;
begin
	for i := 0 to High(InputControls) do
	begin
		IC := InputControls[i];
		if IC = nil then Continue;
		for ac := 0 to High(IC.Actions) do
			with IC.Actions[ac] do
			begin
				if ActionType = ACT_SHORTCUT then
					Shortcut := Shortcuts.FindByName(StringData);
			end;
	end;
end;

function TMIDIController.InitMidiControl(const aIndex, aKind: Byte;
         const aCallback: TInputControlCallback; const aName: AnsiString;
         const aHasLED: Boolean = False; const aData: Byte = 0): TInputControl;
var
	i: Integer;
begin
	if InputControls[aIndex] = nil then
		InputControls[aIndex] := TInputControl.Create;

	with InputControls[aIndex] do
	begin
		Kind := aKind;
		Name := aName;
		HasLED   := aHasLED;
		LEDState := False;
		Callback := aCallback;
		Controller := Self;
		for i := 0 to High(Actions) do
		with Actions[i] do
		begin
			ActionType := ACT_NONE;
			Data := 0;
			StringData := '';
		end;
	end;

	Result := InputControls[aIndex];
end;

function TMIDIController.SetCallbacks(const CtrlKind: Byte;
          const aCallback: TInputControlCallback): Byte;
var
	Ctrl: TInputControl;
	i: Integer;
begin
	Result := 0;
	for i := 0 to High(InputControls) do
	begin
		Ctrl := InputControls[i];
		if (Ctrl <> nil) and (Ctrl.Kind = CtrlKind) then
		begin
			Ctrl.Callback := aCallback;
			Inc(Result);
		end;
	end;
end;

{ TMidiInputOutput }

function TMidiInputOutput.FindDevice(const Name: AnsiString): Integer;
var
	i: Integer;
	S: AnsiString;
begin
	S := LowerCase(Name);
	for i := 0 to Devices.Count-1 do
		if LowerCase(Devices[i]) = S then Exit(i);
	Result := -1;
end;

function TMidiInputOutput.OpenDevice(const Name: AnsiString): Integer;
begin
	Result := FindDevice(Name);
	if Result >= 0 then
		Open(Result);
end;

{ TMIDIHandler }

constructor TMIDIHandler.Create;
var
	i: Integer;
	Ctrl: TMIDIController;
begin
	inherited;

	for i := 0 to High(InputControllers) do
		InputControllers[i] := nil;

	Devices := TMidiDevices.Create;
	Input  := MidiInput;
	Output := MidiOutput;
	Controllers := TMIDIControllerList.Create(True);

	ReadControllerDefs(DataPath + 'midi');

	// remove unavailable controllers
	for i := Controllers.Count-1 downto 0 do
	begin
		Ctrl := Controllers[i];
		if Ctrl.InitIO >= 0 then
			with Ctrl do
			begin
				SetCallbacks(CTRLTYPE_BUTTON,   @MidiCallback_Button);
				SetCallbacks(CTRLTYPE_ABSOLUTE, @MidiCallback_Slider);
				SetCallbacks(CTRLTYPE_RELATIVE, @MidiCallback_Slider);
			end
		else
			Controllers.Delete(i);
	end;

	Input.OnMidiData := @OnMidiInput;

	// register MIDI event messages
	RegisterMessages(MSG_MIDI_FIRST, MSG_MIDI_LAST-MSG_MIDI_FIRST);
end;

destructor TMIDIHandler.Destroy;
begin
	Controllers.Free;

	Input.CloseAll;
	Output.CloseAll;
	Devices.Free;

	inherited;
end;

procedure TMIDIHandler.OnMidiInput(const DeviceIndex: Integer; const Status, Data1, Data2: Byte);
var
	Cont: TMIDIController;
	Ctrl: TInputControl;
	Code: Word;
	Stat, Vol: Byte;
begin
	{$IFDEF LOGGING}
	//Log(Format('Dev=%d S=%x D=%x,%x', [DeviceIndex, Status, Data1, Data2]));
	{$ENDIF}

	Stat := Status and $F0;
	Cont := InputControllers[DeviceIndex];

	if Cont <> nil then
	begin
(*		if Stat = MIDICMD_KEY_ON then
		begin
			Vol := Random(6)+1;
//			Output.Send(Cont.OutputIndex, MIDICMD_KEY_ON, Data1, Vol);
			Output.Send(Cont.OutputIndex, MIDICMD_KEY_ON, $5D, 3);
//log(inttostr(data1));
		end
		else
		if Stat = MIDICMD_KEY_OFF then
			Output.Send(Cont.OutputIndex, MIDICMD_KEY_ON, Data1, 0);
*)
		if Stat = MIDICMD_KEY_OFF then
			Ctrl := Cont.InputControls[MIDICMD_KEY_ON shl 8 or Data1]
		else
			Ctrl := Cont.InputControls[Status shl 8 or Data1];

		if (Ctrl <> nil) and (Ctrl.Kind <> CTRLTYPE_NONE) and (Assigned(Ctrl.Callback)) then
		begin
			{$IFDEF LOGGING}
			case Ctrl.Kind of
				CTRLTYPE_NONE:     s := 'none';
				CTRLTYPE_BUTTON:   s := 'Button';
				CTRLTYPE_RELATIVE: s := 'Rel';
				CTRLTYPE_ABSOLUTE: s := 'Abs';
			end;
			Log(Format('MIDI: Calling: %s "%s" %x %x %x', [s, Ctrl.Name, Status, Data1, Data2]));
			{$ENDIF}

			Ctrl.Callback(Ctrl, Status, Data1, Data2);
			Exit;
		end
		else
		if (Stat = MIDICMD_KEY_ON) and (Cont.MIDINotes.First <> 255) and
			(Data1 >= Cont.MIDINotes.First) and (Data1 <= Cont.MIDINotes.First + 35) then
			with Cont.MIDINotes do
			begin
				Note.Period := PeriodTable[Data1 - First];
				Note.Sample := CurrentSample;
				if IgnoreVolume then
					Vol := 255
				else
					Vol := (Data2 + 1) div 2; // 1..64
				Module.PlayNote(@Note, PatternEditor.Cursor.Channel, Vol);
			end;
	end;

	{$IFDEF LOGGING}
	if Ctrl <> nil then
		Log(Format('MIDI: Unknown: "%s" %x %x %x', [Ctrl.Name, Status, Data1, Data2]))
	else
		Log(Format('MIDI: Unknown: %x %x %x', [Status, Data1, Data2]));
	{$ENDIF}
end;

procedure TMIDIHandler.UpdateVUMeter(Channel: Byte; Volume: Single);
var
	Cont: TMIDIController;
begin
	if (Options.Midi.Enabled) and (Options.Midi.UseDisplay) then
	for Cont in Controllers do
		if Cont.Screen.Enabled then
			Cont.UpdateVUMeter(Channel, Volume);
end;

procedure TMIDIHandler.InitShortcuts;
var
	Cont: TMIDIController;
begin
	for Cont in Controllers do
		Cont.FillKeyShortcuts;
end;

procedure TMIDIHandler.SettingsChanged;
var
	Cont: TMIDIController;
begin
	for Cont in Controllers do
	begin
		if Cont.Screen.Enabled then
			Cont.Screen.Clear;
	end;
end;

function TMIDIHandler.ReadControllerDefs(const Path: String): Integer;
var
	SPath: String;
	SR: TSearchRec;
begin
	Result := 0;
	SPath := IncludeTrailingPathDelimiter(Path);
	if FindFirst(SPath + '*.ini', faAnyFile, SR) = 0 then
	repeat
		if ReadControllerDefinition(SPath + SR.Name) then Inc(Result);
	until FindNext(SR) <> 0;
	FindClose(SR);
end;

function TMIDIHandler.ReadControllerDefinition(const Filename: String): Boolean;
var
	Sl, Sections: TStringList;
	Cont: TMIDIController;
	Ini: TIniFile;
	S, Section: String;
	Index, i, ac, M: Integer;
begin
	Result := False;
	if not FileExists(Filename) then Exit;

	Sections := TStringList.Create;

	Ini := TIniFile.Create(Filename);
	Ini.ReadSections(Sections);

	Section := 'Controller';

	if Sections.IndexOf(Section) >= 0 then
	begin
		Cont := TMIDIController.Create;

		with Cont do
		begin
			Name        := Ini.ReadString(Section, 'Name', '');
			InputName   := Ini.ReadString(Section, 'Input', Name);
			OutputName  := Ini.ReadString(Section, 'Output', '');
			InputIndex  := Input.FindDevice(InputName);
			OutputIndex := Output.FindDevice(OutputName);

			for Section in Sections do
			begin
				if Section = 'Screen' then
				begin
					if not Options.Midi.UseDisplay then Continue;

					Sl := TStringList.Create;

					Screen.Enabled := True;
					Screen.Controller := Cont;

					Screen.Width  := Ini.ReadInteger(Section, 'Width', 0);
					Screen.Height := Ini.ReadInteger(Section, 'Height', 0);

					SetLength(Screen.Colors, Screen.Height);
					SetLength(Screen.Pixels, Screen.Width, Screen.Height);

					S := Ini.ReadString(Section, 'Font', '');
					if S <> '' then
					begin
						ScrollText := TScrollText.Create;
						ScrollText.Controller := Cont;
						//ScrollText.Interval := ;
						if not ScrollText.LoadFont(S) then
							FreeAndNil(ScrollText);
					end;

					ParseDelimited(Sl, Ini.ReadString(Section, 'Colors', '1'), ',');
					M := Min(Sl.Count, Screen.Height);
					for i := 0 to M-1 do
						Screen.Colors[i] := StrToInt(Sl[i]);

					for M := 0 to Screen.Height-1 do
					begin
						ParseDelimited(Sl, Ini.ReadString(
							Section, 'Row' + IntToStr(M+1), ''), ',');
						if Sl.Count > 0 then
							for i := 0 to Min(Sl.Count, Screen.Width)-1 do
							begin
								Screen.Pixels[i,M].Index := StrToInt(Sl[i]);
								Screen.Pixels[i,M].Color := 0;
							end
						else
						begin
							Screen.Enabled := False;
							Break;
						end;
					end;

					Sl.Free;

					{$IFDEF LOGGING}
					if Screen.Enabled then
						Log(Format('MIDI: Using %dx%d screen for controller ',
							[Screen.Width, Screen.Height, Cont.Name]))
					else
						Log('MIDI: Error parsing screen info for controller ' + Cont.Name);
					{$ENDIF}
				end
				else
				if Section = 'Notes' then
				begin
					MIDINotes.First := Ini.ReadInteger(Section, 'First', 36);
					MIDINotes.IgnoreVolume := (LowerCase(Ini.ReadString(Section, 'IgnoreVolume', '')) = 'true');
				end
				else
				begin
					if Pos('$', Section) <> 1 then Continue;

					S := Trim(LowerCase(Ini.ReadString(Section, 'Type', '')));

					if S = 'button' then
						i := CTRLTYPE_BUTTON
					else
					if S = 'absolute' then
						i := CTRLTYPE_ABSOLUTE
					else
					if S = 'relative' then
						i := CTRLTYPE_RELATIVE
					else
						i := CTRLTYPE_BUTTON; // CTRLTYPE_NONE

					if Length(Section) <> 5 then Continue;
					Index := StrToInt(Section);
					if (Index < 0) or (Index > $FFFF) then Continue;

					if InputControls[Index] = nil then
						InputControls[Index] := TInputControl.Create;

					with InputControls[Index] do
					begin
						Kind := i;
						Name := Ini.ReadString(Section, 'Name', '');

						for ac := 0 to 1 do
						with Actions[ac] do
						begin
							Data := 0;
							if ac = 0 then
								StringData := Trim(Ini.ReadString(Section, 'Function', ''))
							else
								StringData := Trim(Ini.ReadString(Section, 'Shifted', ''));
							ActionType := ACT_NONE;
							i := Pos(':', StringData);
							if i > 1 then
							begin
								S := LowerCase(Copy(StringData, 1, i-1));
								StringData := Copy(StringData, i+1, MaxInt);
								if S = 'shortcut' then
								begin
									ActionType := ACT_SHORTCUT;
									Shortcut := Shortcuts.FindByName(StringData);
								end;
							end
							else
							if StringData <> '' then
							begin
								ActionType := ACT_PARSED;
								StringData := LowerCase(StringData);
								i := Pos(' ', StringData);
								if i > 1 then
								begin
									S := LowerCase(Trim(Copy(StringData, i+1, MaxInt)));
									StringData := Copy(StringData, 1, i-1);
									if S <> '' then
									begin
										if S = 'prev' then
											Data := SELECT_PREV
										else
										if S = 'next' then
											Data := SELECT_NEXT
										else
										if S = 'toggle' then
											Data := SELECT_TOGGLE
										else
										try
											Data := StrToInt(S);
										except
											Data := 0;
										end;
									end;
								end;
							end;
						end; // actions[]

						HasLED   := (LowerCase(Ini.ReadString(Section, 'Led', '')) = 'true');
						LEDState := False;
						Callback := nil;
						Controller := Cont;

						if Name = '' then Name := Actions[0].StringData;
						if Name = '' then Name := Format('Unnamed:%d', [Index]);

					end; // InputControls[]

				end; // section

			end; // Controller

		end;

		Cont.MIDIHandler := Self;
		Controllers.Add(Cont);
		InputControllers[Cont.InputIndex] := Cont;

		Result := True;
	end;

	Sections.Free;
	Ini.Free;
end;

function TMIDIHandler.InitController(const Name: String): TMIDIController;
var
	Ctrl: TMIDIController;
begin
	Ctrl := FindController(Name);
	Result := Ctrl;
	if Ctrl <> nil then Ctrl.InitIO;
end;

function TMIDIHandler.FindController(const Name: String): TMIDIController;
var
	Ctrl: TMIDIController;
begin
	for Ctrl in Controllers do
		if Ctrl.Name = Name then
			Exit(Ctrl);
	Result := nil;
end;

{ TMIDIScreen }

procedure TMIDIScreen.Clear;
var
	X, Y: Integer;
begin
	if not Enabled then Exit;

	for Y := 0 to Height-1 do
	for X := 0 to Width-1 do
		SetPixel(X, Y, 0);
end;

procedure TMIDIScreen.Update;
begin
	if not Enabled then Exit;
end;

procedure TMIDIScreen.SetPixel(X, Y, Col: Byte);
var
	Pixel: PMIDIScreenPixel;
begin
	if (not Enabled) or (X >= Width) or (Y >= Height) then Exit;

	Pixel := @Pixels[X, Y];
	if Col <> Pixel^.Color then
	begin
		Pixel^.Color := Col;
		Controller.MIDIHandler.Output.Send(
			Controller.OutputIndex, MIDICMD_KEY_ON, Pixel^.Index, Col);
	end;
end;

end.

