unit ProTracker.Import;

interface

uses
	Generics.Collections, FileStreamEx,
	ProTracker.Util, ProTracker.Player, ProTracker.Sample;

const
	OW_NONE				= 0; // don't overwrite any effects
	OW_VOLUME_NONZERO	= 1; // can overwrite volume if not C00
	OW_VOLUME_ANY		= 2; // can overwrite any volume effect

type
	TConversion = record

		Module:             TPTModule;
		VolEffects:         Cardinal;

		Missed: record
			Notes,
			Effects,
			Volumes,
			VolEffects,
			Tracks:         Cardinal;
		end;

		Want: record
			InsertTempo,
			InsertPattBreak,
			FillParams:     Boolean;
		end;

		Info: record
			OrigSpeed,
			OrigTempo:      Byte;
		end;

	end;

	TExtNote = record
		Instrument:		Byte;
		Pitch:			Byte;
		Volume:			Byte;
		Command:		Byte;
		Parameter:		Byte;
	end;
	PExtNote = ^TExtNote;

	TExtPattern = class
	private
		EmptyNote: 		TExtNote;
	public
		Index,
		Rows,
		Channels,
		UsedChannels:	Byte;

		Notes: 			array of array of TExtNote;

		function		FindFreeEffectSlot(row: Byte; var Note: PExtNote;
						AllowOverwrite: Byte = OW_NONE): Boolean;
		function 		InsertTempoEffect(var Module: TPTModule; tempo, speed: Byte): Boolean;

		constructor 	Create(NumChannels, NumRows: Byte);
		procedure		Resize(NumChannels, NumRows: Byte; Clear: Boolean = False);
	end;

	TExtPatternList = TObjectList<TExtPattern>;


	TImportedModule = class
	protected
		Module:      TPTModule;
		Conversion:  TConversion;
		Patterns:    TExtPatternList;
		ModFile:     TFileStreamEx;
		SamplesOnly: Boolean;
		PrevParam:   array[0..255] of Byte;
	public
		procedure	LoadFromFile; virtual;
		procedure	ReadSample(i: Byte; var ips: TImportedSample); virtual; abstract;
		procedure	ProcessConvertedPatterns;
		procedure	ConvertCommand(var Note: TExtNote); virtual; abstract;
		procedure	Finish;

		constructor	Create(var aModule: TPTModule; var aModFile: TFileStreamEx;
			aSamplesOnly: Boolean = False); virtual;
		destructor	Destroy; override;
	end;


implementation

uses
	Math, SysUtils;

{ TImportedModule }

constructor TImportedModule.Create(var aModule: TPTModule;
	var aModFile: TFileStreamEx; aSamplesOnly: Boolean);
begin
	inherited Create;

	Module := aModule;
	ModFile := aModFile;
	SamplesOnly := aSamplesOnly;

	ZeroMemory(@Conversion, SizeOf(Conversion));

	Conversion.Module := aModule;
	with Conversion.Want do
	begin
		InsertTempo := True;		// insert tempo effect to first pattern?
		InsertPattBreak := True;	// add pattern breaks to patterns < 64 rows?
		FillParams := True;			// compensate for PT's lack of effect memory in some fx?
	end;

	Patterns := TExtPatternList.Create(True);

	LoadFromFile;
end;

destructor TImportedModule.Destroy;
begin
	Finish;
	Patterns.Free;
	inherited Destroy;
end;

procedure TImportedModule.LoadFromFile;
begin
//
end;

procedure TImportedModule.ProcessConvertedPatterns;
var
	i, PC, patt, chan, row, totalchannels: Integer;
	Pattern, NewPattern: TExtPattern;
	SrcNote: PExtNote;
	DstNote: PNote;
	S: String;
begin
	patt := 0;

	while patt < Patterns.Count do
	begin
		Pattern := Patterns[patt];

		// split long pattern into multiple
		if (Pattern.Rows > 64) and (Pattern.UsedChannels > 0) then
		begin
			i := Pattern.Rows;
			totalchannels := 0;
			S := Format('Splitting pattern %d', [patt]);

			while i > 64 do
			begin
				Dec(i, 64);
				Inc(totalchannels, 64);

				NewPattern := TExtPattern.Create(Pattern.UsedChannels, Min(i, 64));
				NewPattern.UsedChannels := Pattern.UsedChannels;
				S := S + Format(' -> %d (%d rows)', [patt+1, NewPattern.Rows]);

				for chan := 0 to Min(3, Pattern.UsedChannels-1) do
					for row := 0 to Min(63, i-1) do
						NewPattern.Notes[chan, row] := Pattern.Notes[chan, row+totalchannels];

				for row := 0 to 127 do
					if Module.OrderList[row] > patt then
						Module.OrderList[row] := Module.OrderList[row] + 1;

				for row := Module.Info.OrderCount-1 downto 0 do
					if Module.OrderList[row] = patt then
					begin
						Module.OrderList.Insert(row+1, patt+1);
						Inc(Module.Info.OrderCount);
					end;

				Inc(patt);
				Patterns.Insert(patt, NewPattern);
				Inc(Module.Info.PatternCount);
			end;

			Log(S);
			S := '';
		end;

		Inc(patt);
	end;

	totalchannels := 0;
	PC := Patterns.Count;

	if PC > MAX_PATTERNS then
	begin
		Log('Patterns above 100 will be lost.');
		PC := MAX_PATTERNS;
	end;

	for patt := 0 to PC-1 do
	begin
		Pattern := Patterns[patt];
		//Log(' -Pattern %d len=%d chans=%d', [patt, Pattern.Rows, Pattern.UsedChannels]);

		totalchannels := Max(Pattern.UsedChannels, totalchannels);

		// if pattern has less than 64 rows, try to insert pattern break command
		if Pattern.Rows < 64 then
		begin
			if Pattern.FindFreeEffectSlot(Pattern.Rows-1, SrcNote, OW_VOLUME_NONZERO) then
			begin
				SrcNote.Command := $D;
				SrcNote.Parameter := $00;
				Log('Inserted pattern break at pattern %2d, row %2d.', [patt, Pattern.Rows]);
			end
			else
				Log(TEXT_WARNING + 'Pattern %2d has %3d rows, ' +
				'but couldn''t find slot for pattern break!', [patt, Pattern.Rows]);
		end;


		for chan := 0 to AMOUNT_CHANNELS-1 do
		begin
			for row := 0 to Min(63, Pattern.Rows-1) do
			begin
				SrcNote := @Pattern.Notes[chan, row];
				DstNote := @Module.Notes[patt, chan, row];

				DstNote.Pitch     := SrcNote.Pitch;
				DstNote.Command   := SrcNote.Command;
				DstNote.Parameter := SrcNote.Parameter;
				DstNote.Sample    := SrcNote.Instrument;
			end;
		end;

	end;

	{if Conversion.Missed.Notes > 0 then
		Log(TEXT_WARNING + 'Module contains notes above B-3!');}

	S := '';

	with Conversion do
	begin
		Missed.Tracks := Max(totalchannels - AMOUNT_CHANNELS, 0);

		if Missed.Notes > 0 then
			S := S + Format('%d notes, ', [Missed.Notes]);
		if Missed.Effects > 0 then
			S := S + Format('%d effects, ', [Missed.Effects]);
		if Missed.Volumes > 0 then
			S := S + Format('%d volumes, ', [Missed.Volumes]);
		if Missed.VolEffects > 0 then
			S := S + Format('%d voleffects, ', [Missed.VolEffects]);
		if Missed.Tracks > 0 then
			S := S + Format('%d channels, ', [Missed.Tracks]);

		if S <> '' then
		begin
			S := Copy(S, 1, Length(S)-2);
			Log(TEXT_WARNING + S + ' lost in conversion.');
		end;

		if VolEffects > 0 then
			Log('%d volume column effects converted.', [VolEffects]);
	end;
end;

// Process imported data after showing import options dialog
procedure TImportedModule.Finish;
var
	Pattern: TExtPattern;
	c, r: Integer;
	Note: PExtNote;
begin
	// convert note pitches and effects from IT to PT
	for Pattern in Patterns do
		for c := 0 to Pattern.UsedChannels-1 do
			for r := 0 to Pattern.Rows-1 do
			begin
				Note := @Pattern.Notes[c, r];
				ConvertCommand(Note^);
			end;

	// insert speed/tempo command at first pattern in orderlist if required
	if (Conversion.Want.InsertTempo) and (Patterns.Count >= Module.OrderList[0]) then
		Patterns[Module.OrderList[0]].InsertTempoEffect(Module,
			Conversion.Info.OrigTempo, Conversion.Info.OrigSpeed);

	// convert intermediate format patterns to ProTracker format
	ProcessConvertedPatterns;
end;

{ TExtPattern }

constructor TExtPattern.Create(NumChannels, NumRows: Byte);
begin
	inherited Create;

	with EmptyNote do
	begin
		Instrument := 0;
		Pitch := 0;
		Command := 0;
		Parameter := 0;
		Volume := 64;
	end;

	Resize(NumChannels, NumRows, True);
end;

procedure TExtPattern.Resize(NumChannels, NumRows: Byte; Clear: Boolean = False);
var
	c, r: Integer;
begin
	Channels := NumChannels;
	c := Max(NumChannels, AMOUNT_CHANNELS-1);

	Rows := NumRows;
	UsedChannels := 0;

	SetLength(Notes, c, NumRows);

	if Clear then
	begin
		for c := 0 to c-1 do
			for r := 0 to Rows-1 do
				Notes[c, r] := EmptyNote;
	end;
end;

function TExtPattern.FindFreeEffectSlot(row: Byte; var Note: PExtNote;
	AllowOverwrite: Byte = OW_NONE): Boolean;
var
	chan: Integer;
	TmpNote: PExtNote;
begin
	Result := False;

	for chan := AMOUNT_CHANNELS-1 downto 0 do
	begin
		TmpNote := @Notes[chan, row];
		if (TmpNote.Command = 0) and (TmpNote.Parameter = 0) then
		begin
			Note := TmpNote;
			Exit(True);
		end;
	end;

	// not found
	if AllowOverwrite = OW_NONE then Exit;

	// overwrite a volume command instead if wanted
	for chan := AMOUNT_CHANNELS-1 downto 0 do
	begin
		TmpNote := @Notes[chan, row];
		if (TmpNote.Command = $C) then
		begin
			if (AllowOverwrite = OW_VOLUME_NONZERO) and
				(TmpNote.Parameter = $00) then Exit; // don't overwrite C00?
			Note := TmpNote;
			Exit(True);
		end;
	end;
end;

function TExtPattern.InsertTempoEffect(var Module: TPTModule; tempo, speed: Byte): Boolean;
var
	channel: Integer;
	Note: PExtNote;
begin
	Result := False;

	Log('Default speed=%d, tempo=%d.', [speed, tempo]);

	if (tempo = 125) and (speed = 6) then Exit;

	// see if tempo command already exists at pattern start
	for channel := AMOUNT_CHANNELS-1 downto 0 do
	begin
		Note := @Notes[channel, 0];
		if (Note.Command = $F) and (Note.Parameter <> 0) then
			Exit;
	end;

	if tempo <> 125 then
	begin
		if FindFreeEffectSlot(0, Note, OW_VOLUME_ANY) then
		begin
			Note.Command := $F;
			Note.Parameter := tempo;
			Log('Inserted tempo command F%s at pattern %d.', [IntToHex(tempo, 2), Index]);
		end
		else
			Log(TEXT_WARNING + 'Couldn''t find slot to insert tempo command!');
	end;

	if speed <> 6 then
	begin
		if FindFreeEffectSlot(0, Note, OW_VOLUME_ANY) then
		begin
			Note.Command := $F;
			Note.Parameter := speed;
			Log('Inserted speed command F%s at pattern %d.', [IntToHex(speed, 2), Index]);
		end
		else
			Log(TEXT_WARNING + 'Couldn''t find slot to insert speed command!');
	end;
end;

{ Processing }

// Splits long patterns into ProTracker sizes and updates orderlist;
// inserts pattern break fx as appropriate; fills in effect values for
// effects that don't have memory in PT.
// Notedata in input should already use ProTracker effects and limitations.
//

end.
