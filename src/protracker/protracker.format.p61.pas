unit ProTracker.Format.P61;

// The Player 61A loader for PoroTracker
// based on code from:
//  ptmf.rs by Mathias Olsson - https://github.com/hypp/modfile/blob/master/src/ptmf.rs
//  p61a.c from libxmp by Asle/ReDoX and Claudio Matsuoka

interface

uses
	FileStreamEx,
	ProTracker.Player;

	procedure LoadThePlayer(var Module: TPTModule; var ModFile: TFileStreamEx;
			  SamplesOnly: Boolean = False);

implementation

uses
	SysUtils, hkaFileUtils,
	ProTracker.Util,
	ProTracker.Sample;

const
	DELTA_4BIT: array[0..15] of Byte = (
		$00, $01, $02, $04, $08, $10, $20, $40,
		$80, $c0, $e0, $f0, $f8, $fc, $fe, $ff
		);


function decode_p61_effect(effect_type, params: Byte; out effect: Word): Boolean;
var
	fixed_params: Byte;
begin
	effect_type := effect_type and $F;
	Result := False;

	if (effect_type in [$5, $6, $A]) and (params and $80 = $80) then
	begin
		// P61con changes two nibble commands to signed/unsigned
		// Let's change it back
		fixed_params := ($100 - params) shl 4;
		effect := (effect_type shl 8) or fixed_params;
	end
	else
	if effect_type =$8 then
	begin
		// P61con changes effect 0 to 8
		// Let's change it back
		effect := params;
	end
	else
	begin
		// End
		effect := (effect_type shl 8) or params;
		Result := (effect_type in [$B, $D]);
	end
end;

// Decode at least one row
// Returns true if the pattern ends here due to effect $D or $B
function decode_p61_row(var Module: TPTModule; data: PByte;
	var current_pos: Cardinal;
	pattern_number, channel_number: Byte; var row_number: Byte): Boolean;
var
	first_byte, second_byte, third_byte,
	ctype,
	num_empty_rows, repeat_count,
	noteno, instrument: Byte;
	i: Integer;
	new_pos: Cardinal;
	copy_offset,
	period, effect: Word;
	has_compression_info,
	is_empty,
	is_only_effect,
	is_no_effect: Boolean;
	Note: PNote;
const
	bits_compression_info = $80;
	bits_empty            = $7F;
begin
	Result := False;
	first_byte := data[current_pos];
	Inc(current_pos);

	// If first bit is set, has compression info
	has_compression_info := (first_byte and bits_compression_info) = bits_compression_info;

	// Is empty?
	is_empty := (first_byte and bits_empty) = bits_empty;

	// Only an effect, no note or instrument
	is_only_effect := (first_byte and $70) = $60;

	// No effect, but note and instrument
	is_no_effect := (first_byte and $78) = $70;

	if is_empty then
		Inc(row_number)
	else
	if is_only_effect then
	begin
		// Only effect command
		// One more byte
		second_byte := data[current_pos];
		Inc(current_pos);
		Result := decode_p61_effect(first_byte and $F, second_byte, effect);
		if (Result) and (has_compression_info) then
			Log(TEXT_WARNING + 'Should never happen! (Effect cmd)');
		Note := @Module.Notes[pattern_number, channel_number, row_number];
		Note.Command := (effect shr 8) and $FF;
		Note.Parameter := effect and $FF;
		Inc(row_number);
	end
	else
	if is_no_effect then
	begin
		// Note and instrument, but no effect
		second_byte := data[current_pos];
		Inc(current_pos);
		noteno := ((first_byte and 7) shl 3) or ((second_byte and $E0) shr 5);
		instrument := second_byte and 31;
		if noteno = 0 then
			period := 0
		else
			period := PeriodTable[noteno-1];
		Note := @Module.Notes[pattern_number, channel_number, row_number];
		Note.Period := period;
		Note.Sample := instrument;
		Note.Text := GetNoteText(Note.Period);
		Inc(row_number);
	end
	else
	begin
		// Full command
		// Two more bytes
		second_byte := data[current_pos];
		Inc(current_pos);
		third_byte := data[current_pos];
		Inc(current_pos);
		noteno := (first_byte and $7E) shr 1;
		instrument := ((first_byte and 1) shl 4) or ((second_byte and $F0) shr 4);
		Result := decode_p61_effect(second_byte and $F, third_byte, effect);
		if (Result) and (has_compression_info) then
			Log(TEXT_WARNING + 'Should never happen! (Full cmd)');
		if noteno = 0 then
			period := 0
		else
			period := PeriodTable[noteno-1];
		Note := @Module.Notes[pattern_number, channel_number, row_number];
		Note.Period := period;
		Note.Sample := instrument;
		Note.Command := (effect shr 8) and $FF;
		Note.Parameter := effect and $FF;
		Note.Text := GetNoteText(Note.Period);
		Inc(row_number);
	end;

	if has_compression_info then
	begin
		first_byte := data[current_pos];
		Inc(current_pos);
		ctype := first_byte and $C0;
		if ctype = $00 then
		begin
			// Empty rows
			num_empty_rows := first_byte and 63;
			Inc(row_number, num_empty_rows);
		end
		else
		if ctype = $80 then
		begin
			// Repeat current row n times
			repeat_count := first_byte and 63;
			Note := @Module.Notes[pattern_number, channel_number, row_number-1];
			for i := 0 to repeat_count-1 do
			begin
				Module.Notes[pattern_number, channel_number, row_number] := Note^;
				Inc(row_number);
			end;
		end
		else
		if ctype in [$40, $C0] then
		begin
			// Copy previous data from offset
			if is_empty then 	// Special handling when empty ... WTF
				Dec(row_number);
			repeat_count := (first_byte and 63) + 1;

			if ctype = $40 then
			begin
				// Copy previous data from 8-bit offset
				second_byte := data[current_pos];
				Inc(current_pos);
				copy_offset := second_byte;
			end
			else
			begin
				// Copy previous data from 16-bit offset
				second_byte := data[current_pos];
				Inc(current_pos);
				third_byte := data[current_pos];
				Inc(current_pos);
				copy_offset := (second_byte shl 8) or third_byte;
			end;

			new_pos := current_pos - copy_offset;

			// Recurse
			for i := 0 to repeat_count-1 do
				if decode_p61_row(Module, data, new_pos,
					pattern_number, channel_number, row_number)
				then Result := True;
		end
		else
			Log(TEXT_WARNING + 'Should never happen! (Unhandled)');
	end;
end;

// TODO: support SamplesOnly in P61A loader
procedure LoadThePlayer(var Module: TPTModule; var ModFile: TFileStreamEx;
	SamplesOnly: Boolean = False);
var
	row_number: array [0..3] of Byte;
	current_pos: array [0..3] of Cardinal;
	os: Cardinal;
	i, j, samplecount, row, channel, current_channel: Integer;
	s: TSample;
	SamPtr: Cardinal;
	SamPtrs: array[0..31] of Cardinal;
	taddr: array[0..127, 0..AMOUNT_CHANNELS-1] of Word;
	SamplePacked: array[0..31] of Boolean;
	SamLength: array [0..31] of Cardinal;
	delta: ShortInt;
	truncate_pos, lowest: Byte;
	usesD8, usesD4: Boolean;
	signed_sample_length: SmallInt;
	sname: AnsiString;
begin
	ModFile.SeekTo(0);

	if ModFile.ReadString(False, 4) = 'P61A' then
	begin
		Log('$6Importing The Player packed module.');
	end
	else
	begin
		Log('$6Attempting to import The Player packed module.');
		ModFile.SeekTo(0);
	end;

	SamPtr := ModFile.Read16R;						// offset to sample data
	SamPtrs[0] := 0;

	Module.Info.PatternCount := ModFile.Read8 - 1;	// # of Patterns
	samplecount := ModFile.Read8; 					// # of Samples

	usesD8 := BitGet(samplecount, 7);
	usesD4 := BitGet(samplecount, 6);
	samplecount := samplecount and 31;

	if usesD4 then
		ModFile.Read32R;					// total unpacked sample length

	Log('%d samples and %d patterns.', [samplecount, Module.Info.PatternCount+1]);

	// Read sample infos
	//
	for i := 0 to samplecount-1 do
	begin
		if SamplesOnly then
		begin
			s := TImportedSample.Create;
			Module.ImportInfo.Samples.Add(TImportedSample(s));
		end
		else
			s := Module.Samples[i];

		j := ModFile.Read16R;	// sample length in words

		//sname := Format('%2d: ', [i+1]);
		if (i > 0) and (j > $FF00) then
		begin
			s.Length := Module.Samples[$FFFF - j].Length;
			SamPtrs[i] := SamPtrs[$FFFF - j];
			SamLength[i] := 0;
			//sname := sname + Format('COPY %2d: ', [$FFFF-j]);
		end
		else
		begin
			if i > 0 then
				SamPtrs[i] := SamPtrs[i-1] + SamLength[i-1];
			s.Length := j;
			SamLength[i] := j * 2;
		end;

		j := ModFile.Read8;					// sample finetune
		SamplePacked[i] := BitGet(j, 7);	// is sample 4-bit delta packed?
		if SamplePacked[i] then
			SamLength[i] := SamLength[i] div 2;

		//sname := sname + Format('Ptr=%6d  Len=%6d', [SamPtrs[i], SamLength[i]]);
		//Log(sname);

		SetLength(s.Data, s.Length * 2 + 1);

		s.Finetune := j and $F;
		s.Volume := ModFile.Read8;
		j := ModFile.Read16R;			// sample repeat start in words
		if j = $FFFF then					// no loop
		begin
			s.LoopStart := 0;
			s.LoopLength := 1;
		end
		else
		begin
			s.LoopStart := j;
			s.LoopLength := s.Length - j;
		end;
	end;

	if not SamplesOnly then
	begin
		// Read pattern data pointers
		//
		for i := 0 to Module.Info.PatternCount do
		for j := 0 to AMOUNT_CHANNELS-1 do
			taddr[i,j] := ModFile.Read16R;

		// Read orderlist
		//
		for i := 0 to 127 do // terminated with $FF
		begin
			j := ModFile.Read8;
			if j > 127 then
			begin
				Module.Info.OrderCount := i;
				Break;
			end
			else
				Module.OrderList[i] := j;
		end;

		// read and convert pattern data
		//
		for i := 0 to Module.Info.PatternCount do
		begin
			for channel := 0 to 3 do
			begin
				row_number[channel] := 0;
				current_pos[channel] := ModFile.Position + taddr[i, channel];
			end;

			truncate_pos := 64;
			ModFile.ReadData; // read file contents to ModFile.Data[]

			while True do
			begin
				// We always need to decode the channel with the lowest row number first
				// to see if it contains a pattern break or jump
				current_channel := 0;
				lowest := 64;
				for channel := 0 to 3 do
				begin
					if row_number[channel] < lowest then
					begin
						lowest := row_number[channel];
						current_channel := channel;
					end;
				end;
				if lowest >= truncate_pos then
					Break;

				// This will make sure we exit the while loop early
				// but still process any remaining channels
				if decode_p61_row(Module, @ModFile.Data[0], current_pos[current_channel],
					i, current_channel, row_number[current_channel]) then
						truncate_pos := row_number[current_channel];
			end;

			// If we exit the while loop early above, make sure that we clear out any data
			// that we parsed but shouldn't really be there
			for row := truncate_pos to 63 do
				for channel := 0 to 3 do
					Module.Notes[i, channel, row] := EmptyNote;
		end;
	end;

	for i := 0 to samplecount-1 do
	begin
		ModFile.SeekTo(SamPtr + SamPtrs[i]);

		if SamplesOnly then
			s := Module.ImportInfo.Samples[i]
		else
			s := Module.Samples[i];

		delta := 0;
		os := 0;

		signed_sample_length := SmallInt(s.Length{ - 32768});
		//Log('%2d = %d', [i+1, signed_sample_length]);
		if (signed_sample_length < 0) and (signed_sample_length >= -31) then
		begin
			// The sample uses the same sample data as another sample
			delta := (-1 * signed_sample_length - 1);
			s.Length := Module.Samples[delta].Length;
			SetLength(s.Data, s.Length * 2 + 1);
			for j := 0 to s.Length*2 do
				s.Data[j] := Module.Samples[delta].Data[j];
			sName := Format('%d  ', [delta+1]);
			s.Name := 'Copy of sample 00';
			s.Name[15] := sName[1];
			s.Name[16] := sName[2];
			// Fix repeat length
			if s.LoopLength > 1 then
				s.LoopLength := s.Length - s.LoopStart;
		end
		else
		if (usesD4) and (SamplePacked[i]) then
		begin
			// unpack 4-bit delta
			//Log('Sample %2d is 4-bit delta packed.', [i+1]);
			s.Name := '4-bit delta packed';
			for j := 0 to s.Length-1 do
			begin
				lowest := ModFile.Read8;
				delta := delta - ShortInt(DELTA_4BIT[(lowest and $F0) shr 4]);
				s.Data[os] := Byte(delta);
				Inc(os);
				delta := delta - ShortInt(DELTA_4BIT[lowest and $0F]);
				s.Data[os] := Byte(delta);
				Inc(os);
			end
		end
		else
		if usesD8 then
		begin
			//Log('Sample %2d is 8-bit delta packed.', [i+1]);
			s.Name := '8-bit delta packed';
			delta := ShortInt(ModFile.Read8);
			for j := 0 to s.Length*2-1 do
			begin
				delta := ShortInt(delta - ShortInt(ModFile.Read8));
				s.Data[os] := Byte(delta);
				Inc(os);
			end;
		end
		else
		// unpacked sample
		begin
			s.Name := 'Unpacked';
			for j := 0 to s.Length*2-1 do
			begin
				s.Data[os] := ModFile.Read8;
				Inc(os);
			end;
		end;

		if (s.LoopLength < 2) and (s.LoopStart = 0) then
		begin
			s.Data[0] := 0;
			s.Data[1] := 0;
		end;
	end;

	if samplecount < 30 then
		Module.Samples[30].Name := 'propulse imported';
end;


end.
