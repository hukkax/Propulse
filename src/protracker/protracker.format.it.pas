unit ProTracker.Format.IT;

// Impulse Tracker loader for Propulse Tracker
// TODO: select channels to convert if >4

interface

uses
	FileStreamEx, hkaFileUtils,
	ProTracker.Util, ProTracker.Player, ProTracker.Sample;

	procedure	LoadImpulseTracker(var Moduli: TPTModule; var ModFile: TFileStreamEx;
				SamplesOnly: Boolean = False);
	procedure	ReadITSample(var Moduli: TPTModule; var ModFile: TFileStreamEx;
				i: Byte; var ips: TImportedSample);
	function 	ReadbitsIT(var ModFile: TFileStreamEx;
				n: Byte; var srcpos, bitbuf, bitnum: Cardinal): Cardinal;
	function 	DecompressIT(var ModFile: TFileStreamEx; dest: PArrayOfShortInt;
				len: Cardinal; it215, sixteenbit: Boolean; channels, index: Byte): Cardinal;

implementation

uses
	SysUtils, Math,
	CWE.Core, Screen.Log,
	soxr, ProTracker.Import;

const
	// Pattern unpacking
	PP_NOTE              = 1;
	PP_INSTRUMENT        = 2;
	PP_VOLUME            = 4;
	PP_COMMAND           = 8;
	PP_MASK_NOTE         = 16;
	PP_MASK_INSTRUMENT   = 32;
	PP_MASK_VOLUME       = 64;
	PP_MASK_COMMAND      = 128;
	PP_NOTE_OFF          = 255;
	PP_NOTE_CUT          = 254;

	CMD_NONE = 0;
	CMD_A = 1;	CMD_B = 2;
	CMD_C = 3;	CMD_D = 4;
	CMD_E = 5;	CMD_F = 6;
	CMD_G = 7;	CMD_H = 8;
	CMD_I = 9;	CMD_J = 10;
	CMD_K = 11;	CMD_L = 12;
	CMD_M = 13;	CMD_N = 14;
	CMD_O = 15;	CMD_P = 16;
	CMD_Q = 17;	CMD_R = 18;
	CMD_S = 19;	CMD_T = 20;
	CMD_U = 21;	CMD_V = 22;
	CMD_W = 23;	CMD_X = 24;
	CMD_Y = 25;	CMD_Z = 26;

var
	PrevParam: array[0..255] of Byte;


procedure ConvertITCommand(var Note: TExtNote; var Conversion: TConversion);
const
	NOTETRANSPOSE = -48;
	ProtectedEffects = [$B, $D, $F];
	VolumeOverridesEffects = [$0, $A, $B, $E];
var
	C, P: Byte;
	V: Integer;

	procedure SetNote(Cmd: Byte; Prm: Integer = -1);
	begin
		C := Cmd;
		if Prm in [0..255] then
			P := Prm
		else
			P := Note.Parameter;
		if (Cmd > 0) and (Cmd <> $C) then
		begin
			if P = 0 then
				P := PrevParam[Cmd]
			else
				PrevParam[Cmd] := P;
		end;
	end;

	procedure SetNoteEx(Eff: Byte);
	begin
		C := $E;
		P := (Eff shl 4) or (P and $0F);
	end;

begin
	// convert pitch
	V := Note.Pitch;

	if InRange(V, 1, 119) then
		V := Max(V + NOTETRANSPOSE + 1, 0)
	else
	if V >= 254 then
		V := $FF // note off/note cut
	else
		V := 0;

	Note.Pitch := V;

	C := Note.Command;
	P := Note.Parameter;

	case Note.Command of 		// 0=no effect, 1=A ...

		0:		;
		CMD_A:	SetNote($F);		// Set speed
		CMD_B:	SetNote($B);		// Jump to order
		CMD_C:	begin				// Break to row
					// 16-based to 10-based
					SetNote($D, (P div 10) shl 4 or (P mod 10));
				end;
		CMD_D:	if (P >= $F1)  //(((P shr 4) = $F) and ((P and 4) <> 0))
				or (((P and 4) = $F) and (P >= $1F)) then //((P shr 4) <> 0)) then
				SetNoteEx($B)		// Fine volume slide
				else
				SetNote($A);		// Volume slide
		CMD_E:	if P < $F0 then
					SetNote($2)		// Pitch slide down
				else
					SetNoteEx($2);	// Fine pitch slide down
		CMD_F:	if P < $F0 then
					SetNote($1)		// Pitch slide up
				else
					SetNoteEx($1);	// Fine pitch slide up
		CMD_G:	SetNote($3);		// Slide to note
		CMD_H:	SetNote($4);		// Vibrato
		CMD_J:	SetNote($0);		// Arpeggio
		CMD_K:	SetNote($6);		// Vibrato + Volumeslide
		CMD_L:	SetNote($5);		// Slide to note + Volumeslide
		CMD_O:	SetNote($9);		// Set sample offset
		CMD_Q:	SetNoteEx($9);		// Retrigger note
		CMD_R:	SetNote($7);		// Tremolo (??)
		CMD_T:	SetNote($F);		// Set tempo
		CMD_S:	case ((P and $F0) shr 4) of
				$0:	SetNoteEx($C);	// Set filter
				$1:	SetNoteEx($3);	// Set glissando ctrl
				$2: SetNoteEx($5);	// Set finetune
				$3:	SetNoteEx($4);	// Set vibrato waveform
				$4: SetNoteEx($7);	// Set tremolo waveform
				$6: SetNoteEx($E);	// Pattern delay for x ticks
				$B: SetNoteEx($6);	// Set loopback point / Loop x times to loopback point
				$C: SetNoteEx($C);	// Note cut after x ticks
				$D: SetNoteEx($D);	// Note delay for x ticks
			else
				Inc(Conversion.Missed.Effects);
				//Log(TEXT_WARNING + 'Unimplemented command: ' + Chr(C + Ord('A') - 1) + IntToHex(P, 2) );
				C := 0;
				P := 0;
			end;
	else
		Inc(Conversion.Missed.Effects);
		//Log(TEXT_WARNING + 'Unimplemented command: ' + Chr(C + Ord('A') - 1) + IntToHex(P, 2) );
		C := 0;
		P := 0;
	end;

	// effects take priority over volume
	if Note.Volume > 64 then
	begin
		if C <> 0 then
			Inc(Conversion.Missed.VolEffects)
		else
		begin
			P := Note.Volume and $F;
			Inc(Conversion.VolEffects);
			// effect memory is ignored!
			case Note.Volume of
				65..74:		// Fine volume up
					SetNoteEx($A);
				75..84:		// Fine volume down
					SetNoteEx($B);
				85..94:		// Volume slide up ???
					SetNote($A, P shl 4);
				95..104:	// Volume slide down ???
					SetNote($A, P);
				105..114:	// Pitch slide down
					SetNote($2, P);
				115..124:	// Pitch slide up
					SetNote($1, P);
				193..202:	// Portamento to
					SetNote($3, P);
				203..212:	// Vibrato
					SetNote($4, P);
			else
				Inc(Conversion.Missed.VolEffects);
				Dec(Conversion.VolEffects);
			end;
		end;
	end
	else
	if Note.Volume < 64 then
	begin
		if C in VolumeOverridesEffects then // volume fx takes priority over fades
			SetNote($C, Note.Volume)
		else
			Inc(Conversion.Missed.Volumes);
	end;

	// emulate note cut/note off by setting volume to 0
	if Note.Pitch = $FF then
	begin
		Note.Pitch := 0;
		if C in ProtectedEffects then // don't discard important fx!
		begin
			Log(TEXT_WARNING + 'Discarded note cut command!');
			Inc(Conversion.Missed.Effects); // !!! does this classify as an effect?
		end
		else
			SetNote($C, $00); // !!! warn if any command was overwritten?
	end;

	Note.Command := C;
	Note.Parameter := P;
end;

// ModFile must be located just after 'IMPS'!
procedure ReadITSample(var Moduli: TPTModule; var ModFile: TFileStreamEx;
	i: Byte; var ips: TImportedSample);
var
	s: TSample;
	flag: Byte;
	j: Integer;
	c5speed, numsamples, sflag: Cardinal;
	FloatBuf: TFloatArray;
begin
	ModFile.Skip(12); // skip dos filename
	//Log('Offset 0x%s', [IntToHex(ModFile.Position, 6)] );

	//0010: 00h GvL Flg Vol Sample Name, max 26 bytes
	if ips = nil then
		s := Moduli.Samples[i]
	else
	begin
		s := ips;
		//Moduli.ImportInfo.Samples.Add(ips);
	end;

	ModFile.Read16; // zero byte, global volume

	flag := ModFile.Read8; // flags
	s.Volume := Min(ModFile.Read8, 64); // default volume

	{if BitGet(flag, 2) then
		Log(TEXT_ERROR + 'Unhandled: Sample %d is a stereo sample!', [i]);}

	for j := 0 to 21 do // sample name
		s.Name[j] := AnsiChar(Max(32, ModFile.Read8));

	//ModFile.SeekTo(os + $30 - 2);
	ModFile.Skip(4);

	ModFile.Read8; 	// sample format, bit 0 = signed?
	ModFile.Read8; 	// skip panning

	{ Flag:		Bit 0. On = Sample exists
				Bit 1. On = 16 bit
				Bit 2. On = Stereo sample
				Bit 3. On = Compressed sample
				Bit 4. On = Use loop
	}
	if BitGet(flag, 0) then // sample used
	begin
		numsamples := ModFile.Read32;
		if ips <> nil then
			ips.OrigSize := numsamples;
		s.Length := Min(numsamples div 2, $FFFF); // sample length

		if BitGet(flag, 4) then // use loop
		begin
			s.LoopStart := (ModFile.Read32 div 2) and $FFFF; // loop start
			s.LoopLength := ((ModFile.Read32 div 2) and $FFFF) - s.LoopStart; // loop end
		end
		else
		begin
			ModFile.Skip(8);
			s.LoopStart := 0;
			s.LoopLength := 1;
		end;

		//C5Speed:  Number of bytes a second for C-5 (ranges from 0->9999999)
		c5speed := ModFile.Read32;
		if (s is TImportedSample) then
			TImportedSample(s).C5freq := c5speed;

		//Value:    0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
		//Finetune: 0  +1  +2  +3  +4  +5  +6  +7  -8  -7  -6  -5  -4  -3  -2  -1
		case c5Speed of
			7800..7895: s.FineTune := $8; // -8
			7896..7941: s.FineTune := $9; // -7
			7942..7985: s.FineTune := $A; // -6
			7986..8046: s.FineTune := $B; // -5
			8047..8107: s.FineTune := $C; // -4
			8108..8169: s.FineTune := $D; // -3
			8170..8232: s.FineTune := $E; // -2
			8233..8280: s.FineTune := $F; // -1
			//
			8364..8413: s.FineTune := 1; // +1
			8414..8463: s.FineTune := 2; // +2
			8464..8529: s.FineTune := 3; // +3
			8530..8581: s.FineTune := 4; // +4
			8582..8651: s.FineTune := 5; // +5
			8652..8723: s.FineTune := 6; // +6
			8724..8757: s.FineTune := 7; // +7
		else
			s.FineTune := 0;
		end;

//		s.SetName(Format('%d = %d', [c5speed, s.Finetune]));
(*		s.SetName(Inttostr(Round(
			12*128*log2(c5speed/8363)
			{1536.0 * (Math.Log2(c5speed / 8363.0) / Math.Log2(2)) /12}
		)));*)

		ModFile.Skip(8);
//		ModFile.SeekTo(os + $48);
		ModFile.SeekTo(ModFile.Read32); // jump to sample data

		// read sample format
		//
		sflag := 0;

		if BitGet(flag, 1) then
			sflag := sflag or ST_16
		else
			sflag := sflag or ST_8;
		if ips <> nil then
			ips.Is16Bit := BitGet(flag, 1);

		if BitGet(flag, 2) then
			sflag := sflag or ST_STEREO
		else
			sflag := sflag or ST_MONO;
		if ips <> nil then
			ips.IsStereo := BitGet(flag, 2);

		if BitGet(flag, 3) then
			sflag := sflag or ST_IT214
		else
			sflag := sflag or ST_SIGNED;
		if ips <> nil then
			ips.IsPacked := BitGet(flag, 3);

		// load (and unpack) sample data 		16574 = C-3
		//
		if 	(SOXRLoaded) and (Options.Import.Resampling.Enable) and
			(c5speed >= Options.Import.Resampling.ResampleFrom) then
		begin
			// resample automatically
			s.LoadDataFloat(ModFile, numsamples, sflag, FloatBuf);
			sflag := PeriodToHz(PeriodTable[Options.Import.Resampling.ResampleTo]);
			Log('Resampling sample #%2d from %d to %d Hz', [s.Index, c5speed, sflag]);
			s.ResampleFromBuffer(FloatBuf, c5speed, sflag,
				SOXRQuality[Options.Import.Resampling.Quality],
				Options.Import.Resampling.Normalize,
				Options.Import.Resampling.HighBoost
				);
		end
		else
			s.LoadData(ModFile, numsamples, sflag);

		if (s.LoopStart = 0) and (s.LoopLength <= 2) then
		begin
			s.Data[0] := 0;
			s.Data[1] := 0;
		end;
	end;
end;

procedure LoadImpulseTracker(var Moduli: TPTModule; var ModFile: TFileStreamEx;
	SamplesOnly: Boolean = False);
var
	os, i, j, V, pattend, row, channel, rowcount: Integer;
	ips: TImportedSample;
	Note: PExtNote;

	RowNotes: array[0..63] of TExtNote;

	SamPtrs: array of Cardinal;
	PattPtrs: array[0..MAX_PATTERNS] of Cardinal;

	flag, channelvariable, maskvariable: Byte;
	prevParameter, previousmaskvariable: array[0..63] of Byte;
	Done: Boolean;

	Pattern: TExtPattern;
	Patterns: TExtPatternList;

	Conversion: TConversion;
begin
	for i := 0 to 255 do
		PrevParam[i] := 0;

	ZeroMemory(@Conversion, SizeOf(Conversion));

	with Conversion.Want do
	begin
		InsertTempo := True;		// insert tempo effect to first pattern?
		InsertPattBreak := True;	// add pattern breaks to patterns < 64 rows?
		FillParams := True;			// compensate for PT's lack of effect memory in some fx?
	end;

	if not SamplesOnly then
	begin
		ChangeScreen(TCWEScreen(LogScreen));
		Log('$6Importing Impulse Tracker Module.');

		Patterns := TExtPatternList.Create(True);

		ModFile.SeekTo($04);
		ModFile.Read(Moduli.Info.Title[0], 20);
	end;

	ModFile.SeekTo($20); // 0020: OrdNum InsNum SmpNum PatNum Cwt/v Cmwt Flags Special

	Moduli.Info.OrderCount := ModFile.Read16 - 1; // # of Orders
	j  := ModFile.Read16; // # of Instruments
	os := ModFile.Read16; // # of Samples
	Moduli.Info.PatternCount := Min(ModFile.Read16-1, MAX_PATTERNS-1); // # of Patterns

	if not SamplesOnly then
		Log('%d samples and %d patterns.', [os, Moduli.Info.PatternCount+1]);

	ModFile.Read16; // Cwt:  Created with tracker. Impulse Tracker y.xx = 0yxxh
	ModFile.Read16; // Cmwt: Compatible with tracker version > value. (ie. format version)

	flag := Byte(ModFile.Read16); // flags
	if BitGet(flag, 2) then // Bit 2: On = Use instruments, Off = Use samples
	begin
		if not SamplesOnly then
			Log(TEXT_WARNING + 'Instrument definitions are unsupported.');
	end;

	// read orderlist
	//
	for i := 0 to 127 do
		Moduli.OrderList[i] := 0;

	ModFile.SeekTo($C0);

	row := 0;
	for i := 0 to Moduli.Info.OrderCount-1 do
	begin
		channel := ModFile.Read8;
		if channel < MAX_PATTERNS then
		begin
			Moduli.OrderList[row] := channel;
			Inc(row);
		end;
	end;
//	Moduli.Info.OrderCount := row;
	ModFile.Read8;

	// skip instrument pointers
	if j > 0 then
		for i := 0 to j-1 do
			ModFile.Read32;

	// read sample offsets
	//
	SetLength(SamPtrs, os);
	channel := os - 1;
	for i := 0 to channel do
		SamPtrs[i] := ModFile.Read32;

	// read pattern offsets
	//
	for i := 0 to Min(Moduli.Info.PatternCount, MAX_PATTERNS-1) do
		PattPtrs[i] := ModFile.Read32;

	if SamplesOnly then
		Moduli.ImportInfo.Samples.Clear
	else
		channel := Min(channel, 30);

	// read samples
	//
	for i := 0 to channel do
	begin
		ips := nil;
		os := SamPtrs[i];
		ModFile.SeekTo(os);

		if ModFile.ReadString(False, 4) <> 'IMPS' then
		begin
			if not SamplesOnly then
				Log(TEXT_ERROR + 'Invalid sample (%d) at offset 0x%s', [i, IntToHex(os, 6)] );
			Continue;
		end;

		if SamplesOnly then
		begin
			ips := TImportedSample.Create;
			Moduli.ImportInfo.Samples.Add(ips);
		end
		else
			ips := nil;

		ReadITSample(Moduli, ModFile, i, ips);
	end;

	if SamplesOnly then
		Exit;

	// read and convert pattern data
	//
	for i := 0 to Min(Moduli.Info.PatternCount, MAX_PATTERNS-1) do
	begin
		if PattPtrs[i] = 0 then
		begin
			Pattern := TExtPattern.Create(AMOUNT_CHANNELS, 64);
			Patterns.Add(Pattern);
			Continue;
		end;

		ModFile.SeekTo(PattPtrs[i]);

		pattend := ModFile.Position + 8 + ModFile.Read16; // packed data length
		rowcount := ModFile.Read16; // rows

		if rowcount > 200 then
		begin
			Log(TEXT_WARNING + 'Pattern %d has invalid row count, skipped! (@%d)', [i, PattPtrs[i]]);
			Continue; // invalid value
		end;

		Pattern := TExtPattern.Create(64, rowcount);
		Patterns.Add(Pattern);

		ModFile.Skip(4);

		for channel := 0 to 63 do
			PrevParameter[channel] := 0;

		channel := 0;
		row := 0;
		Done := False;

		while not Done do
		begin
			channelVariable := 1;

			// unpack a row
			while (channelVariable > 0) and (ModFile.Position < pattend) do
			begin
				channelvariable := ModFile.Read8;
				if channelvariable = 0 then
					Continue // end of row
				else
				begin
					channel := (channelvariable-1) and 63;
					if channel >= Pattern.UsedChannels then
						Pattern.UsedChannels := channel + 1;
				end;

				Note := @Pattern.Notes[channel, row];

				if (channelvariable and 128) <> 0 then
				begin
					maskvariable := ModFile.Read8;
					previousmaskvariable[channel] := maskvariable;
				end
				else
					maskvariable := previousmaskvariable[channel];

				if (maskvariable and PP_NOTE) > 0 then
				begin
					V := ModFile.Read8;
					RowNotes[channel].Pitch := V;
					Note.Pitch := V;
				end;

				if (maskvariable and PP_INSTRUMENT) > 0 then
				begin
					V := Min(ModFile.Read8, 31);
					RowNotes[channel].Instrument := V;
					Note.Instrument := V;
				end;

				if (maskvariable and PP_VOLUME) > 0 then
				begin
					V := ModFile.Read8;
					//if V <= 64 then // volume, not panning or fx
					begin
						RowNotes[channel].Volume := V;
						Note.Volume := V;
					end;
				end;

				if (maskvariable and PP_COMMAND) > 0 then
				begin
					RowNotes[channel].Command   := ModFile.Read8;
					RowNotes[channel].Parameter := ModFile.Read8;
					Note.Command := RowNotes[channel].Command;
					Note.Parameter := RowNotes[channel].Parameter;

					if not ((Note.Command in [CMD_D, CMD_J, CMD_E, CMD_F]) and (Note.Parameter = 0)) then
						PrevParameter[channel] := Note.Parameter;
				end;

				if (maskvariable and PP_MASK_NOTE) <> 0 then
					Note.Pitch := RowNotes[channel].Pitch;

				if (maskvariable and PP_MASK_INSTRUMENT) <> 0 then
					Note.Instrument := RowNotes[channel].Instrument;

				if (maskvariable and PP_MASK_VOLUME) <> 0 then
					Note.Volume := RowNotes[channel].Volume;

				if (maskvariable and PP_MASK_COMMAND) <> 0 then
				begin
					Note.Command   := RowNotes[channel].Command;
					Note.Parameter := RowNotes[channel].Parameter;
				end;

				if (Note.Command in [CMD_D, CMD_J, CMD_E, CMD_F]) and (Note.Parameter = 0) then
					Note.Parameter := PrevParameter[channel];
			end;

			Inc(row);
			if (row >= rowcount) or (ModFile.Position >= pattend) then // pattern done
			begin
				// convert note pitches and effects from IT to PT before additional processing
				for channel := 0 to Pattern.UsedChannels-1 do
					for row := 0 to Pattern.Rows-1 do
					begin
						Note := @Pattern.Notes[channel, row];
						ConvertITCommand(Note^, Conversion);
					end;

				channel := 0; row := 0; // shut up compiler

				Done := True;
			end;

		end; // pattern unpacking
	end;

	// insert speed/tempo command at first pattern in orderlist if required
	if (Conversion.Want.InsertTempo) and (Patterns.Count >= Moduli.OrderList[0]) then
	begin
		ModFile.SeekTo($32);
		Patterns[Moduli.OrderList[0]].InsertTempoEffect(Moduli, ModFile.Read8, ModFile.Read8);
	end;

	// convert intermediate format patterns to ProTracker format
	ProcessConvertedPatterns(Moduli, Patterns, Conversion);

	Patterns.Free;
end;

// ==========================================================================
// Impulse Tracker packed sample decompression
// ==========================================================================

{$R-}

function ReadbitsIT(var ModFile: TFileStreamEx;
	n: Byte; var srcpos, bitbuf, bitnum: Cardinal): Cardinal;
var
	i: Integer;
	value: Cardinal;
begin
	value := 0;
	i := n;

	while (i > 0) do
	begin
		if bitnum = 0 then
		begin
			bitbuf := ModFile.Read8;
			Inc(srcpos);
			bitnum := 8;
		end;
		value := value shr 1;
		value := value or (bitbuf shl 31);
		bitbuf := bitbuf shr 1;
		Dec(bitnum);

		Dec(i);
	end;

	Result := value shr (32 - n);
end;

function DecompressIT(var ModFile: TFileStreamEx; dest: PArrayOfShortInt;
	len: Cardinal; it215, sixteenbit: Boolean; channels, index: Byte): Cardinal;
var
	filelen: Cardinal;
	srcpos: Cardinal;
	destpos: Cardinal;             // position in destination buffer which will be returned
	blklen: uint16;                // length of compressed data block in samples
	blkpos: uint16;                // position in block
	width: uint8;                  // actual "bit width"
	value: Cardinal;               // value read from file to be processed
	d1, d2: int8;                  // integrator buffers (d2 for it2.15) (8-bit)
	d1x, d2x: int16;               // integrator buffers (16-bit)
	v: int8;                       // sample value (8-bit)
	vx: int16;                     // sample value (16-bit)
	bitbuf, bitnum: uint32;        // state for it_readbits
	shift,
	border: uint8;
	borderx: uint16;
	maxlen: Cardinal;
begin
	if sixteenbit then
		Log('Decompressing sample %d (16-bit)', [index])
	else
		Log('Decompressing sample %d (8-bit)', [index]);

	destpos := 0;
	maxlen := len;

	srcpos := ModFile.Position-1;
	filelen := ModFile.Size;
	Result := srcpos;

	// unpack data till the dest buffer is full
	while len > 0 do
	begin
		// read a new block of compressed data and reset variables
		// block layout: word size, <size> bytes data

		if ((srcpos + 2) >= filelen) then
		begin
			Log('Sample %d truncated at %d (position > %d)', [index, srcpos, filelen]);
			Exit(ModFile.Position); // truncated!
		end;

		blklen := ModFile.Read16;
		if ((srcpos + blklen) > filelen) then
		begin
			Log('Sample %d truncated at %d (%d > %d)', [index, srcpos, blklen, filelen]);
			Exit(ModFile.Position); // truncated!
		end;

		Inc(srcpos, 2);
		bitbuf := 0;
		bitnum := 0;
		blkpos := 0;
		d1x := 0; d2x := 0; // reset integrator buffers
		d1  := 0; d2  := 0;

		//Log('  Reading block (blocksize=%d, width=%d)', [blklen, width]);

		if sixteenbit then
		begin
			blklen := Min($4000, len);
			width := 17;
		end
		else
		begin
			blklen := Min($8000, len);
			width := 9;
		end;

		// now uncompress the data block
		while blkpos < blklen do
		begin
			if sixteenbit then
			begin
				if width > 17 then
				begin
					Log(TEXT_WARNING + 'Sample %d: Illegal bit width %d' +
						' for 16-bit sample!', [index, width]);
					Exit(ModFile.Position);
				end;

				value := ReadbitsIT(ModFile, width, srcpos, bitbuf, bitnum);

				// method 1 (1-6 bits)
				if width < 7 then
				begin
					// check for "100..."
					if (int32(value) = (1 shl (width - 1))) then
					begin // yes!
						value := ReadbitsIT(ModFile, 4, srcpos, bitbuf, bitnum) + 1; // read new width
						if value < width then // and expand it
							width := value
						else
							width := value + 1;
						Continue; // ... next value
					end;
				end
				else
				// method 2 (7-16 bits)
				if width < 17 then
				begin
					borderx := ($FFFF shr (17 - width)) - 8; // lower border for width chg
					//if (value > borderx) and (value <= borderx + 16) then
					if (int32(value) > int32(borderx)) and (int32(value) <= int32(borderx) + 16) then
					begin
						Dec(value, borderx); // convert width to 1-8
						if value < width then // and expand it
							width := value
						else
							width := value + 1;
						Continue; // ... next value
					end;
				end
				else
				// method 3 (17 bits)
				if (value and $10000) <> 0 then // bit 8 set?
				begin
					width := (value + 1) and $FF; // new width...
					Continue; // ... and next value
				end;

				// now expand value to signed word
				if width < 16 then
				begin
					shift := 16 - width;
					vx := Word(value shl shift);
					vx := Word(vx shr shift);
				end
				else
					vx := int16(value);

				// integrate upon the sample values
				Inc(d1x, vx);
				Inc(d2x, d1x);

				// .. and store it into the buffer
				if destpos < maxlen then
				begin
					if it215 then
						dest[destpos] := d2x div 256
					else
						dest[destpos] := d1x div 256;
				end
				else
					Exit;

			end	// 16-bit
			else
			begin
				if width > 9 then
				begin
					Log(TEXT_WARNING + 'Sample %d: Illegal bit width %d' +
						' for 8-bit sample!', [index, width]);
					Exit(ModFile.Position);
				end;

				value := ReadbitsIT(ModFile, width, srcpos, bitbuf, bitnum);

				// method 1 (1-6 bits)
				if width < 7 then
				begin
					// check for "100..."
					if (value = (1 shl (width - 1))) then
					begin // yes!
						value := ReadbitsIT(ModFile, 3, srcpos, bitbuf, bitnum) + 1; // read new width
						if value < width then // and expand it
							width := value
						else
							width := value + 1;
						Continue; // ... next value
					end;
				end
				else
				// method 2 (7-8 bits)
				if width < 9 then
				begin
					border := ($FF shr (9 - width)) - 4; // lower border for width chg
					if (value > border) and (value <= (border + 8)) then
					begin
						Dec(value, border); // convert width to 1-8
						if value < width then // and expand it
							width := value
						else
							width := value + 1;
						Continue; // ... next value
					end;
				end
				else
				// method 3 (9 bits)
				if width = 9 then
				begin
					if (value and $100) <> 0 then // bit 8 set?
					begin
						width := (value + 1) and $FF; // new width...
						Continue; // ... and next value
					end;
				end;

				// now expand value to signed byte
				if width < 8 then
				begin
					shift := 8 - width;
					Byte(v) := ShortInt(value shl shift);
					Byte(v) := ShortInt(v shr shift);
				end
				else
					v := ShortInt(value);

				// integrate upon the sample values
				Inc(d1, v);
				Inc(d2, d1);

				// .. and store it into the buffer
				if destpos < maxlen then
				begin
					if it215 then
						dest[destpos] := d2
					else
						dest[destpos] := d1;
				end
				else
					Exit;

			end; // 8-bit

			Inc(destpos, channels);
			Inc(blkpos);
		end;

		// now subtract block length from total length and go on
		Dec(len, blklen);
	end;

	//Log('Done, pos=%d, destpos=%d', [srcpos, destpos] );
	Result := srcpos;
end;

//{$R-}


end.
