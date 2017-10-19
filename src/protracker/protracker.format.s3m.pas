unit ProTracker.Format.S3M;

// Scream Tracker loader for Propulse Tracker
// TODO: select channels to convert if >4

interface

uses
	FileStreamEx, hkaFileUtils,
	ProTracker.Util, ProTracker.Player, ProTracker.Sample;

	procedure	LoadScreamTracker(var Moduli: TPTModule; var ModFile: TFileStreamEx;
				SamplesOnly: Boolean = False);
	procedure	ReadS3MSample(var Moduli: TPTModule; var ModFile: TFileStreamEx;
				i: Byte; var ips: TImportedSample);

implementation

uses
	SysUtils, Math,
	//soxr,
	CWE.Core, Screen.Log,
	ProTracker.Import;

const
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


procedure ConvertS3MCommand(var Note: TExtNote; var Conversion: TConversion);
const
	NOTETRANSPOSE = -48;
	ProtectedEffects = [$B, $D, $F];
var
	C, P, V: Byte;

	procedure SetNote(Cmd: Byte; Prm: Integer = -1);
	begin
		C := Cmd;
		if Prm in [0..255] then
			P := Prm
		else
			P := Note.Parameter;
		if {(Cmd > 0) and} (Cmd <> $C) then
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

	case Note.Command of 			// 0=no effect, 1=A ...

		0:		if P <> 0 then SetNote($0);
		CMD_A:	SetNote($F);		// Set speed
		CMD_B:	SetNote($B);		// Jump to order
		CMD_C:	SetNote($D, (P div 10) shl 4 or (P mod 10));	// Break to row; 16-based to 10-based
		CMD_D:	if (P >= $F1) or (((P and 4) = $F) and (P >= $1F)) then
					SetNoteEx($B)	// Fine volume slide
				else
					SetNote($A);	// Volume slide
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
		CMD_K:	begin				// Vibrato + Volumeslide
					if Note.Parameter > $F0 then // fine slides into normal slides
						Note.Parameter := Note.Parameter and $0F
					else
					if Note.Parameter and $F = $F then
						Note.Parameter := Note.Parameter and $F0;
					SetNote($6);
				end;
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

	if Note.Volume = $FF then
		Note.Volume := 0
	else
	if Note.Volume < 64 then
		SetNote($C, Note.Volume);

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

{         0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  0000: |[T]| Dos filename (12345678.ABC)                   |    MemSeg |
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  0010: |Length |HI:leng|LoopBeg|HI:LBeg|LoopEnd|HI:Lend|Vol| x |[P]|[F]|
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  0020: |C2Spd  |HI:C2sp| x | x | x | x |Int:Gp |Int:512|Int:lastused   |
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  0030: | Sample name, 28 characters max... (incl. NUL)                 |
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  0040: | ...sample name...                             |'S'|'C'|'R'|'S'|
        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  xxxx:	sampledata
}
procedure ReadS3MSample(var Moduli: TPTModule; var ModFile: TFileStreamEx;
	i: Byte; var ips: TImportedSample);
var
	s: TSample;
	samtype, flag: Byte;
	j: Integer;
	c2speed, offset, Datapos: Cardinal;
begin
	offset := ModFile.Position;

	samtype := ModFile.ReadByte;
	if samtype > 1 then Exit;

	{if (samtype > 1) and (not SamplesOnly) then
	begin
		Log(TEXT_WARNING + 'Adlib Samples not supported!');
		Continue;
	end;}

	if Moduli.SamplesOnly then
	begin
		ips := TImportedSample.Create;
		Moduli.ImportInfo.Samples.Add(ips);
	end
	else
		ips := nil;

	ModFile.Skip($0D); // skip dos filename

	Datapos := ModFile.Read16 * $10; // location of sample data in file

	if ips = nil then
		s := Moduli.Samples[i]
	else
		s := ips;

	if samtype = 1 then
	begin
		//        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
		//  0010: |Length |HI:leng|LoopBeg|HI:LBeg|LoopEnd|HI:Lend|Vol| x |[P]|[F]|
		//        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

		s.Resize      (ModFile.Read32);
		s.LoopStart  := ModFile.Read32 div 2;
		s.LoopLength := (ModFile.Read32 div 2) - s.LoopStart;
		s.Volume :=   Min(ModFile.Read8, 64);

		ModFile.Read16;
		flag := ModFile.Read8; // Flags
		if not BitGet(flag, 0) then // use loop
			s.LoopLength := 0;

		//        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
		//  0020: |C2Spd  |HI:C2sp| x | x | x | x |Int:Gp |Int:512|Int:lastused   |
		//        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

		c2speed := ModFile.Read32 and $FFFF;
		case c2speed of
			7800..7895: s.FineTune := $8; // -8
			7896..7941: s.FineTune := $9; // -7
			7942..7985: s.FineTune := $A; // -6
			7986..8046: s.FineTune := $B; // -5
			8047..8107: s.FineTune := $C; // -4
			8108..8169: s.FineTune := $D; // -3
			8170..8232: s.FineTune := $E; // -2
			8233..8280: s.FineTune := $F; // -1
			8364..8413: s.FineTune :=  1; // +1
			8414..8463: s.FineTune :=  2; // +2
			8464..8529: s.FineTune :=  3; // +3
			8530..8581: s.FineTune :=  4; // +4
			8582..8651: s.FineTune :=  5; // +5
			8652..8723: s.FineTune :=  6; // +6
			8724..8757: s.FineTune :=  7; // +7
			else        s.FineTune :=  0;
		end;
		if (s is TImportedSample) then
			TImportedSample(s).C5freq := c2speed;
	end;

	ModFile.SeekTo(offset + $30);
	for j := 0 to 21 do // sample name
		s.Name[j] := AnsiChar(Max(32, ModFile.Read8));

	if (samtype = 1) and (s.ByteLength > 0) then // sample used
	begin
		ModFile.SeekTo(Datapos); // jump to sample data

		for j := 0 to s.ByteLength-1 do
			ShortInt(s.Data[j]) := ModFile.Read8 - 128;

		s.ZeroFirstWord;
	end;
end;

procedure LoadScreamTracker(var Moduli: TPTModule; var ModFile: TFileStreamEx;
	SamplesOnly: Boolean = False);
const
	S3M_EOR			= 0;	// End of row
	S3M_CH_MASK 	= $1F;	// Channel
	S3M_NI_FOLLOW	= $20;	// Note and instrument follow
	S3M_VOL_FOLLOWS	= $40;	// Volume follows
	S3M_FX_FOLLOWS	= $80;	// Effect and parameter follow
var
	os, i, j, pattlen, row, channel: Integer;
	ips: TImportedSample;
	Note: PExtNote;

	SamPtrs: array of Cardinal;
	PattPtrs: array[0..MAX_PATTERNS] of Cardinal;

	flag, b: Byte;

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
		Log('$6Importing Scream Tracker Module.');

		Patterns := TExtPatternList.Create(True);

		ModFile.SeekTo($0);
		ModFile.Read(Moduli.Info.Title[0], 20);
	end;

	ModFile.SeekTo($20); // 0020: OrdNum SmpNum PatNum Flags Cwt/v Ffv

	Moduli.Info.OrderCount := ModFile.Read16; // # of Orders
	os := ModFile.Read16; // # of Samples
	Moduli.Info.PatternCount := Min(ModFile.Read16-1, MAX_PATTERNS-1); // # of Patterns

	if not SamplesOnly then
		Log('%d samples and %d patterns.', [os, Moduli.Info.PatternCount+1]);

	flag := Byte(ModFile.Read16); // flags

	ModFile.Read16; // Cwtv:  Created with tracker
	if Byte(ModFile.Read16) <> 2 then // Ffv: File format version
		Log(TEXT_WARNING + '');

	// read orderlist
	//
	ModFile.SeekTo($60);
	row := 0;
	for i := 1 to Moduli.Info.OrderCount do
	begin
		channel := ModFile.Read8;
		if channel < MAX_PATTERNS then
		begin
			Moduli.OrderList[row] := channel;
			Inc(row);
		end;
	end;
	Moduli.Info.OrderCount := row;

	// read sample offsets
	//
	SetLength(SamPtrs, os);
	channel := os - 1;
	for i := 0 to channel do
		SamPtrs[i] := ModFile.Read16 * $10;

	// read pattern offsets
	//
	for i := 0 to Min(Moduli.Info.PatternCount, MAX_PATTERNS-1) do
		PattPtrs[i] := ModFile.Read16 * $10;

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
		ReadS3MSample(Moduli, ModFile, i, ips);
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

		if PattPtrs[i] >= ModFile.Size then
		begin
			//Log(TEXT_ERROR + 'EOF 1!');
			Break;
		end;

		ModFile.SeekTo(PattPtrs[i]);

		Pattern := TExtPattern.Create(64, 64);
		Patterns.Add(Pattern);

		pattlen := ModFile.Read16 - 2;
		channel := 0;
		row := 0;

		while (pattlen >= 0) and (row < 64) do
		begin
			b := Modfile.Read8;

			if b = S3M_EOR then
			begin
				Inc(row);
				Continue;
			end;

			channel := b and S3M_CH_MASK;
			Note := @Pattern.Notes[channel, row];
			Note.Volume := $FF; // temporary

			if (b and S3M_NI_FOLLOW) <> 0 then
			begin
				flag := ModFile.Read8;
				case flag of
					255: flag := 0;		// Empty note
					254: flag := $FF;	// Key off
				else
					flag := 12 + 12 * ((flag and $F0) shr 4) + (flag and $0F);
				end;
				Note.Pitch := flag;
				Note.Instrument := ModFile.Read8;
				Dec(pattlen, 2);
			end;

			if (b and S3M_VOL_FOLLOWS) <> 0 then
			begin
				Note.Volume := ModFile.Read8;
				Dec(pattlen);
			end;

			if (b and S3M_FX_FOLLOWS) <> 0 then
			begin
				Note.Command   := ModFile.Read8;
				Note.Parameter := ModFile.Read8;
				Dec(pattlen, 2);
			end;

		end; // pattern unpacking

		// convert note pitches and effects from S3M to PT before additional processing
		for channel := 0 to 3 do
			for row := 0 to 63 do
			begin
				Note := @Pattern.Notes[channel, row];
				ConvertS3MCommand(Note^, Conversion);
			end;
	end;

	// insert speed/tempo command at first pattern in orderlist if required
	if (Conversion.Want.InsertTempo) and (Patterns.Count >= Moduli.OrderList[0]) then
	begin
		ModFile.SeekTo($31);
		i := ModFile.Read8; // tempo
		j := ModFile.Read8; // speed
		Patterns[Moduli.OrderList[0]].InsertTempoEffect(Moduli, j, i);
	end;

	// convert intermediate format patterns to ProTracker format
	ProcessConvertedPatterns(Moduli, Patterns, Conversion);

	Patterns.Free;
end;

end.
