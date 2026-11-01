unit ProTracker.Format.STM;

// Scream Tracker v2.xx loader for Propulse Tracker
// reference: https://gist.github.com/cs127/c0f5dffc9f4db2221a66fd9ffc1e3edb

interface

uses
	FileStreamEx, hkaFileUtils,
	ProTracker.Util, ProTracker.Import,
	ProTracker.Player, ProTracker.Sample;

type
	TSTMModule = class(TImportedModule)
	private
		function	ConvertSTMTempo(const Tempo: Byte): Cardinal;
	public
		procedure	LoadFromFile; override;
		procedure	ReadSample(i: Byte; var ips: TImportedSample); override;
		procedure	ConvertCommand(var Note: TExtNote); override;
	end;


implementation

uses
	SysUtils, Math,
	CWE.Core, Screen.Log;

const
	CMD_A = 1;	CMD_B = 2;
	CMD_C = 3;	CMD_D = 4;
	CMD_E = 5;	CMD_F = 6;
	CMD_G = 7;	CMD_H = 8;
	CMD_I = 9;	CMD_J = 10;

{ snippet based on https://gist.github.com/cs127/c0f5dffc9f4db2221a66fd9ffc1e3edb#v110-tempo-calculation }
function TSTMModule.ConvertSTMTempo(const Tempo: Byte): Cardinal;
const
	st_mixing_rate: Cardinal = 15909;
	factor_constants: array of Byte = [140, 50, 25, 15, 10, 7, 6, 4, 3, 3, 2, 2, 2, 2, 1, 1];
var
	st1_tpr, st1_fac: Byte;
	samples_per_tick: Longint;
begin
	st1_tpr := Tempo shr 4;
	st1_fac := Tempo and $F;
	samples_per_tick := floor(st_mixing_rate / (50 - floor((factor_constants[st1_tpr] * st1_fac) / 16)));

	if (samples_per_tick <= 0) then
		samples_per_tick := samples_per_tick + 65536;

	Result := (st_mixing_rate * 5) div (samples_per_tick * 2);
end;

procedure TSTMModule.ConvertCommand(var Note: TExtNote);
const
	NOTETRANSPOSE = -47;
	ProtectedEffects = [$B, $D, $F];
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
	end;

begin
	// convert pitch
	V := Note.Pitch;

	if InRange(V, 1, 119) then
	begin
		Inc(V, NOTETRANSPOSE);
		if not InRange(V, 1, 36) then
		begin
			Inc(Conversion.Missed.Notes);
			if V < 1 then
				V := High(NoteText);
		end;
	end
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
		//CMD_A:	SetNote($F);		// Set tempo
		CMD_B:	SetNote($B);		// Jump to order
		CMD_C:	SetNote($D, 0);     // Break to row; parameter is ignored
		CMD_D:	SetNote($A);    	// Volume slide
		CMD_E:	SetNote($2);		// Pitch slide down
		CMD_F:	SetNote($1);		// Pitch slide up
		CMD_G:	SetNote($3);		// Slide to note
		CMD_H:	SetNote($4);		// Vibrato
		CMD_J:	SetNote($0);		// Arpeggio
	else
		Inc(Conversion.Missed.Effects);
		//Log(TEXT_WARNING + 'Unimplemented command: ' + Chr(C + Ord('A') - 1) + IntToHex(P, 2) );
		C := 0;
		P := 0;
	end;

	if Note.Volume > 64 then
		Note.Volume := 0
	else
	if Note.Volume <= 64 then
		if not ((C = $0) and (P > 0)) then // prioritize arpeggio over volume
			SetNote($C, Note.Volume);

	// emulate note cut/note off by setting volume to 0
	if Note.Pitch = $FF then
	begin
		Note.Pitch := 0;
		if (C in ProtectedEffects) or ((C = $0) and (P > 0)) then // don't discard important fx!
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

{
| offset   | type       | content                                                         |
|----------|------------|-----------------------------------------------------------------|
| s+0x0000 | `char[13]` | sample filename (null-terminated) (often used as song comments) |
| s+0x000D | `u8`       | disk number (seemingly unused in v1)                            |
| s+0x000E | `u16`      | data position (in paragraphs) (see below)                       |
| s+0x0010 | `u16`      | sample length (in sampling points / bytes)                      |
| s+0x0012 | `u16`      | loop start (in sampling points / bytes)                         |
| s+0x0014 | `u16`      | loop end (in sampling points / bytes)                           |
| s+0x0016 | `u8`       | default note volume (max 64)                                    |
| s+0x0017 | `u8`       | (unused)                                                        |
| s+0x0018 | `u16`      | middle C sample rate                                            |
| s+0x001A | `u8[4]`    | (unused)                                                        |
| s+0x001E | `u16`      | (unknown)                                                       |
}
procedure TSTMModule.ReadSample(i: Byte; var ips: TImportedSample);
var
	s: TSample;
	samtype, flag: Byte;
	j: Integer;
	c2speed, offset, Datapos: Cardinal;
    loopstart, loopend: Cardinal;
begin
	offset := ModFile.Position;

	if Module.SamplesOnly then
	begin
		ips := TImportedSample.Create;
		Module.ImportInfo.Samples.Add(ips);
	end
	else
		ips := nil;

	if ips = nil then
		s := Module.Samples[i]
	else
		s := ips;


	for j := 0 to $0D do // sample filename
		s.Name[j] := AnsiChar(Max(32, ModFile.Read8));
    ModFile.Skip(1);
	Datapos := ModFile.Read16 * $10; // location of sample data in file
 

	s.Resize      (ModFile.Read16);
    loopstart := ModFile.Read16;
    loopend := ModFile.Read16;
    if (loopend <> $FFFF) then
    begin
         s.LoopStart  := loopstart div 2;
	 s.LoopLength := (loopend div 2) - s.LoopStart;
    end;

	s.Volume :=   Min(ModFile.Read8, 64);
    ModFile.Skip(1);

	c2speed := ModFile.Read16;
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

	if (s.ByteLength > 0) then // sample used
	begin
		ModFile.SeekTo(Datapos); // jump to sample data
		ModFile.Read(s.Data[0], s.ByteLength);
		s.ZeroFirstWord;
	end;
end;

procedure TSTMModule.LoadFromFile;
var
	os, i, j, pattlen, row, channel: Integer;
	ips: TImportedSample;
	Note: PExtNote;
	SamPtrs: array of Cardinal;
	b: array[0..3] of Byte;
	flag: Byte;
	Pattern: TExtPattern;
	STMVersion: Cardinal;
	PatternsinFile, OrdersinFile: Cardinal;
	Tempo: Cardinal;
begin
	if not SamplesOnly then
	begin
		ChangeScreen(TCWEScreen(LogScreen));
		Log('$6Importing Scream Tracker v2.xx Module.');

		ModFile.SeekTo($0);
		ModFile.Read(Module.Info.Title[0], 20);
	end;

	ModFile.SeekTo($1E);
	STMVersion := ModFile.Read8 * 100;
	STMVersion := STMVersion + ModFile.Read8;
	Tempo := ModFile.Read8;
	if STMVersion <= 220 then
		Tempo := ((Tempo div 10) shl 4) + (Tempo mod 10);
	PatternsinFile := Min(ModFile.Read8-1, MAX_PATTERNS-1); // # of Patterns
	Module.Info.PatternCount := PatternsinFile;
	// # of Orders
	if STMVersion <= 200 then
		OrdersinFile := 64
	else
		OrdersinFile := 128;
	Module.Info.OrderCount := OrdersinFile;

	// read orderlist
	//
	ModFile.SeekTo($410);
	row := 0;
	for i := 1 to OrdersinFile do
	begin
		channel := ModFile.Read8;
		if channel < 63 then
		begin
			Module.OrderList[row] := channel;

			if Module.OrderList[row] >= Module.Info.PatternCount then
				Module.Info.PatternCount := channel;

			Inc(row);
		end;
	end;
	Module.Info.PatternCount := Module.Info.PatternCount-1;
	if not SamplesOnly then
		Log('31 samples and %d patterns.', [Module.Info.PatternCount+1]);

	if SamplesOnly then
		Module.ImportInfo.Samples.Clear;

	// read samples
	//
	for i := 0 to 31 do
	begin
		ips := nil;
		os := $30 + (i * 32);
		ModFile.SeekTo(os);
		ReadSample(i, ips);
	end;

	if SamplesOnly then
		Exit;

	ModFile.SeekTo($410 + OrdersinFile);

	// read and convert pattern data
	//
	for i := 0 to Min(Module.Info.PatternCount, MAX_PATTERNS-1) do
	begin
		if Module.Info.PatternCount >= PatternsinFile then
		begin
			Pattern := TExtPattern.Create(AMOUNT_CHANNELS, 64);
			Patterns.Add(Pattern);
			Continue;
		end;

		Pattern := TExtPattern.Create(AMOUNT_CHANNELS, 64);
		Patterns.Add(Pattern);

		channel := 0;
		row := 0;

		ModFile.SeekTo($410 + OrdersinFile);

		for row := 0 to 64 do
		begin
			for channel := 1 to 4 do
			begin
				b[0] := Modfile.Read8;
				b[1] := Modfile.Read8;
				b[2] := Modfile.Read8;
				b[3] := Modfile.Read8;				
			(*
				// packed representation
				// happens in songs but not modules
				// due to a bug in ST2 (?)
				if b[0] = 252 then
				begin
					// empty
					b[0] := $FF;
					b[1] := $01;
					b[2] := $80;
					b[3] := $00;
				end
				else if b[0] = 253 then
				begin
					// empty (note cut)
					b[0] := $FE;
					b[1] := $01;
					b[2] := $80;
					b[3] := $00;
				end
				else if b[0] = 251 then
				begin
					// all 0 bytes
					b[0] := 0;
					b[1] := 0;
					b[2] := 0;
					b[3] := 0;
				end
				else
				begin
					b[1] := Modfile.Read8;
					b[2] := Modfile.Read8;
					b[3] := Modfile.Read8;
				end;			
			*)

				Note := @Pattern.Notes[channel, row];
				flag := b[0];
				case flag of
					255: flag := 0;		// Empty note
					254: flag := $FF;	// Key off
				else
					flag := 12 + 12 * ((flag and $F0) shr 4) + (flag and $0F);
				end;
				Note.Pitch		:= flag;
				Note.Instrument := b[1] shr 3;
				Note.Volume		:= (b[1] and 7) or ((b[2] shr 4) shl 3);
				Note.Command	:= b[2] and $F;
				Note.Parameter	:= b[3];
			end;
		end; // pattern unpacking

	end;

	Conversion.Info.OrigTempo := ConvertSTMTempo(Tempo);
	Conversion.Info.OrigSpeed := Tempo shr 4;
	
	CountChannels;
end;


end.

