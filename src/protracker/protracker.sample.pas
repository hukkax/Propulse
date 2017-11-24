unit ProTracker.Sample;

interface

uses
    soxr,
	ProTracker.Util,
	FileStreamEx;

const
	BASSSupportedFormats = '[.mp3][.ogg][.aiff]';

	MAX_IMPORTED_SAMPLESIZE = 6; // load max. 6 megabytes of 8-bit mono sample data

	// Bit width (8 bits for simplicity)
	ST_BIT_MASK	= $000000FF;
	ST_8		= 8;		// 8-bit sample data
	ST_16		= 16;		// 16-bit sample data

	// Channels (4 bits)
	ST_CHN_MASK	= $00000F00;
	ST_MONO		= 0 shl 8;	// mono sample
	ST_STEREO 	= 1 shl 8;	// stereo sample

	// Endianness (4 bits)
	ST_END_MASK = $0000F000;
	ST_LE 		= 0 shl 12;	// little-endian byte order
	ST_BE 		= 1 shl 12;	// big-endian byte order

	// Encoding (8 bits)
	ST_ENC_MASK = $00FF0000;
	ST_UNSIGNED	= 0 shl 16;	// PCM, signed
	ST_SIGNED	= 1 shl 16;	// PCM, unsigned
	ST_DELTA	= 2 shl 16;	// PCM, delta encoded
	ST_IT214 	= 3 shl 16;	// Impulse Tracker 2.14 compressed
	ST_IT215 	= 4 shl 16;	// Impulse Tracker 2.15 compressed

	// Default: 8-bit signed PCM data
	RS_PCM8S	= (ST_SIGNED or ST_8 or ST_MONO);
	// 8-bit unsigned PCM data
	RS_PCM8U	= (ST_UNSIGNED or ST_8 or ST_MONO);
	// 8-bit ADPCM data with linear table
	RS_PCM8D	= (ST_DELTA or ST_8 or ST_MONO);
	// 16-bit ADPCM data with linear table
	RS_PCM16D	= (ST_DELTA or ST_16 or ST_MONO);
	// 16-bit signed PCM data
	RS_PCM16S	= (ST_SIGNED or ST_16 or ST_MONO);
	// 16-bit signed mono PCM motorola byte order
	RS_PCM16M	= (ST_SIGNED or ST_16 or ST_MONO or ST_BE);
	// 16-bit unsigned PCM data
	RS_PCM16U	= (ST_UNSIGNED or ST_16 or ST_MONO);
	// 16-bit signed stereo big endian
	RS_STPCM16M	= (ST_SIGNED or ST_16 or ST_STEREO or ST_BE);
	// 8-bit stereo samples
	RS_STPCM8S	= (ST_SIGNED   or ST_8 or ST_STEREO);
	RS_STPCM8U	= (ST_UNSIGNED or ST_8 or ST_STEREO);
	RS_STPCM8D	= (ST_DELTA    or ST_8 or ST_STEREO);
	// 16-bit stereo samples
	RS_STPCM16S	= (ST_SIGNED   or ST_16 or ST_STEREO);
	RS_STPCM16U	= (ST_UNSIGNED or ST_16 or ST_STEREO);
	RS_STPCM16D	= (ST_DELTA    or ST_16 or ST_STEREO);
	// IT compressed samples
	RS_IT2148	= (ST_IT214 or ST_8  or ST_MONO);
	RS_IT21416	= (ST_IT214 or ST_16 or ST_MONO);
	RS_IT2158	= (ST_IT215 or ST_8  or ST_MONO);
	RS_IT21516	= (ST_IT215 or ST_16 or ST_MONO);
	RS_IT2148S	= (ST_IT214 or ST_8  or ST_STEREO);
	RS_IT21416S	= (ST_IT214 or ST_16 or ST_STEREO);
	RS_IT2158S	= (ST_IT215 or ST_8  or ST_STEREO);
	RS_IT21516S	= (ST_IT215 or ST_16 or ST_STEREO);

type
	TSampleFormat = ( SamFmtRAW, SamFmtIFF, SamFmtWAV, SamFmtITS, SamFmtFromExt );

	TSample = class
	public
		Data,
		Backup:			packed array of Byte;
		Name:			packed array [0..21] of AnsiChar;
		Finetune:		ShortInt;
		Volume:			Byte;
		Length:			Cardinal;		// data length in words
		BackupLength:	Word;
		LoopStart,
		LoopLength,
		TempLoopStart,
		TempLoopLength,
		tmpLoopStart:	Cardinal;		// positions in words
		Age: 			ShortInt;
		FileOffset:		Cardinal;
		Index:			Byte; 			// 1-based!

		function 		IsEmpty: Boolean; inline;
		function 		IsLooped: Boolean; inline;
		function		IsDuplicateOf(const Other: TSample): Boolean;

		constructor 	Create;
		procedure 		Validate;
		procedure		ValidateCoords(out X1, X2: Integer);
		function 		ByteLength: Cardinal; inline;
		procedure		Assign(const Source: TSample);
		procedure 		GetFloatData(X1, X2: Integer; var buffer: TFloatArray);

		procedure 		LoadData(var ModFile: TFileStreamEx;
						NumSamples: Cardinal; Flags: Cardinal = RS_PCM8S);
		procedure 		LoadDataFloat(var ModFile: TFileStreamEx;
						NumSamples: Cardinal; Flags: Cardinal;
						var Buffer: TFloatArray);
		function 		LoadWithBASS(const Filename: String): Boolean;
		function		LoadFromFile(const Filename: String): Boolean;
		procedure 		SaveToFile(const Filename: String; FileFormat: TSampleFormat);

		function 		GetName: AnsiString;
		procedure 		UpdateVoice;
		procedure		SetName(const S: AnsiString);
		function 		EnableLooping(B: Boolean): Boolean;
		function  		SetLoopStart(WordPos: Integer): Boolean;
		function  		SetLoopEnd(WordPos: Integer): Boolean;
		procedure		ZeroFirstWord;
		procedure 		Clear;
		function 		RestoreBackup: Boolean;
		procedure 		StoreBackup;
		procedure		Reverse(X1: Integer = 0; X2: Integer = -1);
		procedure		Invert(X1: Integer = 0; X2: Integer = -1);
		procedure		Centralise;
		procedure		PreLoopCut;
		procedure		PostLoopCut;
		procedure		Resize(NewSize: Cardinal);
		procedure 		Resample(OrigHz, DestHz: Cardinal;
						RsQuality: Integer = SOXR_VHQ;
						DoNormalize: Boolean = True;
						DoHighBoost: Boolean = True);
		procedure 		ResampleFromBuffer(var iBuf: TFloatArray;
						OrigHz, DestHz: Cardinal;
						RsQuality: Integer = SOXR_VHQ;
						DoNormalize: Boolean = True;
						DoHighBoost: Boolean = True);
		function		GetNormalizationValue(X1: Integer = 0; X2: Integer = -1): Single;
		procedure		Normalize(NormalizationValue: Single = -100;
						X1: Integer = 0; X2: Integer = -1);
		procedure 		Downsample;
		procedure		Upsample;

	end;

	TImportedSample = class (TSample)
		OrigSize:		Cardinal;
		C5freq: 		Cardinal;
		Is16Bit:		Boolean;
		IsStereo:		Boolean;
		IsPacked:		Boolean;

		constructor 	Create; overload;
	end;

	function IsEmptySample(const Sam: TSample): Boolean; inline;
	function GetCurrentSample: TSample; inline;

var
	LastSampleFormat: packed record
		Length:		Cardinal;
		isStereo,
		is16Bit: 	Boolean;
	end;

implementation

uses
	{$IFDEF BASS_DYNAMIC}
		lazdynamic_bass,
	{$ELSE}
		BASS,
	{$ENDIF}
	Math, Classes, SysUtils,
	fpwavformat, fpwavreader, fpwavwriter,
	FloatSampleEffects,
	ProTracker.Player,
	ProTracker.Paula,
	ProTracker.Format.IT,
	ProTracker.Editor;


function IsEmptySample(const Sam: TSample): Boolean;
begin
	if Sam = nil then
		Exit(True)
	else
		Result := (Sam.Length < 2);
end;

function GetCurrentSample: TSample;
begin
	Result := Module.Samples[CurrentSample-1];
end;

// ==========================================================================
// TSample
// ==========================================================================

constructor TSample.Create;
begin
	Clear;
end;

function TSample.ByteLength: Cardinal;
begin
	Result := System.Length(Data);
end;

function TSample.IsEmpty: Boolean;
begin
	Result := High(Data) <= 1;
end;

function TSample.IsLooped: Boolean;
begin
	Result := ((LoopLength + LoopStart) > 1);
end;

function TSample.IsDuplicateOf(const Other: TSample): Boolean;
var
	i: Integer;
begin
	Result := False;
	if (Other.Length <> Length) or (Other.Finetune <> Finetune) or (Other.Volume <> Volume) or
		(Other.LoopStart <> LoopStart) or (Other.LoopLength <> LoopLength) then Exit;
	for i := 0 to High(Data) do
		if Other.Data[i] <> Data[i] then Exit;
	Result := True;
end;

procedure TSample.ZeroFirstWord;
begin
	if (not IsLooped) and (not IsEmpty) then
	begin
		Data[0] := 0;
		Data[1] := 0;
	end;
end;

procedure TSample.ValidateCoords(out X1, X2: Integer);
var
	L: Integer;
begin
	L := High(Data);
	if ({%H-}X2 < {%H-}X1) or (X2 > L) then
		X2 := L;
	if (X1 < 0) or (X1 > L) then
		X1 := 0;
end;

procedure TSample.GetFloatData(X1, X2: Integer; var buffer: TFloatArray);
var
	i: Integer;
begin
	ValidateCoords(X1, X2);
	SetLength(buffer, X2-X1+1);
	for i := X1 to X2 do
		buffer[i-X1] := ShortInt(Data[i]) / 128;
end;

procedure TSample.UpdateVoice;
var
	i: Integer;
	Ch: TPaulaVoice;
begin
	if (not IsLooped) then Exit;

	for i := 0 to 3 do
	if Module.Channel[i].Enabled then
	begin
		Ch := Module.Channel[i].Paula;
		if (Ch.Sample = Index-1) and (Ch.PlayPos >= 0) then
		begin
			Ch.QueuedOffset := LoopStart * 2;
			Ch.SRC_DAT := @Data[Ch.QueuedOffset];
			Ch.SRC_LEN := LoopLength * 2;
		end;
	end;
end;

function TSample.EnableLooping(B: Boolean): Boolean;
begin
	if B then
	begin
		// enable sample loop
		LoopStart  := TempLoopStart;
		LoopLength := TempLoopLength;
	end
	else
	begin
		// disable sample loop
		TempLoopStart  := LoopStart;
		TempLoopLength := LoopLength;
		LoopStart  := 0;
		LoopLength := 1;
	end;
	Result := IsLooped;
end;

function TSample.SetLoopStart(WordPos: Integer): Boolean;
begin
	WordPos := Max(WordPos, 0);
	WordPos := Min(WordPos, Length);
	if WordPos < (LoopStart + LoopLength) then
	begin
		LoopLength := (LoopStart + LoopLength) - WordPos;
		LoopStart  := WordPos;
		UpdateVoice;
		Result := True;
	end
	else
		Result := False;
end;

function TSample.SetLoopEnd(WordPos: Integer): Boolean;
begin
	WordPos := Max(WordPos, 0);
	WordPos := Min(WordPos, Length);
	if (WordPos >= 2) and (WordPos > LoopStart) then
	begin
		LoopLength := WordPos - LoopStart;
		UpdateVoice;
		Result := True;
	end
	else
		Result := False;
end;

procedure TSample.Resize(NewSize: Cardinal); // size in bytes
begin
	// TODO: clear new data on sample size increase
	if (NewSize mod 2) <> 0 then Inc(NewSize);
	SetLength(Data, NewSize);
	Length := NewSize div 2;
end;

function TSample.GetName: AnsiString;
var
	x: Integer;
begin
	Result := '';
	for x := 0 to 21 do
		if (Ord(Name[x]) >= 32) then
			Result := Result + Name[x]
		else
			Result := Result + ' ';
	Result := TrimRight(Result);
end;

procedure TSample.SetName(const S: AnsiString);
var
	x: Integer;
begin
	for x := 0 to 21 do
		Name[x] := #0;
	if S <> '' then
		for x := 1 to Min(System.Length(S), 21+1) do
			Name[x-1] := S[x];
end;

procedure TSample.Assign(const Source: TSample);
var
	X: Integer;
begin
	if (Source = nil) or (Source = Self) then Exit;

	Length := Source.Length;
	SetLength(Data, Source.ByteLength);
	for X := 0 to Source.ByteLength-1 do
		Data[X] := Source.Data[X];

	LoopStart  := Source.LoopStart;
	LoopLength := Source.LoopLength;

	for X := 0 to High(Name) do
		Name[X] := Source.Name[X];

	Finetune := Source.Finetune;
	Volume := Source.Volume;
	tmpLoopStart := Source.tmpLoopStart;
	Age := 0;
	FileOffset := Source.FileOffset;
end;

procedure TSample.Clear;
begin
	SetName('');
	SetLength(Data, 2);
	SetLength(Backup, 0);
	BackupLength := 0;
	Length     := 0;
	LoopStart  := 0;
	LoopLength := 1;
	TempLoopStart  := LoopStart;
	TempLoopLength := LoopLength;
	Volume     := 64;
	Finetune   := 0;
	Age        := -1;
	ZeroFirstWord;
end;

procedure TSample.StoreBackup;
begin
	BackupLength := (Length * 2) and $FFFF;
	SetLength(Backup, BackupLength);
	Move(Data[0], Backup[0], BackupLength);
end;

function TSample.RestoreBackup: Boolean;
begin
	Result := (BackupLength > 0);
	if Result then
		Move(Backup[0], Data[0], BackupLength);
	BackupLength := 0;
end;

procedure TSample.Reverse(X1: Integer = 0; X2: Integer = -1);
var
	h, i, e: Integer;
	V: Byte;
begin
	ValidateCoords(X1, X2);
	e := X2;
	h := X1 + ((X2 - X1) div 2);
	for i := X1 to h-1 do
	begin
		V := Data[e];
		Data[e] := Data[i];
		Data[i] := V;
		Dec(e);
	end;
	if (IsLooped) and (X1 = 0) and (X2 = High(Data)) then
		LoopStart := Max(Length - LoopStart - LoopLength, 0);
	ZeroFirstWord;
end;

procedure TSample.Invert(X1: Integer = 0; X2: Integer = -1);
var
	i: Integer;
begin
	ValidateCoords(X1, X2);
	for i := X1 to X2 do
		Data[i] := 255 - Data[i];
	ZeroFirstWord;
end;

procedure TSample.Centralise;
begin
end;

procedure TSample.PreLoopCut;
begin
	if LoopStart > 0 then
	begin
		CopyMemory(@Data[0], @Data[LoopStart*2], System.Length(Data)-(LoopStart*2));
		LoopStart := 0;
		Resize(Max(0, (Length - LoopStart) * 2));
		ZeroFirstWord;
	end;
end;

procedure TSample.PostLoopCut;
var
	i: Integer;
begin
	i := LoopStart + LoopLength;
	if i < Length then
		Resize(i * 2);
end;

procedure TSample.ResampleFromBuffer(var iBuf: TFloatArray;
	OrigHz, DestHz: Cardinal;
	RsQuality: Integer = SOXR_VHQ;
	DoNormalize: Boolean = True;
	DoHighBoost: Boolean = True);
var
	success: soxr_error_t;
	odone, ilen, olen: size_t;
	obuf: TFloatArray;
	i: Integer;
	iospec:  soxr_io_spec_t;
	quality: soxr_quality_spec_t;
	runtime: soxr_runtime_spec_t;
	loopL, loopR: Single;
begin
	if not SOXRLoaded then Exit;

	if (ibuf = nil) or (System.Length(ibuf) < 1) then Exit;

	ilen := System.Length(ibuf);
	olen := ilen; //Round(ilen * (DestHz / OrigHz) + 0.5);
	odone := ilen;
	SetLength(obuf, olen + 1);

	if IsLooped then
	begin
		loopL := LoopStart / Length;
		loopR := (LoopStart + LoopLength) / Length;
	end
	else
		loopL := -1.0;

	if DoHighBoost then
	begin
//		FloatSampleEffects.Normalize(ibuf, 0.9);
//		FloatSampleEffects.Equalize(ibuf, 880, 5000, OrigHz, 1.0, 1.0, 1.6);
	end;

{	Log(TEXT_LIGHT+'[%s]', [Trim(Name)]);
	Log('Resampling: %d Hz -> %d Hz', [OrigHz, DestHz]);}

	iospec  := soxr_io_spec(SOXR_FLOAT32_I, SOXR_FLOAT32_I);
	quality := soxr_quality_spec(
		RsQuality or SOXR_INTERMEDIATE_PHASE or SOXR_STEEP_FILTER, 0);
	runtime := soxr_runtime_spec(0);

	success := soxr_oneshot(
		OrigHz,			// input_rate: double;
		DestHz,			// output_rate: double;
		1,				// num_channels: Cardinal;
		@ibuf[0],		// input: soxr_in_t;
		ilen,			// ilen: size_t;
		nil,			// idone: Psize_t;
		@obuf[0],		// output: soxr_out_t;
		olen,			// olen: size_t;
		@odone,			// odone: Psize_t;
		@iospec, 		// iospec: soxr_io_spec_t;
		@quality,		// quality: Psoxr_quality_spec;
		@runtime		// runtime: Psoxr_runtime_spec
	);

	if success <> nil then
	begin
		Log(TEXT_ERROR + 'soxr_oneshot: %s', [AnsiString(success)]);
		Exit;
	end;

//	Log('Out: %d bytes', [odone]);

//	odone := Min(odone, $1FFFF+1); // limit sample length to 128KB
	Length := odone div 2;
	SetLength(Data, Length*2 + 1);

	if loopL >= 0.0 then
	begin
		LoopStart  := Trunc(Length * loopL);
		LoopLength := Trunc((Length * loopR) - LoopStart);
	end
	else
	begin
		LoopStart  := 0;
		LoopLength := 1;
	end;

	if DoNormalize then
		FloatSampleEffects.Normalize(obuf);

	for i := 0 to odone-1 do
		ShortInt(Data[i]) := ShortInt(Trunc(obuf[i] * 127));

	ZeroFirstWord;
end;

procedure TSample.Resample(OrigHz, DestHz: Cardinal;
	RsQuality: Integer = SOXR_VHQ;
	DoNormalize: Boolean = True;
	DoHighBoost: Boolean = True);
var
	ibuf: TFloatArray;
	i: Integer;
	divider: Single;
begin
	if (not SOXRLoaded) or (ByteLength < 1) then Exit;

	SetLength(ibuf, System.Length(Data));

	divider := 1.0 / 128.0;
	for i := 0 to High(Data) do
		ibuf[i] := (ShortInt(Data[i])) * divider {* 0.99};

	ResampleFromBuffer(iBuf, OrigHz, DestHz,
		RsQuality, DoNormalize, DoHighBoost);
end;

procedure TSample.Downsample;
var
	x, l: Integer;
begin
	l := ByteLength div 2;
	for x := 0 to l-1 do
		Data[x] := Data[x * 2];
	Resize(l);

	LoopStart  := LoopStart div 2;
	LoopLength := LoopLength div 2;

	Validate;
end;

procedure TSample.Upsample;
var
	x, l: Integer;
begin
	l := ByteLength;
	Resize(l * 2);
	for x := l-1 downto 0 do
	begin
		Data[x*2]   := Data[x];
		Data[x*2+1] := Data[x];
	end;

	LoopStart  := LoopStart * 2;
	LoopLength := LoopLength * 2;

	Validate;
end;

function TSample.GetNormalizationValue(X1: Integer = 0; X2: Integer = -1): Single;
var
	x: Integer;
	ndone, numstat: LongWord;
	minp, maxp: Smallint;
	normpercent, peakpercent: Single;
	stats: array[0..255] of Cardinal;
begin
	ValidateCoords(X1, X2);

	Result := 1.0;
	normpercent := 100.0;
	peakpercent := 100.0;
	numstat := 0;
	for x := Low(stats) to High(stats) do
		stats[x] := 0;

	for x := X1 to X2 do
	begin
		Inc(stats[Data[x]]);
		Inc(numstat);
	end;

	// find how many samples is <percent> of the max
	numstat := Round( numstat * (1.0 - (peakpercent / 100.0)) );
	// use this to accumulate values
	ndone := 0;
	// count the min sample value that has the given percentile
	for x := 0 to 255 do
	begin
		if ndone > numstat then Break;
		Inc(ndone, stats[x]);
	end;
	minp := x - 129;
	// count the max sample value that has the given percentile
	ndone := 0;
	for x := 255 downto 0 do
	begin
		if ndone > numstat then Break;
		Inc(ndone, stats[x]);
	end;

	maxp := x - 127;
	if minp = -128  then minp := -127;
	if -minp > maxp then maxp := -minp;
	if maxp = 0 then Exit;

	Result := (127.0 * normpercent) / (maxp * 100.0);
end;

procedure TSample.Normalize(NormalizationValue: Single = -100;
	X1: Integer = 0; X2: Integer = -1);
var
	x, t: Integer;
begin
	ValidateCoords(X1, X2);

	if NormalizationValue <= -100 then
		NormalizationValue := GetNormalizationValue(X1, X2);

	for x := X1 to X2 do
	begin
		t := Trunc(ShortInt(Data[x]) * NormalizationValue);
		if t > 127 then
			t := 127
		else
		if t < -127 then
			t := -127;
		ShortInt(Data[x]) := ShortInt(t);
	end;
end;

procedure TSample.Validate;
var
	L: Word;
begin
	{if ByteLength >= $1FFFF then
		Resize($1FFFF-1)
	else}
	if ByteLength < (Length * 2) then
		SetLength(Data, Length * 2 + 1);

	L := Length and $FFFF;
	if LoopStart > L then 					// fix this stuff
		LoopStart := 0;
	if (LoopStart + LoopLength) > L then
		LoopLength := L - LoopStart;
	if LoopLength < 1 then
		LoopLength := 1;

	ZeroFirstWord;
end;

function TSample.LoadWithBASS(const Filename: String): Boolean;
var
	DataLength: QWord;
	Stream: HSTREAM;
	Info: BASS_CHANNELINFO;
	Buf: TFloatArray;
	S: AnsiString;
	Channels, V: Int64;
	i, Freq: Cardinal;
begin
	Result := False;

	Stream := BASS_StreamCreateFile(False, PChar(Filename), 0, 0,
		BASS_SAMPLE_FLOAT or BASS_STREAM_DECODE);
	if Stream = 0 then
	begin
		case BASS_ErrorGetCode() of
			BASS_ERROR_FILEOPEN:	S := 'The file could not be opened.';
			BASS_ERROR_FILEFORM:	S := 'File format not recognised or supported.';
			BASS_ERROR_CODEC:		S := 'Unavailable or unsupported codec.';
			BASS_ERROR_MEM:			S := 'Insufficient memory.';
		else
			S := 'Unknown problem!';
		end;
		Log(TEXT_ERROR + S);
		Exit;
	end;

	DataLength := BASS_ChannelGetLength(Stream, BASS_POS_BYTE);
	if DataLength < 2 then
	begin
		BASS_StreamFree(Stream);
		Exit;
	end;

	if BASS_ChannelGetInfo(Stream, Info) then
		Channels := Info.chans
	else
		Channels := 1;

	DataLength := Min(DataLength, 1024*1024*Channels*MAX_IMPORTED_SAMPLESIZE);

	SetLength(Buf, DataLength div 4);
	DataLength := BASS_ChannelGetData(Stream, @Buf[0], DataLength or BASS_DATA_FLOAT) div 4;

	if 	(SOXRLoaded) and (Options.Import.Resampling.Enable) and
		(Info.freq >= Options.Import.Resampling.ResampleFrom) then
	begin
		if Channels > 1 then
		begin
			DataLength := DataLength div Channels;
			for i := 0 to DataLength-1 do
				Buf[i] := Buf[i * Channels];
			SetLength(Buf, DataLength);
		end;
		if Options.Import.Resampling.Normalize then
			FloatSampleEffects.Normalize(Buf);

		// resample automatically
		Freq := PeriodToHz(PeriodTable[Options.Import.Resampling.ResampleTo]);
		//Log('Resampling from %d to %d Hz', [Info.freq, Freq]);
		ResampleFromBuffer(Buf, Info.freq, Freq,
			SOXRQuality[Options.Import.Resampling.Quality],
			Options.Import.Resampling.Normalize,
			Options.Import.Resampling.HighBoost);
	end
	else
	begin
		DataLength := DataLength div Channels div 4;
		Self.Resize(DataLength);

		for i := 0 to DataLength-1 do
		begin
			V := Trunc(Buf[i * Channels] * 127);
			if V < -128 then V := -128 else if V > 127 then V := 127;
			ShortInt(Data[i]) := ShortInt(V);
		end;
	end;

	BASS_StreamFree(Stream);

	if (Self is TImportedSample) then
	begin
		TImportedSample(Self).isStereo := (Channels > 1);
		TImportedSample(Self).is16Bit  := True; // !!!
	end;
	LastSampleFormat.Length := Length;

	Result := True;
end;

function TSample.LoadFromFile(const Filename: String): Boolean;
var
	Wav: TWavReader;
	FileAcc: TFileStreamEx;
	ID, sName: AnsiString;
	X, i: Integer;
	Len, WavLen: Cardinal;
	ips: TImportedSample;
	Buf: array of SmallInt;
    SupportedFormat: Boolean;
begin
	Result := False;

	if not FileExists(Filename) then
	begin
		Log(TEXT_ERROR + 'File not found: %s', [Filename]);
		Exit;
	end;

	Self.Volume := 64;
	Self.Finetune := 0;
	Self.LoopStart  := 0;
	Self.LoopLength := 1;
	Self.SetName(ExtractFileName(Filename));

	// let BASS decode the file contents unless we have our own loader for it
	ID := '[' + LowerCase(ExtractFileExt(Filename)) + ']';
	if Pos(ID, BASSSupportedFormats) > 0 then
	begin
		Result := LoadWithBASS(Filename);
		if Result then Exit;
	end;

	FileAcc := TFileStreamEx.Create(Filename, fmOpenRead, fmShareDenyNone);

	ID := FileAcc.ReadString(False, 4);

	if ID = 'RIFF' then	// WAV
	begin
		Wav := TWavReader.Create;
		FileAcc.SeekTo(0);
		Wav.LoadFromStream(FileAcc);

        SupportedFormat := True;

		case Wav.fmt.Channels of
			1:  LastSampleFormat.isStereo := False;
			2:  LastSampleFormat.isStereo := True;
		else
			SupportedFormat := False;
		end;

        if SupportedFormat then
		case Wav.fmt.BitsPerSample of
			8:  LastSampleFormat.is16Bit := False;
			16:
			begin
				LastSampleFormat.is16Bit := True;
				SupportedFormat := False; // load 16-bit samples via BASS
			end;
		else
			SupportedFormat := False;
		end;

		if not SupportedFormat then
		begin
			FileAcc.Free;
			Wav.Free;
			// use BASS to decode unsupported wav format
			Exit(LoadWithBASS(Filename));
		end;

		WavLen := Min(Wav.dataSize, 1024*1024*MAX_IMPORTED_SAMPLESIZE);
		Len := WavLen;

		if LastSampleFormat.isStereo then Len := Len div 2;
		if LastSampleFormat.is16Bit  then Len := Len div 2;

		SetLength(Data, Len + 1);
		Self.Length := Len div 2;

		if (not LastSampleFormat.isStereo) and (not LastSampleFormat.is16Bit) then
		begin
			// mono 8-bit
			Wav.ReadBuf(Data[0], WavLen);
			for X := 0 to WavLen-1 do
				Data[X] := Byte(127 - Data[X]);
		end
		else
		if LastSampleFormat.is16Bit then
		begin
			SetLength(Buf, WavLen + 1);
			Wav.ReadBuf(Buf[0], WavLen);
			if LastSampleFormat.isStereo then
			begin
				// stereo 16-bit
				for X := 0 to Len-1 do
					Data[X] := Byte((Buf[X*2] + Buf[X*2+1]) div 512);
			end
			else
				// mono 16-bit
				for X := 0 to Len-1 do
					Data[X] := Byte(Buf[X] div 256);
		end
		else
		begin
			// stereo 8-bit UNTESTED
			SetLength(Buf, WavLen + 1);
			Wav.ReadBuf(Buf[0], WavLen);
			for X := 0 to Len do
				Data[X] := Buf[X] and $FF;
		end;

		if (Wav.smpl.Exists) and (wav.smpl.MainChunk.NumSampleLoops > 0) then
		begin
			LoopStart  := wav.smpl.LoopInfo.LoopStart and $FFFFFFFE;
			LoopLength := ((wav.smpl.LoopInfo.LoopEnd + 1) and $FFFFFFFE) - LoopStart;
			Validate;
		end;

		Wav.Free;
	end
	else
	if ID = 'FORM' then	// IFF 8SVX
	begin
		// Should be the size of the file minus 4+4 ( 'FORM'+size )
		Len := FileAcc.Read32R;
		ID := FileAcc.ReadString(False, 4);
		if ID <> '8SVX' then Exit;
		i := 0;

		while (ID <> 'BODY') and (i < 30) do
		begin
			ID := FileAcc.ReadString(False, 4);
			Inc(i); // iterations
			Len := FileAcc.Read32R;

			if ID = 'VHDR' then
			begin
				// # samples in the high octave 1-shot part
				Self.LoopStart  := FileAcc.Read32R div 2;
				// # samples in the high octave repeat part
				Self.LoopLength := FileAcc.Read32R div 2;
				if Self.LoopLength < 1 then
					Self.LoopLength := 1;
				FileAcc.Read32;				// # samples/cycle in high octave, else 0
				FileAcc.Read16;				// samples per second
				FileAcc.Read8;				// # octaves of waveforms
				if FileAcc.Read8 <> 0 then	// # data compression technique used
					Exit;
				Self.Volume := Trunc((FileAcc.Read32R / 1024) + 0.5);	// playback volume
			end
			else
			if ID = 'BODY' then
			begin
				// 8-bit sample data
				Resize(Len);
				FileAcc.Read(Data[0], Len-1);

				if 	(Self.LoopStart > Self.Length) or
					((Self.LoopStart + Self.LoopLength) > Self.Length) then
				begin
					Self.LoopStart  := 0;
					Self.LoopLength := 1;
				end;
			end
			else
			if ID = 'NAME' then
			begin
				sName := FileAcc.ReadString(False, Len);
				Self.SetName(sName);
				if Len mod 2 = 1 then FileAcc.Read8;
			end
			else
			begin
				if (Len and 1) <> 0 then Inc(Len);	// padding
				// skip the remaining bytes of this chunk
				if (Len <> 0) then FileAcc.Skip(Len);
			end;
		end;
	end
	else
	if ID = 'IMPS' then	// Impulse Tracker sample
	begin
		ips := nil;
		ReadITSample(Module, FileAcc, Self.Index-1, ips);
	end
	else
	begin				// read file as raw 8-bit mono sample data
		Len := FileAcc.Size;
		Resize(Len);
		FileAcc.Read(Data[0], Len-1);
	end;

	ZeroFirstWord;
	FileAcc.Free;

	if (Self is TImportedSample) then
	begin
		TImportedSample(Self).isStereo := LastSampleFormat.isStereo;
		TImportedSample(Self).is16Bit  := LastSampleFormat.is16Bit;
	end;
	LastSampleFormat.Length := Length;

	Result := True;
end;

procedure TSample.LoadData(var ModFile: TFileStreamEx;
	NumSamples: Cardinal; Flags: Cardinal);
var
	i: Integer;
	Data16: array of SmallInt;
	//offset, memsize: Cardinal;
begin
	if (Self is TImportedSample) then
	with (Self as TImportedSample) do
	begin
		OrigSize := NumSamples;
		Is16Bit  :=  (Flags and ST_16) <> 0;
		IsStereo :=  (Flags and ST_STEREO) <> 0;
		IsPacked := ((Flags and ST_IT215) <> 0) or ((Flags and ST_IT214) <> 0);
	end;

	SetLength(Data, NumSamples);

	case Flags of

		// 8-bit signed PCM data
		RS_PCM8S:
			ModFile.Read(Data[0], NumSamples);

		// 16-bit signed PCM data
		RS_PCM16S:
		begin
			SetLength(Data16, NumSamples);
			ModFile.Read(Data16[0], NumSamples);
			for i := 0 to NumSamples-1 do
				Data[i] := Word(Data16[i]) div 256;
		end;

		// IT 2.14 compressed samples
		RS_IT2148, RS_IT2158:
			DecompressIT(ModFile, @Data[0], NumSamples, (Flags = RS_IT2158), False, 1, Index);

		RS_IT21416, RS_IT21516:
		begin
			SetLength(Data16, NumSamples);
			DecompressIT(ModFile, @Data16[0], NumSamples, (Flags = RS_IT21516), True, 1, Index);
			for i := 0 to NumSamples-1 do
				Data[i] := Word(Data16[i]) div 256;
		end;

		RS_IT2148S, RS_IT2158S:
		begin
			Log(TEXT_WARNING + 'Unhandled: Sample %d is packed stereo!', [Index]);
			{offset := it_decompress8(ModFile, @Data[0], Length, (Flags = RS_IT2158S), 2, Index);
			it_decompress(ModFile, @Data[1], Length, buffer + offset,
				memsize - offset, (Flags = RS_IT2158S), 2, Index);}
		end;

		RS_IT21416S, RS_IT21516S:
		begin
			Log(TEXT_WARNING + 'Unhandled: Sample %d is packed, stereo and 16-bit!', [Index]);
			{offset := it_decompress16(ModFile, @Data[0], Length, (Flags = RS_IT21516S), 2, Index);
			it_decompress16(ModFile, @Data[2], Length,
				buffer + offset, memsize - offset, (flags = RS_IT21516S), 2, Index);}
		end;

	end;

end;

procedure TSample.LoadDataFloat(var ModFile: TFileStreamEx;
	NumSamples: Cardinal; Flags: Cardinal; var Buffer: TFloatArray);
var
	i: Integer;
	divider: Single;
	//offset, memsize: Cardinal;
begin
	if (Self is TImportedSample) then
	with (Self as TImportedSample) do
	begin
		OrigSize := NumSamples;
		Is16Bit  :=  (Flags and ST_16) <> 0;
		IsStereo :=  (Flags and ST_STEREO) <> 0;
		IsPacked := ((Flags and ST_IT215) <> 0) or ((Flags and ST_IT214) <> 0);
	end;

	SetLength(Buffer, NumSamples);

	case Flags of

		// 8-bit signed PCM data
		RS_PCM8S:
		begin
			divider := 1.0 / 128.0;
			for i := 0 to NumSamples-1 do
				Buffer[i] := ShortInt(ModFile.Read8) * divider;
		end;

		// 16-bit signed PCM data
		RS_PCM16S:
		begin
			divider := 1.0 / 32768.0;
			for i := 0 to NumSamples-1 do
				Buffer[i] := SmallInt(ModFile.Read16) * divider;
		end;

		// IT 2.14 compressed samples
		RS_IT2148, RS_IT2158:
		begin
			divider := 1.0 / 128.0;
			SetLength(Data, NumSamples);
			DecompressIT(ModFile, @Data[0], NumSamples, (Flags = RS_IT2158), False, 1, Index);
			for i := 0 to NumSamples-1 do
				Buffer[i] := ShortInt(Data[i]) * divider;
		end;

		RS_IT21416, RS_IT21516:
		begin
			divider := 1.0 / 32768.0;
			SetLength(Data, NumSamples);
			DecompressIT(ModFile, @Data[0], NumSamples, (Flags = RS_IT21516), True, 1, Index);
			for i := 0 to NumSamples-1 do
				Buffer[i] := ShortInt(Data[i]) * divider;
		end;

		RS_IT2148S, RS_IT2158S,
		RS_IT21416S, RS_IT21516S:
			Log(TEXT_WARNING + 'Unhandled: Sample %d is packed stereo!', [Index]);

	end;

end;

procedure TSample.SaveToFile(const Filename: String; FileFormat: TSampleFormat);
var
	Wav: TWavWriter;
	Buf: array of ShortInt;
	Stream: TFileStreamEx;
	i, L, iffSize: Integer;
	F: AnsiString;
begin
	if Filename = '' then
	begin
		Log(TEXT_ERROR + 'No filename given!');
		Exit;
	end;

	if FileFormat = SamFmtFromExt then
	begin
		F := UpperCase(ExtractFileExt(Filename));
		if F = '.WAV' then
			FileFormat := SamFmtWAV
		else
		if F = '.IFF' then
			FileFormat := SamFmtIFF
		else
		if F = '.ITS' then
			FileFormat := SamFmtITS
		else
			FileFormat := SamFmtRAW;
	end;

	case FileFormat of

		SamFmtWAV:
		begin
			L := ByteLength;
			SetLength(Buf, L);
			for i := 0 to L-1 do
				Buf[i] := ShortInt(256 - (Data[i] - 127));

			Wav := TWavWriter.Create;
			Wav.fmt.Format := AUDIO_FORMAT_PCM;
			Wav.fmt.BitsPerSample := 8;
			Wav.fmt.Channels := 1;
			Wav.fmt.SampleRate := 16574;
			Wav.fmt.ByteRate := 16574;
			Wav.fmt.BlockAlign := 1;
			if Wav.StoreToFile(Filename) then
				Wav.WriteBuf(Buf[0], L-1)
			else
				Log(TEXT_ERROR + 'Error saving WAV file!');
			Wav.Free;
		end;

		SamFmtIFF:
		begin
			Stream := TFileStreamEx.Create(Filename, fmCreate);

			Stream.WriteString('FORM');
			iffSize := Self.Length * 2 + 100;
			Stream.Write32R(iffSize);

			Stream.WriteString('8SVXVHDR');
			Stream.Write32R($00000014);
			if LoopLength >= 2 then
			begin
				Stream.Write32R(LoopStart  * 2 and $FFFFFFE);
				Stream.Write32R(LoopLength * 2 and $FFFFFFE);
			end
			else
			begin
				Stream.Write32R(iffSize);
				Stream.Write32R(0);
			end;

			Stream.Write32R($00000000);
			Stream.Write16R($4156); 		// 16726 (rate)
			Stream.Write16R($0100); 		// numSamples and compression
			Stream.Write32R(Volume*1024);	// sample volume

			Stream.WriteString('NAME');
			Stream.Write32R($00000016);
			for L := 0 to 21 do
				Stream.Write8(Name[L]);

			Stream.WriteString('ANNO');
			Stream.Write32R(14);
			Stream.WriteString('Propulse ' + Copy(ProTracker.Util.VERSION, 1, 5)); // 'PoroTracker 0.x.x'
			//Stream.Write8(0); // even padding

			Stream.WriteString('BODY');
			Stream.Write32R(Self.Length * 2);
			for L := 0 to High(Data) do
				Stream.Write8(Data[L]);

			Stream.Free;
		end;

		SamFmtITS:
		begin
			Log(TEXT_ERROR + 'ITS saving not implemented yet!');
		end;

		SamFmtRAW:
		begin
			Stream := TFileStreamEx.Create(Filename, fmCreate);
			Stream.WriteBuffer(Data[0], System.Length(Data));
			Stream.Free;
		end;

	end;
end;

{ TImportedSample }

constructor TImportedSample.Create;
begin
	inherited;

	SetName('');
	C5freq := 0;
	Is16Bit := False;
	IsStereo := False;
	IsPacked := False;
	OrigSize := 0;
end;

end.
