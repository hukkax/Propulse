unit ProTracker.Sample;

{$mode delphi}

interface

uses
    soxr,
	ProTracker.Util,
	FileStreamEx;

const
	// Sample unpacking

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
	private
	public
		Data:			packed array of Byte;
		Name:			packed array [0..21] of AnsiChar;
		Finetune:		ShortInt;
		Volume:			Byte;
		Length:			Cardinal;	// data length in words
		LoopStart,
		LoopLength,
		tmpLoopStart:	Word;		// positions in words
		Age: 			ShortInt;
		FileOffset:		Cardinal;
		Index:			Byte; // 1-based!

		function 		IsEmpty: Boolean; inline;
		function 		IsLooped: Boolean; inline;

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
		procedure		LoadFromFile(const Filename: String);
		procedure 		SaveToFile(const Filename: String; FileFormat: TSampleFormat);

		function 		GetName: AnsiString;
		procedure 		UpdateVoice;
		procedure		SetName(const S: AnsiString);
		function  		SetLoopStart(WordPos: Integer): Boolean;
		function  		SetLoopEnd(WordPos: Integer): Boolean;
		procedure		ZeroFirstWord;
		procedure 		Clear;
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


implementation

uses
	Math, Classes, SysUtils,
	//{$IFDEF WINDOWS}Windows,{$ENDIF}
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
// Impulse Tracker packed sample decompression
// ==========================================================================

//{$R+}

function ReadbitsIT(var ModFile: TFileStreamEx;
	n: ShortInt; var srcpos, bitbuf, bitnum: Cardinal): Cardinal;
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
	value: uint32;                 // value read from file to be processed
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
		d1 := 0; d2 := 0; // reset integrator buffers

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
					if (value = Cardinal(1 shl (width - 1))) then
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
					if (value > borderx) and (value <= Cardinal(borderx) + 16) then
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
					vx := int16(value shl shift);
					vx := int16(vx shr shift);
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
					if (value = 1 shl (width - 1)) then
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
				if (value and $100) <> 0 then // bit 8 set?
				begin
					width := (value + 1) and $FF; // new width...
					Continue; // ... and next value
				end;

				// now expand value to signed byte
				if width < 8 then
				begin
					shift := 8 - width;
					v := ShortInt(value shl shift);
					v := ShortInt(v shr shift);
				end
				else
					v := int8(value);

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

//	Log('Done, pos=%d, destpos=%d', [srcpos, destpos] );

	Result := srcpos;
end;

//{$R-}

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
	Result := not (High(Data) >= 1);
end;

function TSample.IsLooped: Boolean;
begin
	Result := ((LoopLength + LoopStart) > 1);
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
	if (X2 < X1) or (X2 > L) then
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
	Length     := 0;
	LoopStart  := 0;
	LoopLength := 1;
	Volume     := 64;
	Finetune   := 0;
	Age        := -1;
	ZeroFirstWord;
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
//	sb: ShortInt;
begin
	if not SOXRLoaded then Exit;

	if (ibuf = nil) or (System.Length(ibuf) < 1) then Exit;

	ilen := System.Length(ibuf);
	olen := ilen; //Round(ilen * (DestHz / OrigHz) + 0.5);
	odone := ilen;
	SetLength(obuf, olen + 1);

	if DoHighBoost then
	begin
		FloatSampleEffects.Normalize(ibuf, 0.9);
		FloatSampleEffects.Equalize(ibuf, 880, 5000, OrigHz, 1.0, 1.0, 1.6);
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

	odone := Min(odone, $1FFFF+1); // limit sample length to 127KB
	Length := odone div 2;
	SetLength(Data, Length*2 + 1);
	LoopStart := 0;
	LoopLength := 1;

	if DoNormalize then
		FloatSampleEffects.Normalize(obuf);

	for i := 0 to odone-1 do
	begin
		//sb := ShortInt(Trunc(obuf[i] * 127));
		//if (sb < -128) then
		//	sb := -128;
		{else
		if (sb > 127) then
			sb := 127;}
		Data[i] := ShortInt(Trunc(obuf[i] * 127));
	end;

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
	l := ByteLength;
	for x := 0 to l div 2 -1 do
		Data[x] := Data[x * 2];

	Length := Length div 2;
	LoopStart := LoopStart div 2;
	LoopLength := LoopLength div 2;
	SetLength(Data, Max(l div 2, Length * 2)+1);

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
	if ByteLength >= $1FFFF then
		Resize($1FFFF-1)
	else
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

procedure TSample.LoadFromFile(const Filename: String);
var
	Wav: TWavReader;
	FileAcc: TFileStreamEx;
	ID, sName: AnsiString;
	X, i: Integer;
	Len, WavLen: Cardinal;
	isStereo, is16Bit: Boolean;
	ips: TImportedSample;
	Buf: array of SmallInt;
begin
	FileAcc := TFileStreamEx.Create(Filename, fmOpenRead, fmShareDenyNone);

	Self.Volume := 64;
	Self.Finetune := 0;
	Self.LoopStart  := 0;
	Self.LoopLength := 1;
	Self.SetName(ExtractFileName(Filename));

	ID := FileAcc.ReadString(False, 4);

	if ID = 'RIFF' then	// WAV
	begin
		Wav := TWavReader.Create;
		FileAcc.SeekTo(0);
		Wav.LoadFromStream(FileAcc);

		case Wav.fmt.Channels of
			1:  isStereo := False;
			2:  isStereo := True;
		else
			Log(TEXT_ERROR + 'WAV loader: %d-channel samples not supported!', [Wav.fmt.Channels]);
			Exit;
		end;

		case Wav.fmt.BitsPerSample of
			8:  is16Bit := False;
			16: is16Bit := True;
		else
			Log(TEXT_ERROR + 'WAV loader: %d-bit samples not supported!', [Wav.fmt.BitsPerSample]);
			Exit;
		end;

		WavLen := Wav.dataSize;
		Len := WavLen;

		if isStereo then Len := Len div 2;
		if is16Bit  then Len := Len div 2;

		SetLength(Data, Len + 1);
		Self.Length := Len div 2;

		if (not isStereo) and (not is16Bit) then
		begin
			// mono 8-bit
			Wav.ReadBuf(Data[0], WavLen);
			for X := 0 to System.Length(Data)-1 do
				Data[X] := 127 - Data[X];
		end
		else
		if is16Bit then
		begin
			SetLength(Buf, WavLen + 1);
			Wav.ReadBuf(Buf[0], WavLen);
			if isStereo then
			begin
				// stereo 16-bit
				for X := 0 to Len do
					Data[X] := (Buf[X*2] + Buf[X*2+1]) div 512;
			end
			else
				// mono 16-bit
				for X := 0 to Len do
					Data[X] := Buf[X] div 256;
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
			for i := 0 to NumSamples - 1 do
				Data[i] := (ModFile.Read16 div 256);

		// IT 2.14 compressed samples
		RS_IT2148, RS_IT2158:
			DecompressIT(ModFile, @Data[0], NumSamples, (Flags = RS_IT2158), False, 1, Index);

		RS_IT21416, RS_IT21516:
		begin
			SetLength(Data16, NumSamples);
			DecompressIT(ModFile, @Data16[0], NumSamples, (Flags = RS_IT21516), True, 1, Index);
			for i := 0 to NumSamples-1 do
				Data[i] := Data16[i] div 256;
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
				Buffer[i] := SmallInt(Data[i]) * divider;
		end;

		RS_IT2148S, RS_IT2158S:
			Log(TEXT_WARNING + 'Unhandled: Sample %d is packed stereo!', [Index]);

		RS_IT21416S, RS_IT21516S:
			Log(TEXT_WARNING + 'Unhandled: Sample %d is packed, stereo and 16-bit!', [Index]);

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
			L := System.Length(Data);//Self.Length * 2;
			SetLength(Buf, L);
			for i := 0 to L-1 do
				Buf[i] := 256 - (Data[i] - 127);
			Wav := TWavWriter.Create;

			Wav.fmt.Format := AUDIO_FORMAT_PCM;
			Wav.fmt.BitsPerSample := 8;
			Wav.fmt.Channels := 1;
			Wav.fmt.SampleRate := 16574;
			Wav.fmt.ByteRate := 16574;
			Wav.fmt.BlockAlign := 1;

			if not Wav.StoreToFile(Filename) then
				Log(TEXT_ERROR + 'Error saving WAV file!');

			Wav.WriteBuf(Buf[0], L-1);
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
