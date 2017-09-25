unit ProTracker.Paula;

interface

uses
	ProTracker.Util;

type
	TPaulaVoice = class
	public
		Enabled: Boolean;

		SRC_DAT: PArrayOfShortInt;
		SRC_LEN: uint32;
		SRC_VOL: Single;

		DMA_DAT: PArrayOfShortInt;
		DMA_LEN: uint32;
		DMA_POS: uint32;

		f_OutputFreq: Single;
		PANL, PANR:   Single;

		DELTA, LASTDELTA: Single;
		FRAC, LASTFRAC:   Single;

		Volume: Single;

		Sample,
		QueuedSample: Byte;
		QueuedOffset: Cardinal;
		PlayPos:      Integer;

		constructor Create(OutputFreq: Word);

		procedure Kill;
		procedure TurnOffDMA; inline;
		procedure RestartDMA;
		procedure SetData(NewSample, NewOffset: Cardinal); inline; // can't inline due to compiler bug!
		procedure SetDataPtr(const src: PArrayOfShortInt); inline;
		procedure SetLength(len: Cardinal); inline;
		procedure SetPeriod(period: Word); inline;
		procedure SetVolume(vol: Word);
	end;


implementation

uses
	ProTracker.Player;


constructor TPaulaVoice.Create(OutputFreq: Word);
begin
	inherited Create;

	f_OutputFreq := OutputFreq;

	SRC_DAT := nil;
	DMA_DAT := nil;

	Sample := 31;
	QueuedSample := Sample;
end;

procedure TPaulaVoice.TurnOffDMA;
begin
	Volume := 0;
end;

procedure TPaulaVoice.RestartDMA;
begin
	FRAC := 0.0;
	DMA_POS := 0;
	DMA_DAT := SRC_DAT;
	DMA_LEN := SRC_LEN;
	PlayPos := -1;
	Sample := QueuedSample;

	if Enabled then
	with Module.Samples[Sample] do
	begin
		PlayPos := QueuedOffset;
		Age := 3;
	end;

	QueuedOffset := 0;
	Volume := SRC_VOL;
end;

procedure TPaulaVoice.Kill;
begin
	DMA_DAT := nil;
	SRC_DAT := nil;
	SRC_LEN := 0;
	DMA_LEN := 0;
	DMA_POS := 0;
	SRC_VOL := 0;
	DELTA   := 0;
	FRAC    := 0;
	LASTDELTA := 0;
	LASTFRAC  := 0;
	Volume  := 0;
	PlayPos := -1;
end;

procedure TPaulaVoice.SetPeriod(period: Word);
begin
	// This is what really happens on Paula on a real Amiga
	// on normal video modes. Tested and confirmed by 8bitbubsy!
	if period = 0 then
		DELTA := 0.0
	else
	begin
		if period < 113 then
			period := 113;
		DELTA := (PAULA_PAL_CLK / period) / f_outputFreq;
	end;
	if LASTDELTA = 0.0 then
		LASTDELTA := DELTA;
end;

procedure TPaulaVoice.SetVolume(vol: Word);
var
	SV: Single;
begin
	if (vol and (1 shl 6)) <> 0 then
		vol := $0040
	else
		vol := vol and $003F;

	SV := SRC_VOL;
	SRC_VOL := vol * (1.0 / 64.0);
	if SV <> SRC_VOL then
		Volume := SRC_VOL;
end;

procedure TPaulaVoice.SetLength(len: Cardinal);
begin
	SRC_LEN := len * 2;
end;

procedure TPaulaVoice.SetData(NewSample, NewOffset: Cardinal);
begin
	SRC_DAT := @Module.Samples[NewSample].Data[NewOffset];
	QueuedSample := NewSample;
	QueuedOffset := NewOffset;
end;

procedure TPaulaVoice.SetDataPtr(const src: PArrayOfShortInt);
begin
	SRC_DAT := src;
	//if src = nil then TurnOffDMA;
end;


end.
