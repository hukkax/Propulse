unit ProTracker.Paula;

interface

uses
	ProTracker.Util,
	ProTracker.Sample;

type
	TPaulaVoice = class
	public
		Enabled,
		HasSound: Boolean;

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
		QueuedSample: SmallInt;
		QueuedOffset: Cardinal;
		PlayPos:      Integer;

		constructor Create(OutputFreq: Word);

		procedure Kill;
		procedure TurnOffDMA; inline;
		procedure RestartDMA;
		procedure SetData(NewSample: Byte; NewOffset: Integer); inline;
		procedure SetDataPtr(const src: PArrayOfShortInt); inline;
		procedure SetLength(len: Cardinal); inline;
		procedure SetPeriod(period: Word); inline;
		procedure SetVolume(vol: Word);
	end;

var
	EmptySample: TSample;


implementation

uses
	Math,
	ProTracker.Player;


constructor TPaulaVoice.Create(OutputFreq: Word);
begin
	inherited Create;

	f_OutputFreq := OutputFreq;

	SRC_DAT := nil;
	DMA_DAT := nil;

	Sample := -1;
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
	DMA_LEN := Max(SRC_LEN, 2);
	PlayPos := -1;
	Sample := QueuedSample;

	if (Enabled) and (Sample >= 0) and (Sample < Module.Samples.Count) then
	begin
		with Module.Samples[Sample] do
		begin
			PlayPos := QueuedOffset;
			Age := 6;//Trunc(6 * SRC_VOL) + 2;
		end;
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
	Sample := -1;
	QueuedSample := -1;
	HasSound := False;
end;

procedure TPaulaVoice.SetPeriod(period: Word);
begin
	// This is what really happens on Paula on a real Amiga
	// on normal video modes. Tested and confirmed by 8bitbubsy!
	if period > 0 then
	begin
		DELTA := (PAULA_PAL_CLK / Max(period, 113)) / f_outputFreq;
		HasSound := True;
	end
	else
	begin
		DELTA := 0.0;
		HasSound := False;
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

procedure TPaulaVoice.SetData(NewSample: Byte; NewOffset: Integer);
begin
	if NewOffset >= 0 then
	begin
		SRC_DAT := @Module.Samples[NewSample].Data[NewOffset];
		QueuedSample := NewSample;
		QueuedOffset := NewOffset;
	end
	else
		SetDataPtr(nil);
end;

procedure TPaulaVoice.SetDataPtr(const src: PArrayOfShortInt);
begin
	if src <> nil then
		SRC_DAT := src
	else
	begin
		SRC_DAT := @EmptySample.Data[0];
		QueuedSample := -1;
	end;
end;

initialization

	EmptySample := TSample.Create;
	EmptySample.Resize(16);

finalization

	EmptySample.Free;

end.
