unit FloatSampleEffects;

interface

uses
	Types, ProTracker.Util;

const
	FILTERS_BASE_FREQ = 16574;


	function 	LimitSample(sample: Single): Single; inline;
	procedure 	Amplify(var buffer: TFloatArray; amp: Single; dB: Boolean);
	procedure 	Normalize(var buffer: TFloatArray; maxdB: Single = 0.0);
	procedure	Equalize(var buffer: TFloatArray;
				lowfreq, highfreq, mixfreq: Word;
				BassGain, MidGain, HighGain: Single);
	procedure 	Filter(var buffer: TFloatArray; Cutoff: Word; LP: Boolean);


implementation

uses
	Math,
	ProTracker.Filters;


function LimitSample(sample: Single): Single; inline;
const
	MINMAX = 1.0;
begin
	if sample <= -MINMAX then
		Result := -MINMAX
	else
	if sample >= MINMAX then
		Result := MINMAX
	else
		Result := sample;
end;

procedure Amplify(var buffer: TFloatArray; amp: Single; dB: Boolean);
var
	i: Integer;
begin
	if dB then
		amp := DecibelToLinear(amp);
	//Log('Amplify: amp=%f', [amp]);
	for i := 0 to Length(buffer)-1 do
		buffer[i] := LimitSample(buffer[i] * amp);
end;

procedure Normalize(var buffer: TFloatArray; maxdB: Single = 0.0);
var
	i: Integer;
	sam, amp: Single;
begin
	if (buffer = nil) or (Length(buffer) < 1) then Exit;

	amp := 0;
	for i := 0 to Length(buffer)-1 do
	begin
		sam := Abs(buffer[i]);
		if sam > amp then
			amp := sam;
	end;

	amp := 1.0 / amp;

	if (maxdB > 0.1) and (amp <= 1.0) then
	begin
		// sam := DecibelToLinear(maxdB);
		amp := maxdB;
		//	Log('Normalize: amp = %f - %f', [amp, sam]);
	end;

	//if amp > 0.99 then
	begin
		//Log('Normalize: amp = %f', [amp]);
		Amplify(buffer, amp, False);
	end;
end;

// public domain, adapted from
// http://www.musicdsp.org/showArchiveComment.php?ArchiveID=236
procedure Equalize(var buffer: TFloatArray;
	lowfreq, highfreq, mixfreq: Word;
	BassGain, MidGain, HighGain: Single);
const
	vsa = 1.0 / 4294967295.0; // Very small amount (Denormal Fix)
var
	i: Integer;
	l, m, h,
	lf, f1p0, f1p1, f1p2, f1p3,
	hf, f2p0, f2p1, f2p2, f2p3,
	sdm1, sdm2, sdm3,
	lg, mg, hg: Single;
	sample: Single;
begin
	{ (880, 5000, 44100, 1.5, 0.75, 1.0) ->
	  eq.lg := 1.5; 	// Boost bass by 50%
	  eq.mg := 0.75; 	// Cut mid by 25%
	  eq.hg := 1.0; 	// Leave high band alone }

	lg := {1 +} (BassGain {/ 100});
	mg := {1 +} (MidGain  {/ 100});
	hg := {1 +} (HighGain {/ 100});

	// Calculate filter cutoff frequencies
	lf := 2 * Sin(PI * (lowfreq  / mixfreq));
	hf := 2 * Sin(PI * (highfreq / mixfreq));

	f1p0 := 0;	f1p1 := 0;	f1p2 := 0;	f1p3 := 0;
	f2p0 := 0;	f2p1 := 0;	f2p2 := 0;	f2p3 := 0;
	sdm1 := 0;  sdm2 := 0;  sdm3 := 0;

	//Log('Equalize %d', [Length(buffer)]);

	for i := 0 to Length(buffer)-1 do
	begin
		sample := buffer[i];

		// Filter #1 (lowpass)
		f1p0 := f1p0 + (lf * (sample - f1p0)) + vsa;
		f1p1 := f1p1 + (lf * (f1p0 - f1p1));
		f1p2 := f1p2 + (lf * (f1p1 - f1p2));
		f1p3 := f1p3 + (lf * (f1p2 - f1p3));

		l := f1p3;

		// Filter #2 (highpass)
		f2p0 := f2p0 + (hf * (sample - f2p0)) + vsa;
		f2p1 := f2p1 + (hf * (f2p0 - f2p1));
		f2p2 := f2p2 + (hf * (f2p1 - f2p2));
		f2p3 := f2p3 + (hf * (f2p2 - f2p3));

		h := sdm3 - f2p3;

		// Calculate midrange (signal - (low + high))
		m := sdm3 - (h + l);

		// Scale, Combine and store
		l := l * lg;
		m := m * mg;
		h := h * hg;

		// Shuffle history buffer
		sdm3 := sdm2;
		sdm2 := sdm1;
		sdm1 := sample;

		// Return result
		buffer[i] := LimitSample(l + m + h);
	end;
end;

procedure Filter(var buffer: TFloatArray; Cutoff: Word; LP: Boolean);
var
	i: Integer;
	_in, _out: array [0..1] of Single;
	filter: TLossyIntegrator;

	procedure DoFilter;
	begin
		_in[0] := buffer[i];
		if LP then
			lossyIntegrator(@filter, @_in, @_out)
		else
			lossyIntegratorHighPass(@filter, @_in, @_out);
		buffer[i] := LimitSample(_out[0]);
	end;

begin
	CutOff := Max(1, Min(CutOff, FILTERS_BASE_FREQ div 2));

	filter.coeff[0] := Tan(M_PI_F * CutOff / FILTERS_BASE_FREQ);
	filter.coeff[1] := 1.0 / (1.0 + filter.coeff[0]);

	filter.buffer[0] := 0.0;
	for i := 0 to Length(buffer)-1 do
		DoFilter;

	filter.buffer[0] := 0.0;
	for i := Length(buffer)-1 downto 0 do
		DoFilter;
end;

end.
