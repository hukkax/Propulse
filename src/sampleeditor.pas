unit SampleEditor;

interface

uses
	Types, Classes, Math,
	CWE.Core,
	Screen.Samples, SampleView;

const
	actSelectNone		= 1;
	actSelectAll		= 2;
	actSelectPreLoop	= 3;
	actSelectPostLoop	= 4;
	actSelectLoop		= 5;

	actShowAll			= 10;
	actShowRange		= 11;
	actZoomIn			= 20;
	actZoomOut			= 21;

	actCopy				= 30;
	actCut				= 31;
	actPaste			= 32;
	actMixPaste			= 33;

	actAmplify			= 40;
	actFadeIn			= 41;
	actFadeOut			= 42;
	actCrossfade		= 45;
	actEqualizer		= 50;
	actFilterLo			= 52;
	actFilterHi			= 53;
	actFilterFlt		= 54;
	actFilterBst		= 55;
	actReverse			= 60;
	actInvert			= 61;
	actResample			= 70;
	actGenerate			= 80;

type
	TWavetype = (
		WAVE_SILENCE,
		WAVE_SINE,
		WAVE_SQUARE,
		WAVE_SAW,
		WAVE_TRIANGLE,
		WAVE_NOISE);

	TSampleEditor = class
	public
		Waveform: 	TSampleView;

		function	GetSelection(var X1, X2: Integer): Boolean;
		function	HasSelection: Boolean; inline;
		function	HasLoop: Boolean; inline;
		function	HasSample: Boolean;

		procedure 	ProcessCommand(Cmd: Integer);
		procedure 	OnCommand(Sender: TCWEControl);

		function 	MakeRoom(Pos, Len: Integer): Boolean;
		procedure 	Delete;
		procedure 	Cut;
		procedure 	Copy;
		procedure 	Paste;
		procedure 	MixPaste;

		procedure 	Amplify;
		procedure 	FadeIn;
		procedure 	FadeOut;
		procedure	CrossFade;
		procedure 	Equalizer;
		procedure 	DoFilter(Hz: Word; LP: Boolean; X1, X2: Integer);
		procedure	FilterLo(Hz: Word = 0);
		procedure	FilterHi(Hz: Word = 0);
		procedure	FilterDecreaseTreble;
		procedure	FilterIncreaseTreble;
		procedure 	Reverse;
		procedure 	Invert;
		procedure 	Resample;
		procedure	Generate;

		procedure 	GenerateAudio(Wavetype: TWavetype;
					numsamples, samplerate: Integer; frequency: Single);
	end;

var
	SampleEdit: TSampleEditor;


implementation

uses
	ShortcutManager,
	CWE.Dialogs,
	CWE.Widgets.Text,
	Dialog.ValueQuery,
	Dialog.GenerateWaveform,
	ProTracker.Util,
	ProTracker.Player,
	ProTracker.Sample,
	FloatSampleEffects;

var
	Clipbrd: array of Byte;

// --------------------------------------------------------------------------
// Utility
// --------------------------------------------------------------------------

function TSampleEditor.HasSample: Boolean;
begin
	Result := (Waveform.Sample <> nil);
	if not Result then
		ModalDialog.ShowMessage('No sample', 'No sample to operate on!')
end;

function TSampleEditor.HasLoop: Boolean;
begin
	Result := (Waveform.Sample <> nil) and (Waveform.Sample.LoopLength > 2);
	if not Result then
	begin
		if HasSample then
			ModalDialog.ShowMessage('Loop required', 'This function requires a loop.');
	end;
end;

function TSampleEditor.HasSelection: Boolean;
begin
	Result := (Waveform.Selection.R > Waveform.Selection.L) and (Waveform.Sample <> nil);
	if not Result then
	begin
		if HasSample then
			ModalDialog.ShowMessage('Selection required', 'Current selection is empty.');
	end;
end;

function TSampleEditor.GetSelection(var X1, X2: Integer): Boolean;
begin
	with Waveform do
	if (Selection.L >= 0) and (Selection.R > Selection.L) then
	begin
		X1 := Selection.L;
		X2 := Min(Selection.R, Sample.ByteLength);
		Result := not Sample.IsEmpty;
	end
	else
	if Sample <> nil then
	begin
		X1 := 0;
		X2 := Sample.ByteLength;
		Result := not Sample.IsEmpty;
	end
	else
	begin
		X1 := -1;
		X2 := -1;
		Result := False;
	end;
end;

// --------------------------------------------------------------------------
// Events
// --------------------------------------------------------------------------

procedure TSampleEditor.OnCommand(Sender: TCWEControl);
var
	Item: TCWEListItem;
begin
	CurrentScreen.MouseInfo.Control := nil;
	with Sender as TCWEList do
		Item := Items[ItemIndex];
	if Item <> nil then
		ProcessCommand(Item.Data);
end;

procedure TSampleEditor.ProcessCommand(Cmd: Integer);
var
	Sam: TSample;
begin
	if (Waveform = nil) or (Cmd = LISTITEM_HEADER) then Exit;

	Sam := Waveform.Sample;
	if Sam = nil then Exit;

	with Waveform do
	case Cmd of

		// -----------------------------------------------
		// Select

		actSelectNone:
			Selection.SetRange(0, 0);

		actSelectAll:
			Selection.SetRange(0, Sam.ByteLength);

		actSelectLoop:
			if HasLoop then
				Selection.SetRange(Sam.LoopStart*2, (Sam.LoopStart + Sam.LoopLength) * 2);

		actSelectPreLoop:
			if HasLoop then
				Selection.SetRange(0, Sam.LoopStart*2);

		actSelectPostLoop:
			if HasLoop then
				Selection.SetRange((Sam.LoopStart + Sam.LoopLength) * 2, Sam.ByteLength);

		// -----------------------------------------------
		// Show

		actShowAll:		SetViewport(0, Sam.ByteLength);
		actShowRange:	if HasSelection then
							SetViewport(Selection.L, Selection.R);
		actZoomIn:		Zoom(True);
		actZoomOut:		Zoom(False);

		// -----------------------------------------------
		// Clipboard

		actCopy:		Copy;
		actCut:			Cut;
		actPaste:		Paste;
		actMixPaste:	MixPaste;

		// -----------------------------------------------
		// Effects

		actAmplify:		Amplify;
		actFadeIn:		FadeIn;
		actFadeOut:		FadeOut;
		actCrossfade:	CrossFade;
		actEqualizer:	Equalizer;
		actFilterLo:	FilterLo(0);
		actFilterHi:	FilterHi(0);
		actFilterFlt:	FilterDecreaseTreble;
		actFilterBst:	FilterIncreaseTreble;
		actReverse:		Reverse;
		actInvert:		Invert;
		actResample:	Resample;
		actGenerate:	Generate;
	end;

	// Done!
	//
	Waveform.DrawWaveform;
	if CurrentScreen = SampleScreen then
		SampleScreen.UpdateSampleInfo;
	if ModalDialog.Dialog <> nil then
		ModalDialog.Dialog.Paint;
end;

// --------------------------------------------------------------------------
// Clipboard
// --------------------------------------------------------------------------

procedure TSampleEditor.Delete;
var
	X1, X2, L: Integer;
begin
	if HasSelection then
	with Waveform do
	begin
		GetSelection(X1, X2);

		L := Sample.ByteLength - X2;
		Move(Sample.Data[X2], Sample.Data[X1], L);
		Sample.Resize(Sample.ByteLength - (X2 - X1));

		// Fix loop points if loop enabled (from PT clone)
		if Sample.LoopLength > 1 then
		begin
			X1 := X1 div 2;
			X2 := X2 div 2;
			if X2 > Sample.LoopStart then
			begin
				if X1 < (Sample.LoopStart + Sample.LoopLength) then
				begin
					// we cut data inside the loop, increase loop length
					L := Sample.LoopLength - ((X2 - X1) and $FFFFFFFE);
					if L < 2 then L := 2;
					Sample.LoopLength := L;
				end;
			end
			else
			begin
				// We cut data before the loop, adjust loop start point
				L := (Sample.LoopStart - (X2 - X1)) and $FFFFFFFE;
				if L < 0 then
				begin
					Sample.LoopStart  := 0;
					Sample.LoopLength := 1;
				end
				else
					Sample.LoopStart := L;
			end;
		end;

		Selection.SetRange(Selection.L, Selection.L, Sample);

		Module.SetModified;
		Sample.Validate;
	end;
end;

procedure TSampleEditor.Cut;
begin
	if HasSelection then
	begin
		Module.Stop;
		Copy;
		Delete;
	end;
end;

procedure TSampleEditor.Copy;
var
	X1, X2: Integer;
begin
	if HasSelection then
	with Waveform do
	begin
		GetSelection(X1, X2);
		SetLength(Clipbrd, X2-X1+1);
		Move(Sample.Data[X1], Clipbrd[0], X2-X1);
	end;
end;

function TSampleEditor.MakeRoom(Pos, Len: Integer): Boolean;
var
	X1, oldLen: Integer;
begin
	if not HasSample then Exit(False);

	with Waveform do
	begin
		if IsEmptySample(Sample) then
			oldLen := 0
		else
			oldLen := Sample.ByteLength;

		Sample.Resize(oldLen + Len);
		if (oldLen > 0) and (Pos < oldLen) then
			Move(Sample.Data[Pos], Sample.Data[Pos + Len],
				Sample.ByteLength - Pos - Len );

		if Sample.LoopLength > 1 then // loop enabled?
		begin
			X1 := Selection.L div 2;
			Len := Len div 2;
			if X1 > Sample.LoopStart then
			begin
				if X1 < (Sample.LoopStart + Sample.LoopLength) then
				begin
					// we added data inside the loop, increase loop length
					Sample.LoopLength := Sample.LoopLength + Len;
					if (Sample.LoopStart + Sample.LoopLength) > Sample.Length then
					begin
						Sample.LoopStart  := 0;
						Sample.LoopLength := 1;
					end;
				end;
				// we added data after the loop, don't modify loop points
			end
			else
			begin
				// we added data before the loop, adjust loop start point
				Sample.LoopStart := Sample.LoopStart + Len;
				if (Sample.LoopStart + Sample.LoopLength) > Sample.Length then
				begin
					Sample.LoopStart  := 0;
					Sample.LoopLength := 1;
				end;
			end;
		end;
	end;

	Result := ((Pos + Len) < Waveform.Sample.ByteLength);
end;

procedure TSampleEditor.Paste;
var
	L: Integer;
	B: Boolean;
begin
	with Waveform do
	begin
		if not HasSample then Exit;

		Module.Stop;

		L := Length(Clipbrd);
		B := IsEmptySample(Sample);

		// pasting into an empty sample slot?
		if (B) and (L > 1) then
		begin
			Sample.Resize(L);
			Selection.L := 0;
			Selection.R := Sample.ByteLength;
		end;

		if (Selection.L >= 0) then
		begin
			if L < 1 then
				ModalDialog.ShowMessage('Clipboard empty', 'Nothing to paste.')
			else
			begin
				// was whole sample visible in view?
				if not B then
					B := (Viewport.L = 0) and (Viewport.R >= Sample.ByteLength-1);

				// replace current selection with clipboard contents?
				if Selection.Length > 1 then Delete;

				if MakeRoom(Selection.L, L) then
					Move(Clipbrd[0], Sample.Data[Selection.L], L-1);

				if B then
					Viewport.SetRange(0, Sample.ByteLength, Sample);

				Sample.Validate;
				Module.SetModified;
			end;
		end;
	end;
end;

procedure TSampleEditor.MixPaste;
var
	X, X1, X2, S: Integer;
begin
	with Waveform do
	begin
		if not HasSample then Exit;

		if Length(Clipbrd) < 1 then
		begin
			ModalDialog.ShowMessage('Clipboard empty', 'Nothing to paste.');
			Exit;
		end;

		if IsEmptySample(Sample) then
		begin
			Paste;
			Exit;
		end;

		X1 := Max(0, Selection.L);
		X2 := Min(Length(Clipbrd), Sample.ByteLength) - 1;

		if IsShiftPressed then
		begin
			for X := X1 to X2 do
			begin
				S := Trunc(ShortInt(Sample.Data[X]) + ShortInt(Clipbrd[X-X1]));
				if S < -128 then
					S := -128
				else
				if S > 127 then
					S := 127;
				Sample.Data[X] := ShortInt(S);
			end;
		end
		else
		begin
			for X := X1 to X2 do
				Sample.Data[X] := ShortInt(
					Trunc((ShortInt(Sample.Data[X]) / 2) + (ShortInt(Clipbrd[X-X1]) / 2)));
		end;

		Sample.Validate;
		Module.SetModified;
	end;
end;

// --------------------------------------------------------------------------
// Effects
// --------------------------------------------------------------------------

procedure TSampleEditor.Amplify;
var
	X1, X2: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		AskValue(ACTION_AMPLIFY_SAMPLE, 'Sample amplification %:', 0, 300,
			Trunc(Waveform.Sample.GetNormalizationValue(X1, X2) * 100),
			SampleScreen.DialogCallback);
	end;
end;

procedure TSampleEditor.FadeIn;
var
	X1, X2, x, L: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		L := X2 - X1;
		for x := 0 to L-1 do
			ShortInt(Waveform.Sample.Data[x+X1]) :=
				Trunc(ShortInt(Waveform.Sample.Data[x+X1]) * (x / L));
		Module.SetModified;
	end;
end;

procedure TSampleEditor.FadeOut;
var
	X1, X2, x, L: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		L := X2 - X1;
		for x := 0 to L-1 do
			ShortInt(Waveform.Sample.Data[x+X1]) :=
				Trunc(ShortInt(Waveform.Sample.Data[x+X1]) * (1.0 - (x / L)));
		Module.SetModified;
	end;
end;

procedure TSampleEditor.CrossFade;
var
	h, i, e, X1, X2: Integer;
	V: Byte;
begin
	with Waveform do
	begin
		if (not HasSample) or (IsEmptySample(Sample)) then Exit;

		if not GetSelection(X1, X2) then Exit;

		e := X2-1;
		h := X1 + ((X2 - X1) div 2);
		h := Min(h, Sample.ByteLength) - 1;

		with Sample do
		for i := X1 to h do
		begin
			V := Data[e];
			Data[e] := ShortInt(
				Trunc((ShortInt(Data[e]) / 2) + (ShortInt(Data[i]) / 2)));
			Data[i] := ShortInt(
				Trunc((ShortInt(V) / 2) + (ShortInt(Data[i]) / 2)));
			Dec(e);
		end;

		Sample.ZeroFirstWord;
		Module.SetModified;
	end;
end;

procedure TSampleEditor.Equalizer;
begin
end;

procedure TSampleEditor.DoFilter(Hz: Word; LP: Boolean; X1, X2: Integer);
var
	buf: TFloatArray;
	Sam: TSample;
	i: Integer;
begin
	Sam := GetCurrentSample;
	if Sam = nil then Exit;

	Sam.ValidateCoords(X1, X2);
	Sam.GetFloatData(X1, X2, buf);
	Filter(buf, Hz, LP);

	for i := X1 to X2 do
		Sam.Data[i] := ShortInt(Trunc(buf[i-X1] * 127));

	Sam.ZeroFirstWord;
	Module.SetModified;
end;

procedure TSampleEditor.FilterLo(Hz: Word = 0);
var
	X1, X2: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		if Hz = 0 then
			AskValue(ACTION_FILTER_LOWPASS, 'Lowpass Filter Frequency (Hz):',
				1, FILTERS_BASE_FREQ div 2, 1000,
				SampleScreen.DialogCallback)
		else
			DoFilter(Hz, True, X1, X2);
	end;
end;

procedure TSampleEditor.FilterHi(Hz: Word = 0);
var
	X1, X2: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		if Hz = 0 then
			AskValue(ACTION_FILTER_HIGHPASS, 'Highpass Filter Frequency (Hz):',
				1, FILTERS_BASE_FREQ div 2, 1000,
				SampleScreen.DialogCallback)
		else
			DoFilter(Hz, False, X1, X2);
	end;
end;

function ROUND_SMP_D(x: Single): Integer; inline;
begin
	if x >= 0.0 then
		Result := Floor(x + 0.5)
	else
		Result := Ceil(x - 0.5);
end;

procedure TSampleEditor.FilterDecreaseTreble;
var
	i, X1, X2: Integer;
	Sam: TSample;
	D: Single;
begin
	if GetSelection(X1, X2) then
	begin
		Sam := GetCurrentSample;
		if Sam = nil then Exit;
		Sam.ValidateCoords(X1, X2);

		for i := X1 to X2-1 do
		begin
			D := (ShortInt(Sam.Data[i]) + ShortInt(Sam.Data[i+1])) / 2.0;
			Sam.Data[i] := ShortInt(Clamp(ROUND_SMP_D(D), -128, +127));
		end;
		Sam.ZeroFirstWord;
		Module.SetModified;
	end;
end;

procedure TSampleEditor.FilterIncreaseTreble;
var
	i, X1, X2: Integer;
	Sam: TSample;
	tmp16_1, tmp16_2, tmp16_3: SmallInt;
begin
	if GetSelection(X1, X2) then
	begin
		Sam := GetCurrentSample;
		if Sam = nil then Exit;
		Sam.ValidateCoords(X1, X2);

		tmp16_3 := 0;
		for i := X1 to X2 do
		begin
			tmp16_1 := ShortInt(Sam.Data[i]);
			tmp16_2 := tmp16_1;
			Dec(tmp16_1, tmp16_3);
			tmp16_3 := tmp16_2;
			Inc(tmp16_2, CLAMP(ROUND_SMP_D(tmp16_1 / 4), -128, 127));
			Sam.Data[i] := ShortInt(Clamp(tmp16_2, -128, +127));
		end;

		Sam.ZeroFirstWord;
		Module.SetModified;
	end;
end;

procedure TSampleEditor.Reverse;
var
	X1, X2: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		Waveform.Sample.Reverse(X1, X2);
		Module.SetModified;
	end;
end;

procedure TSampleEditor.Invert;
var
	X1, X2: Integer;
begin
	if GetSelection(X1, X2) then
	begin
		Waveform.Sample.Invert(X1, X2);
		Module.SetModified;
	end;
end;

procedure TSampleEditor.Resample;
begin
	Module.Stop;
end;

procedure TSampleEditor.Generate;
begin
	Module.Stop;
	Dialog_GenerateWaveform;
end;

// adapted from http://www.joelstrait.com/nanosynth_create_sound_with_ruby/
//
procedure TSampleEditor.GenerateAudio(Wavetype: TWavetype;
	numsamples, samplerate: Integer; frequency: Single);
var
	position_in_period,
	position_in_period_delta: Single;
	Data: Single;
	X, i: Integer;
begin
	if not HasSample then Exit;

	X := Max(Waveform.Selection.L, 0);
	if not MakeRoom(X, numsamples) then
	begin
		ModalDialog.ShowMessage('Internal Error', 'Could not make room for sample data.');
		Exit;
	end;

	position_in_period := 0.0;
	position_in_period_delta := frequency / samplerate;
	Data := 0;

	for i := 0 to numsamples-1 do
	begin
		case Wavetype of
			WAVE_SINE:		Data := Sin(position_in_period * (Pi * 2));
			WAVE_SQUARE:	if position_in_period >= 0.5 then Data := 1.0 else Data := -1.0;
			WAVE_SAW:		Data := ((position_in_period * 2.0) - 1.0);
			WAVE_TRIANGLE:	Data := 1.0 - Abs(((position_in_period * 2.0) - 1.0) * 2.0);
			WAVE_NOISE:		begin Waveform.Sample.Data[X+i] := Random(255); Continue; end;
		end;

		Waveform.Sample.Data[X+i] := ShortInt(Trunc(Data * 127));

		position_in_period := position_in_period + position_in_period_delta;
		if position_in_period >= 1.0 then
			position_in_period := position_in_period - 1.0;
	end;

	with Waveform do
	begin
		Sample.Validate;
		Viewport.SetRange(0, Sample.ByteLength, Sample);
		Selection.SetRange(X, X+numsamples);
		DrawWaveform;
	end;

	if CurrentScreen = SampleScreen then
		SampleScreen.UpdateSampleInfo;

	Module.SetModified;
end;

initialization

	SampleEdit := TSampleEditor.Create;

finalization

	SampleEdit.Free;


end.
