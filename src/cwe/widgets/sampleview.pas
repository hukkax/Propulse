unit SampleView;

interface

uses
	Classes, Types, Math, Graphics32, TextMode,
	CWE.Core, CWE.Widgets.Scrollers,
	ProTracker.Util, ProTracker.Player, ProTracker.Sample;


type
	TSampleRange = record
		L, R,
		Origin:		Integer;
		procedure	SetRange(aL, aR: Integer; const Sample: TSample = nil);
		function	Length: Integer; inline;
	end;

	TSampleView = class(TCWEScrollableControl)
	const
		BOXSIZE = 4;

		MOUSE_NONE      = 0;
		MOUSE_SELECT    = 1;
		MOUSE_SETLOOP_L = 2;
		MOUSE_SETLOOP_R = 3;
		MOUSE_DRAW      = 4;
	class var
		COLOR_BACKGROUND,
		COLOR_WAVEFORM,
		COLOR_WAVEFORM_PEAKS,
		COLOR_OVERRUN,
		COLOR_CENTERLINE,
		COLOR_LOOP,
		COLOR_LOOP_HOVER,
		COLOR_SEL_FORE,
		COLOR_SEL_BACK,
		COLOR_PLAYBACK:		Byte;
	private
		MouseAction: Byte;
		PrevMousePos: TPoint;
		BoxL, BoxR,
		PixelRect:	TRect;
		BmCache:	TBitmap32;
		FSample:	TSample;
		Step:		Single;
		HalfHeight:	Integer;

		procedure	SetSample(const Sample: TSample);
		procedure 	InitCachedBitmap;

		function 	SampleToPixelPos(pos: Integer): Integer; inline;
		function 	PixelToSamplePos(x, mx: Integer): Integer; inline;
		function 	PixelToSampleValue(Y: Integer): SmallInt; inline;
		function 	GetSampleY(X: Integer): Integer; inline;
		function 	GetSamplePeakY(X: Integer): Integer;
	public
		ReadOnly:	Boolean;

		Selection,
		Viewport:	TSampleRange;

		procedure	Init(AllowEditing, AddBorder: Boolean);
		procedure	DrawWaveform;
		procedure	SetViewport(aL, aR: Integer);
		procedure 	Zoom(ZoomIn: Boolean);

		function	MouseDownEvent(Sender: TCWEControl;
					Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
		function	MouseUpEvent(Sender: TCWEControl;
					Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
		function	MouseWheelEvent(Sender: TCWEControl;
					Shift: TShiftState; DirDown: Boolean; P: TPoint): Boolean;
		function 	MouseMoveEvent(Sender: TCWEControl; X, Y: Integer; P: TPoint): Boolean;
		function 	ScrollEvent(Sender: TCWEControl; NewOffset: Cardinal): Boolean;
		procedure	ScrollBy(Amount: Integer); override;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		destructor  Destroy; override;
		procedure 	Paint; override;

		property	Sample: TSample read FSample write SetSample;
	end;


implementation

uses
	CWE.Dialogs,
	Screen.Samples;

// --------------------------------------------------------------------------
{ TSampleRange }
// --------------------------------------------------------------------------

function TSampleRange.Length: Integer;
begin
	Result := Abs(R-L);
end;

procedure TSampleRange.SetRange(aL, aR: Integer; const Sample: TSample = nil);
begin
	L := Min(aL, aR);
	R := Max(aR, aL);

	L := Max(L, 0);
	if Sample <> nil then
	begin
		R := Min(R, Sample.ByteLength);
		L := Min(L, R);
	end;

{	if (L = R) then
		R := 0
	else}
end;

// --------------------------------------------------------------------------
{ TSampleView }
// --------------------------------------------------------------------------

constructor	TSampleView.Create(Owner: TCWEControl;
	const sCaption, sID: AnsiString; const Bounds: TRect;
	IsProtected: Boolean = False);
begin
	inherited;

	MouseAction := MOUSE_NONE;
	BmCache := TBitmap32.Create;
	InitCachedBitmap;
	GetPixelRect(PixelRect);
end;

destructor TSampleView.Destroy;
begin
	BmCache.Free;
end;

procedure TSampleView.Init(AllowEditing, AddBorder: Boolean);
begin
	ReadOnly := not AllowEditing;

	WantKeyboard := False;
	WantMouse    := AllowEditing;

	if AddBorder then
		SetBorder(True, True, True, True);

	if AllowEditing then
	begin
		WantPixelPrecision := True;

		OnMouseDown  := MouseDownEvent;
		OnMouseWheel := MouseWheelEvent;
		OnMouseMove  := MouseMoveEvent;
		OnMouseUp    := MouseUpEvent;

		CreateScrollbar;
		Scrollbar.Horizontal := True;
		Scrollbar.AdjustBounds;
		OnScroll := ScrollEvent;
	end;
end;

procedure TSampleView.InitCachedBitmap;
var
	P: TPoint;
begin
	P := GetPixelSize;
	if (P.X <> BmCache.Width) or (P.Y <> BmCache.Height) then
		BmCache.SetSize(P.X, P.Y);
end;

function TSampleView.GetSampleY(X: Integer): Integer;
begin
	Result := Trunc( ((127 - ShortInt(FSample.Data[PixelToSamplePos(X,-1)])) / 127) * HalfHeight);
end;

function TSampleView.GetSamplePeakY(X: Integer): Integer;
var
	i, ay, y, v, p1, p2: Integer;
begin
	if X = 0 then
		y := ShortInt(FSample.Data[0])
	else
	begin
		p1 := PixelToSamplePos(X-1, -1);
		p2 := PixelToSamplePos(X,   -1);
		y := 0; ay := 0;
		for i := p1 to p2 do // find max. peak from range
		begin
			v := ShortInt(FSample.Data[i]);
			if Abs(v) > ay then
			begin
				y := v;
				ay := Abs(v);
			end;
		end;
	end;
	Result := Trunc( ((127 - y) / 127) * HalfHeight);
end;

{begin
	Result := Trunc( ((127 - ShortInt(FSample.Data[PixelToSamplePos(X,-1)])) / 127) * HalfHeight);
end;}

// sample pos -> pixel x pos
function TSampleView.SampleToPixelPos(pos: Integer): Integer;
var
	scaledPos_f: Single;
begin
	scaledPos_f := (pos / Viewport.Length) * (BmCache.Width-1);
	Result := Round(scaledPos_f);
	if Viewport.Length > 0 then
	begin
		scaledPos_f := (Offset / Viewport.Length) * (BmCache.Width-1);
		Dec(Result, Round(scaledPos_f));
	end;
end;

// pixel x pos -> sample pos
// set mx = -1 when drawing, 0 when selecting
function TSampleView.PixelToSamplePos(x, mx: Integer): Integer;
var
	scaledPos_f: Single;
begin
	if Viewport.Length > 0 then
	begin
		scaledPos_f := (Offset / Viewport.Length) * (BmCache.Width-1);
		Inc(x, Round(scaledPos_f));
	end;
	scaledPos_f := (x / (BmCache.Width-1)) * Viewport.Length;
	Result := Min(Round(scaledPos_f), Length(FSample.Data) + mx);
end;

function TSampleView.PixelToSampleValue(Y: Integer): SmallInt;
begin
	Result := SmallInt(0 - (Trunc(Y / (BmCache.Height-1) * 255) - 127));
end;


// --------------------------------------------------------------------------
// Painting
// --------------------------------------------------------------------------

procedure TSampleView.Paint;
var
	x1, y1, x2, y2, x, y: Cardinal;
	Col: TColor32;

	function GetLoopBoxColor(Hovered: Boolean): TColor32; inline;
	begin
		if Hovered then
			Result := Console.Palette[COLOR_LOOP_HOVER]
		else
			Result := Console.Palette[COLOR_LOOP];
	end;

begin
	if ModalDialog.Dialog <> nil then Exit;

	Console.BeginUpdate;

	if Border.Enabled then
		inherited DrawBorder(AdjustedRect, ColorBack);

	Console.Bitmap.Draw(PixelRect.Left, PixelRect.Top, BmCache);

	if (Viewport.R > 0) and (IsEmptySample(FSample) = False) then
	begin
		// Don't accidentally draw outside our client area
		Console.Bitmap.ClipRect := PixelRect;

		y1 := PixelRect.Top;
		y2 := PixelRect.Bottom - 1;
		x1 := PixelRect.Left;
		x2 := PixelRect.Right  - 1;

		// Show loop points if sample is looping
		//
		if (FSample.LoopLength > 1) or (FSample.LoopStart > 0) then
		begin
			x := x1 + SampleToPixelPos(FSample.LoopStart * 2);

			if (x - PixelRect.Left) >= BOXSIZE then
				BoxL := Bounds(x-BOXSIZE, y1, BOXSIZE, BOXSIZE)
			else
				BoxL := Bounds(x, y1, BOXSIZE, BOXSIZE);

			y := x1 + SampleToPixelPos((FSample.LoopStart + FSample.LoopLength) * 2);
			if y < (PixelRect.Right - BOXSIZE) then
				BoxR := Bounds(y+1, y1, BOXSIZE, BOXSIZE)
			else
				BoxR := Bounds(y-BOXSIZE, y1, BOXSIZE, BOXSIZE);

			with Console.Bitmap do
			begin
				Col := GetLoopBoxColor(MouseAction = MOUSE_SETLOOP_L);
				VertLine(x, y1, y2, Col);
				FillRect(BoxL, Col);

				Col := GetLoopBoxColor(MouseAction = MOUSE_SETLOOP_R);
				VertLine(y, y1, y2, Col);
				FillRect(BoxR, Col);
			end;
		end;

		// Show playback position if sample is playing on any channel
		//
		for y := 0 to AMOUNT_CHANNELS-1 do
		with Module.Channel[y].Paula do
		begin
			if (Sample = FSample.Index-1) and (PlayPos >= 0) then
			begin
				x := x1 + SampleToPixelPos(PlayPos); /// !!! FIXME 217
				if x < x2 then
					Console.Bitmap.VertLine(x, y1, y2, Console.Palette[COLOR_PLAYBACK]);
			end;
		end;

		// Done painting
		Console.Bitmap.ResetClipRect;
	end;

	Console.EndUpdate;
end;

procedure TSampleView.DrawWaveform;
var
	w, h, x, y, x1, x2: Integer;
	C, CP: TColor32;
	PC: PColor32;
	PaintSelection: Boolean;

	procedure DrawCenterLine;
	var
		xx: Integer;
	begin
		for xx := 1 to w div 2-1 do
			BmCache.Pixel[xx*2, HalfHeight] := Console.Palette[COLOR_CENTERLINE];
	end;

begin
	GetPixelRect(PixelRect);

	w := BmCache.Width  - 1;
	h := BmCache.Height - 1;
	HalfHeight := h div 2;

	BmCache.Clear(Console.Palette[COLOR_BACKGROUND]);

	if (Viewport.R > Viewport.L) and (IsEmptySample(Sample) = False) then
	begin
		Viewport.R := Min(Viewport.R, Sample.ByteLength);

		Step := Viewport.Length / w;

		if (Selection.L >= 0) and (Selection.R >= Selection.L) then
		begin
			x := SampleToPixelPos(Selection.L);
			y := SampleToPixelPos(Selection.R);
			x1 := Min(x, y);
			x2 := Max(x, y);
			PaintSelection := not ( (x2 < 0) or (x1 > w) );
		end
		else
		begin
			PaintSelection := False;
			x1 := 0; x2 := 0;
		end;

		if PaintSelection then
			BmCache.FillRectS(x1, 0, x2+1, h, Console.Palette[COLOR_SEL_BACK]);

		// Draw stippled center line
		DrawCenterLine;

		BmCache.MoveTo(0, GetSampleY(0));

		// Paint waveform peaks
		CP := Console.Palette[COLOR_WAVEFORM_PEAKS];
		if CP <> Console.Palette[COLOR_BACKGROUND] then
		for x := 1 to w do
		begin
			y := GetSamplePeakY(x);
			if y > HalfHeight then y := h - y;
			BmCache.VertLine(x, y, h - y, CP);
		end;

		// Paint waveform data
		C := Console.Palette[COLOR_WAVEFORM];
		if C <> Console.Palette[COLOR_BACKGROUND] then
		for x := 1 to w do
			BmCache.LineTo(x, GetSampleY(x), C);

		if PaintSelection then
		begin
			for y := 0 to h do
			for x := x1 to x2 do
				if (x >= 0) and (x < w) then
				begin
					PC := BmCache.PixelPtr[x,y];
					if PC^ = C then
						PC^ := Console.Palette[COLOR_SEL_FORE]
					else
					if PC^ = CP then
						PC^ := Console.Palette[COLOR_SEL_BACK];
				end;
		end;

		{if Sample.ByteLength > $1FFFF then
		for x := 1 to w-1 do
		begin
			pos := Trunc(p * x) + Display.L;
			y := Trunc(	((127 - ShortInt(Sample.Data[pos])) / 127) * hh);
			if pos > $1FFFF then
				BmCache.PenColor := Console.Palette[COLOR_OVERRUN];
			BmCache.LineToS(x, y);
		end;}
	end
	else
		DrawCenterLine;

	Paint;
end;

// --------------------------------------------------------------------------
// Actions
// --------------------------------------------------------------------------

procedure TSampleView.SetSample(const Sample: TSample);
begin
	InitCachedBitmap;

	//if (Sample <> FSample) then
	begin
		FSample := Sample;
		Selection.L := -1;
		Selection.R := -1;
		if FSample <> nil then
			SetViewport(0, FSample.ByteLength)
		else
			SetViewport(0, 0);
	end;
end;

procedure TSampleView.SetViewport(aL, aR: Integer);
var
	L: Integer;
begin
	Viewport.SetRange(aL, aR, FSample);

	if FSample <> nil then
	begin
		L := FSample.ByteLength;
		MaxScroll := L - Viewport.Length;
	end
	else
	begin
		L := 0;
		MaxScroll := 0;
	end;

	Offset := Viewport.L;

	if Assigned(Scrollbar) then
		Scrollbar.Adjust(L, Viewport.Length, True);

	DrawWaveform;
end;

procedure TSampleView.Zoom(ZoomIn: Boolean);
var
	aStep: Integer;
begin
	aStep := Max(Viewport.Length div 10, 1);
	if ZoomIn then
	begin
		if Viewport.Length >= 4 then
			SetViewport(Viewport.L + aSTEP, Viewport.R - aSTEP);
	end
	else
		SetViewport(Viewport.L - aSTEP, Viewport.R + aSTEP);
end;

// --------------------------------------------------------------------------
// Event Handlers
// --------------------------------------------------------------------------

function TSampleView.MouseDownEvent(Sender: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;

	procedure CaptureMouse;
	begin
		Screen.MouseInfo.Capturing := True;
		Screen.MouseInfo.Control := Self;
	end;

begin
	inherited;
	if (not WantMouse) or (ReadOnly) then Exit(False);

	Result := True;

	if not IsEmptySample(Sample) then
	case Button of

		mbLeft:
		begin
			CaptureMouse;

			case MouseAction of

				MOUSE_NONE, MOUSE_SELECT:
				begin
					MouseAction := MOUSE_SELECT;
					X := Max(PixelToSamplePos(X, 0), 0);
					Selection.SetRange(X, X, FSample);
					Selection.Origin := X;
					DrawWaveform;
				end;

			end;

		end;

		mbRight:
		begin
			CaptureMouse;
			MouseAction := MOUSE_DRAW;
			PrevMousePos := Point(-1, -1);
			MouseMoveEvent(Self, X+PixelRect.Left, Y+PixelRect.Top, P);
		end;

	end;

	if CurrentScreen = SampleScreen then
		SampleScreen.UpdateSampleInfo;
end;

function TSampleView.MouseMoveEvent(Sender: TCWEControl;
	X, Y: Integer;  P: TPoint): Boolean;
var
	X1, X2, Z: Integer;
begin
	inherited;
	Result := True;

	if Capturing then
	begin
		Dec(X, PixelRect.Left);
		Dec(Y, PixelRect.Top);

		X1 := PixelToSamplePos(X, 0);

		case MouseAction of

			MOUSE_SELECT:
			begin
				Selection.SetRange(Selection.Origin, X1);
				DrawWaveform;
			end;

			MOUSE_SETLOOP_L:
				if FSample.SetLoopStart((X1 + 1) div 2) then
				begin
					Paint;
					Module.SetModified;
				end;

			MOUSE_SETLOOP_R:
				if FSample.SetLoopEnd((X1 + 1) div 2) then
				begin
					Paint;
					Module.SetModified;
				end;

			MOUSE_DRAW:
				if (X >= 0) and (X < BmCache.Width) then
				begin
					Y := Max(Y, 0);
					Y := Min(Y, BmCache.Height-1);

					X1 := Max(X1, 0);

					if PrevMousePos.X >= 0 then
						X2 := PixelToSamplePos(PrevMousePos.X, 0)
					else
						X2 := X1;

					X2 := Max(X2, 0);
					Z  := Max(X1, X2);
					X1 := Min(X1, X2);
					X2 := Z;
					X2 := Min(X2, High(FSample.Data));

					PrevMousePos := Point(X, Y);
					Y := PixelToSampleValue(Y);

					for X := X1 to X2 do
						FSample.Data[X] := Byte(Y);
					FSample.ZeroFirstWord;

					DrawWaveform;
					Module.SetModified;
					Exit;
				end;

		end;

		if CurrentScreen = SampleScreen then
			SampleScreen.UpdateSampleInfo;
	end
	else
	begin
		P := Point(X, Y);
		X := MouseAction;

		if PtInRect(BoxL, P) then
			MouseAction := MOUSE_SETLOOP_L
		else
		if PtInRect(BoxR, P) then
			MouseAction := MOUSE_SETLOOP_R
		else
			MouseAction := MOUSE_SELECT;

		if MouseAction <> X then
			Paint;
	end;
end;

function TSampleView.MouseUpEvent(Sender: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Capturing then
	begin
		Screen.MouseInfo.Capturing := False;
//		Screen.MouseInfo.Control := nil;
		PrevMousePos := Point(-1, -1);
	end;
	MouseAction := MOUSE_NONE;
	Result := True;
end;

function TSampleView.MouseWheelEvent(Sender: TCWEControl;
	Shift: TShiftState; DirDown: Boolean; P: TPoint): Boolean;
begin
	if (not WantMouse) or (ReadOnly) then Exit(False);
	Result := True;
	Zoom(not DirDown);
end;

procedure TSampleView.ScrollBy(Amount: Integer);
var
	N: Integer;
begin
	N := Max(Offset + Trunc(Amount * (Viewport.Length / 20)), 0);

	if Amount > 0 then
	begin
		if Offset >= MaxScroll then Exit;
		if N > MaxScroll then N := MaxScroll;
	end;

	Offset := N;
	Viewport.SetRange(Offset, Offset + Viewport.Length, FSample);
	DrawWaveform;
	Scrollbar.Paint;
end;

function TSampleView.ScrollEvent(Sender: TCWEControl; NewOffset: Cardinal): Boolean;
begin
	Offset := NewOffset;
	Viewport.SetRange(Offset, Offset + Viewport.Length, FSample);
	DrawWaveform;
	Result := True;
end;

end.
