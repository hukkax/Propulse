unit FX.VectorLogo;

{$mode delphi}

interface

uses
	Classes, SysUtils, Generics.Collections,
	Graphics32, Screen.Splash;

type
	TVector = class
		X, Y, Z,
		sx, sy:		Integer;
		NewLine:	Boolean;
	end;

	TVectorList = TObjectList<TVector>;

	TVectorObject = class
	const
		LineColor  = $FFFFFFFF;
		ShadeColor = $FF555555;
	public
		X, Y, Z:	Integer;
		DegX, DegY, DegZ: Single;
		Limits:		TRect;
		Segments:	TVectorList;

		function	GetSegmentCoords(Index: Integer; var XX, YY: Integer): Boolean;
		procedure	Paint(const Buffer: TBitmap32; DestX, DestY: Integer);

		constructor	Create(const Text: AnsiString); overload;
		destructor	Destroy; override;
	end;

	TVectorItem = class
		Name:		AnsiString;
		Segments:	TVectorList;

		constructor Create(const AName: AnsiString); overload;
		destructor  Destroy; override;
	end;

	TStar = record
		Age, X, Y,
		DirX, DirY: Single;
		procedure Reset;
	end;

	TSplashEffectVector = class(TSplashEffect)
	const
		AMOUNT_STARS = 120;
	private
		g, ct256:	Extended;
		Vector,
		Vector1,
		Vector2:	TVectorObject;
		TextChangeCounter: Integer;
		Starfield:	array[0..AMOUNT_STARS] of TStar;

		function 	CreateVector(const Name: AnsiString; const Segments: array of Integer): TVectorItem;
		procedure 	SwapVectors;
	public
		procedure	Render(var Buffer: TBitmap32; DestX, DestY: Cardinal); override;

		constructor	Create(R: TRect; W, H: Cardinal);
		destructor	Destroy; override;
	end;


implementation

uses
	Math,
	ProTracker.Util;

const
	NEW = 65535;
	loi = PI/128;

var
	Letters: TObjectList<TVectorItem>;
	ScreenCenter: TPoint;
	TempVector: TVector;

// ============================================================================
// Utility
// ============================================================================

procedure RotatePoint(const point: TVector; var Result: TVector; rx, ry, rz: Single);
var
	temp1, temp2: Real;
	m: Single;
begin
	Result.X := point.X;
	Result.Y := point.Y;
	Result.Z := point.Z;
	{ X axis rotation, by 256-degree angle rx }
	m := rx*loi;
	temp1 := Result.Y * Cos(m) - Result.Z * Sin(m);
	temp2 := Result.Z * Cos(m) + Result.Y * Sin(m);
	Result.Y := Round(temp1);
	Result.Z := Round(temp2);
	{ Y axis rotation, by 256-degree angle ry }
	m := ry*loi;
	temp1 := Result.Z * Cos(m) - Result.X * Sin(m);
	temp2 := Result.X * Cos(m) + Result.Z * Sin(m);
	Result.Z := Round(temp1);
	Result.X := Round(temp2);
	{ Z axis rotation, by 256-degree angle rz }
	m := rz*loi;
	temp1 := Result.X * Cos(m) - Result.Y * Sin(m);
	temp2 := Result.Y * Cos(m) + Result.X * Sin(m);
	Result.X := Round(temp1);
	Result.Y := Round(temp2);
end;

procedure PointProject(var point: TVector; distance: Integer);
begin
	distance := distance - point.Z;
	if distance = 0 then distance := 1;
	point.sx := 256 * point.X div distance;
	point.sy := 256 * point.Y div distance;
end;

function FindLetter(C: AnsiChar): TVectorItem;
var
	Letter: TVectorItem;
begin
	for Letter in Letters do
		if Letter.Name = C then
			Exit(Letter);
	Result := nil;
end;

{ TVectorItem }

constructor TVectorItem.Create(const AName: AnsiString);
begin
	inherited Create;
	Name := AName;
	Segments := TVectorList.Create;
end;

destructor TVectorItem.Destroy;
begin
	Segments.Free;
	inherited;
end;

// ============================================================================
// TStar
// ============================================================================

procedure TStar.Reset;
begin
	X := ScreenCenter.X;
	Y := ScreenCenter.Y;
	Age := 0.0;
	repeat
		DirX := ((Random(500) - 250) / 100);
		DirY := ((Random(500) - 250) / 100);
	until (Abs(DirX) >= 0.25) and (Abs(DirY) >= 0.25);
end;

// ============================================================================
// TVectorObject
// ============================================================================

function TVectorObject.GetSegmentCoords(Index: Integer; var XX, YY: Integer): Boolean;
begin
	if (Index < 0) or (Index >= Segments.Count) then Exit(False);

	RotatePoint(Segments[Index], TempVector, DegX, DegY, DegZ);
	PointProject(TempVector, Z);

	XX := TempVector.sX + X;
	YY := TempVector.sY + Y;
	Result := not Segments[Index].NewLine;
end;

procedure TVectorObject.Paint(const Buffer: TBitmap32; DestX, DestY: Integer);
var
	i, x, y: Integer;
	B: Boolean;
begin
	Limits := Rect(ScreenCenter.X, ScreenCenter.Y, ScreenCenter.X, ScreenCenter.Y);

	for i := 0 to Segments.Count-1 do
	begin
		B := GetSegmentCoords(i, x{%H-}, y{%H-});
		Inc(x, DestX); Inc(y, DestY);
		if B then
		begin
			Buffer.LineToS(x, y, LineColor);
			Buffer.PixelS[x, y] := LineColor;
		end
		else
			Buffer.MoveTo(x, y);

		if x < Limits.Left then
			Limits.Left := x
		else
		if x > Limits.Right then
			Limits.Right := x;

		if y < Limits.Top then
			Limits.Top := y
		else
		if y > Limits.Bottom then
			Limits.Bottom := y;
	end;
end;

constructor TVectorObject.Create(const Text: AnsiString);
var
	i, p, x, maxx, maxy: Integer;
	Letter: TVectorItem;
	Seg, NewSeg: TVector;
const
	ScaleX = 12;
	ScaleY = 16;
	Spacing = 10;
begin
	inherited Create;

	Segments := TVectorList.Create;
	x := 0;

	for i := 1 to Length(Text) do
	begin
		Letter := FindLetter(Text[i]);
		if Letter = nil then Continue;

		maxx := 0; // letter width
		for p := 0 to Letter.Segments.Count-1 do
		begin
			Seg := Letter.Segments[p];
			if Seg.X > maxx then maxx := Seg.X;

			NewSeg := TVector.Create;

			NewSeg.X := Seg.X * ScaleX + x;
			NewSeg.Y := Seg.Y * ScaleY;
			NewSeg.Z := 0;
			NewSeg.sx := Seg.sx;
			NewSeg.sy := Seg.sy;
			NewSeg.NewLine := Seg.NewLine;

			Segments.Add(NewSeg);
		end;

		Inc(x, maxx * ScaleX + Spacing);
	end;

	maxx := 0; // object width
	maxy := 0; // object height
	for p := 0 to Segments.Count-1 do
	begin
		Seg := Segments[p];
		if Seg.X > maxx then maxx := Seg.X;
		if Seg.Y > maxy then maxy := Seg.Y;
	end;
	maxx := maxx div 2;
	maxy := maxy div 2;
	for p := 0 to Segments.Count-1 do
	begin
		Seg := Segments[p];
		Seg.X := Seg.X - maxx;
		Seg.Y := Seg.Y - maxy;
	end;
end;

destructor TVectorObject.Destroy;
begin
	Segments.Free;
	inherited Destroy;
end;

// ============================================================================
// TSplashEffectVector
// ============================================================================

constructor TSplashEffectVector.Create(R: TRect; W, H: Cardinal);
var
	i: Integer;
begin
	inherited Create(R, W, H);

	TempVector := TVector.Create;
	ScreenCenter := Point(R.Width div 2 + R.Left, R.Height div 2 + R.Top);

	Letters := TObjectList<TVectorItem>.Create(True);

	CreateVector('A', [04,20,44]);
//	CreateVector('A', [04,20,44,NEW,12,32]);
	CreateVector('B', [04,00,40,22,44,04,NEW,02,22]);
	CreateVector('C', [44,04,00,40]);
	CreateVector('D', [04,00,20,42,24,04]);
	CreateVector('E', [44,04,00,40,NEW,02,42]);
	CreateVector('F', [04,00,40,NEW,02,42]);
	CreateVector('G', [22,42,44,04,00,40]);
	CreateVector('H', [00,04,NEW,40,44,NEW,02,42]);
	CreateVector('I', [00,04]);
	CreateVector('J', [04,44,40]);
	CreateVector('K', [00,04,NEW,40,02,44]);
	CreateVector('L', [00,04,44]);
	CreateVector('M', [04,00,22,40,44]);
	CreateVector('N', [04,00,44,40]);
	CreateVector('O', [00,40,44,04,00]);
	CreateVector('P', [04,00,40,42,02]);
	CreateVector('Q', [00,40,44,04,00,NEW,22,44]);
	CreateVector('R', [04,00,40,42,02,44]);
	CreateVector('S', [04,44,42,02,00,40]);
	CreateVector('T', [00,40,NEW,20,24]);
	CreateVector('U', [00,04,44,40]);
	CreateVector('V', [00,24,40]);
	CreateVector('W', [00,04,22,44,40]);
	CreateVector('X', [00,44,NEW,04,40]);
	CreateVector('Y', [00,22,40,NEW,22,24]);
	CreateVector('Z', [00,40,04,44]);
	CreateVector('0', [00,40,44,04,00]);
	CreateVector('2', [00,04]);
	CreateVector('4', [00,40,42,02,04,44]);
	CreateVector('3', [00,40,44,04,NEW,42,02]);
	CreateVector('4', [44,40,02,42]);
	CreateVector('5', [04,44,42,02,00,40]);
	CreateVector('6', [40,00,04,44,42,02]);
	CreateVector('7', [00,40,24]);
	CreateVector('8', [00,40,44,04,00,NEW,02,42]);
	CreateVector('9', [04,44,40,00,02,42]);

	Vector1 := TVectorObject.Create('PROPULSE');
	Vector2 := TVectorObject.Create('TRACKER');

	Vector1.X := ScreenCenter.X; Vector1.Y := ScreenCenter.Y;
	Vector2.X := ScreenCenter.X; Vector2.Y := ScreenCenter.Y;

	ct256 := 0.0;
	g := 0.5;
	TextChangeCounter := 60 * 6;
	Vector := Vector1;

	Letters.Free;

	for i := 0 to AMOUNT_STARS do
		Starfield[i].Reset;
end;

destructor TSplashEffectVector.Destroy;
begin
	Vector1.Free;
	Vector2.Free;
	TempVector.Free;
	inherited Destroy;
end;

function TSplashEffectVector.CreateVector(const Name: AnsiString; const Segments: array of Integer): TVectorItem;
var
	i, c: Integer;
	Num: AnsiString;
	IsNew: Boolean;
	Seg: TVector;
begin
	Result := TVectorItem.Create(Name);

	IsNew := True;
	for i := 0 to Length(Segments)-1 do
	begin
		c := Segments[i];
		if c = NEW then
			IsNew := True
		else
		begin
			Num := Format('%.2d', [c]); // stupid :D
			Seg := TVector.Create;
			with Seg do
			begin
				X := StrToInt(Num[1]);
				Y := StrToInt(Num[2]);
				NewLine := IsNew;
				IsNew := False;
			end;
			Result.Segments.Add(Seg);
		end;
	end;

	Letters.Add(Result);
end;

procedure TSplashEffectVector.SwapVectors;
var
	OtherVector: TVectorObject;
begin
	if Vector = Vector1 then
	begin
		Vector := Vector2;
		OtherVector := Vector1;
	end
	else
	begin
		Vector := Vector1;
		OtherVector := Vector2;
	end;

//	Vector.LineColor := Random($FFFFFFFF);
//	OtherVector.LineColor := Vector.LineColor;

	Vector.DegX := OtherVector.DegX;
	Vector.DegY := OtherVector.DegY;
	Vector.DegZ := OtherVector.DegZ;
	Vector.Z := OtherVector.Z;
end;

procedure TSplashEffectVector.Render(var Buffer: TBitmap32; DestX, DestY: Cardinal);
var
	i, x, y: Integer;
	C: TColor32;
	P: PColor32;
begin
	Buffer.ClipRect := Rect;
	Buffer.FillRectS(Buffer.ClipRect, $FF000000);

	for i := 0 to AMOUNT_STARS do
	with Starfield[i] do
	begin
		X := X + DirX;
		Y := Y + DirY;
		if (X < Rect.Left) or (Y < Rect.Top) or (X > Rect.Right) or (Y > Rect.Bottom) then
			Reset;
		Age := Age + ((Abs(DirX) + Abs(DirY)));
		Buffer.SetPixelS(Trunc(X), Trunc(Y), Gray32(Min(Trunc(Age), 255)));
	end;

	// rotate and draw wireframe vector
	//
	g := g + 0.03;
	ct256 := ct256 + 0.7;
	if ct256 >= 255.0 then ct256 := ct256 - 255.0;

	with Vector do
	begin
		Z := Round(Cos(g)*150) + 600;
		DegX := ct256;// mod 128 + 128;
		DegY := ct256 * 2;
		DegZ := Cos(g) * 24;

		Paint(Buffer, 0, 0);
	end;

	if (TextChangeCounter = 0) and (InRange(Trunc(Vector.DegY), 189, 195)) then
	begin
		SwapVectors;
		TextChangeCounter := 60 * 2;
	end
	else
	if TextChangeCounter > 0 then
		Dec(TextChangeCounter);

	C := Vector.LineColor;

	for y := Max(Vector.Limits.Top, 1) to Min(Vector.Limits.Bottom, Buffer.Height-2) do
	for x := Max(Vector.Limits.Left, 0) to Min(Vector.Limits.Right, Buffer.Width-1) do
	begin
		P := Buffer.PixelPtr[x,y];
		if P^ = C then Continue;
		if Buffer.Pixel[x, y+1] = C then P^ := Vector.ShadeColor
		else
		if Buffer.Pixel[x, y-1] = C then P^ := Vector.ShadeColor;
	end;

	Buffer.ResetClipRect;
end;


end.

