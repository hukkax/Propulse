unit Screen.Splash;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text,
	GR32, GR32_Transforms, GR32_Rasterizers;
//	ProTracker.Player;

type
	TXYZ = record
		X, Y, Z: Integer;
	end;

	TFloatXYZ = record
		X, Y, Z: Single;
	end;

	TStar = record
		P, R, Z, S: Single;
		Color: TColor32;
	end;

	TSplashScreen = class(TCWEScreen)
	const
		NUMSTARS = 350;
	private
		Mid: TPoint;
		Scale: Single;
		Angle, Pivot, Camera: TFloatXYZ;
		Vec: array [1..4] of TXYZ;
		DR: TRect;
		ZoomingIn: Single;
		Logo, Pb, Scroll: TBitmap32;
		Trans: TProjectiveTransformation;
		Rasterizer: TRasterizer;
		ScrollPix, ScrollChar: Cardinal;
		ScrollColor: Byte;
		Gradient: array of TColor32;
		LogoHeight: Word;
		Stars: array [0..NUMSTARS] of TStar;
		ScrollText: AnsiString;
		//OriginalModule: TPTModule;

		Counter: record
			X, Y, Z, S: Single;
		end;

		procedure	InitStar(i: Integer);
		procedure	MakeGradient(frequency1, frequency2, frequency3,
					phase1, phase2, phase3: Single);
	public
		//Tune:	TPTModule;
		Box:	TCWEControl;

		procedure	Update(Draw: Boolean = True);
		procedure	Show; override;
		procedure 	Leave; override;
		procedure 	Paint; override;

		procedure 	OKClick(Sender: TCWEControl);

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
		destructor	Destroy; override;
		procedure 	Init;
	end;

var
	SplashScreen: TSplashScreen;


implementation

uses
	FormMain,
	SysUtils,
	Math,
	BuildInfo,
	ProTracker.Util,
	Screen.Editor;


const
	SCRSEP = '%R'#3'%%';

	Scroll_Text: AnsiString = '                                ' +
	'%Ghukka%% presents %Propulse ' + ProTracker.Util.VERSION + '%% § ' +
	'%WPT2PLAY%% playroutine and coding help by %G8bitbubsy%% § ' +
	'Thanks to %Gmuzzy%% § %Gwuffe%% § %GTempest%% § ' +
	'For full credits press F1 now § ' +
	'Greets to %WVoid § %Wexec § %Wsvenonacid § %Y#kukkakoodi § %Y#protracker § '+
	'%GHaikz § %GArchyx § %GHofnarr § %GKaarlo § %GCrank § %Gamaneog § ' +
	'%%' + '  ';


procedure SetXYZ(var XYZ: TXYZ; X, Y, Z: Integer); overload;
begin
	XYZ.X := X;
	XYZ.Y := Y;
	XYZ.Z := Z;
end;

procedure SetXYZ(var XYZ: TFloatXYZ; X, Y, Z: Single); overload;
begin
	XYZ.X := X;
	XYZ.Y := Y;
	XYZ.Z := Z;
end;

procedure TSplashScreen.InitStar(i: Integer);
begin
	with Stars[i] do
	begin
		P := Pi * 2 * Random();	// Circular position
		R := Random(100) + 1;	// Radius
		Z := 300;				// Z
		S := Random(6) + 2;		// Speed
		Color := Gray32(Random(200) + 55);
	end;
end;

procedure TSplashScreen.Show;
begin
	ScrollText := StringReplace(Scroll_Text, '§', SCRSEP, [rfReplaceAll]);
{
	if (Assigned(Module)) and (Module.PlayMode <> PLAY_STOPPED) then
	begin
		//showmessage('not changing');
		Exit; // a song is playing, don't change it
	end;

	OriginalModule := Module;

	if not Assigned(Tune) then
	begin
		Tune := TPTModule.Create(False);
		if not Tune.LoadFromFile(AppPath + '\data\1.mod') then
			Exit;
	end
	else
		Tune.Reset;

	Module := Tune;
	Tune.Play;
}
end;

procedure TSplashScreen.OKClick(Sender: TCWEControl);
begin
	Leave;
	if Sender <> nil then
		Window.ChangeScreen(TCWEScreen(Editor));
end;

procedure TSplashScreen.Leave;
begin
	{if Assigned(Tune) then
	begin
		Tune.Stop;
		Tune.Close;
		FreeAndNil(Tune);
		Module := OriginalModule;
	end;
	OriginalModule := nil;}
	ScrollText := '';
end;

// Called at Create and subsequently if font size changes
procedure TSplashScreen.Init;
var
	Fn: String;
begin
	Box.GetPixelRect(DR);
	Mid.X := RectWidth(DR)  div 2;
	Mid.Y := RectHeight(DR) div 2;

	Fn := DataPath + 'logo.png';
	if FileExists(Fn) then
	begin
		Scroll.LoadFromFile(Fn);
		LogoHeight := Scroll.Height;
		Logo.SetSize(Scroll.Width, Scroll.Height + 1 + Console.Font.Height);
		Logo.Draw(0, 0, Scroll);
	end
	else
	begin
		// logo image missing, just use normal font
		Logo.SetSize(11 * Console.Font.Width, Console.Font.Height * 2 + 1);
		LogoHeight := Console.Font.Height;
		Console.BlitText(Logo, 0, 0, 'Propulse', clWhite32, clBlack32);
	end;

	SetLength(Gradient, LogoHeight);

	Pb.SetSize(RectWidth(DR), RectHeight(DR));

	Scroll.SetSize(Logo.Width + Console.Font.Width, Console.Font.Height);
	Scroll.Clear(clBlack32);

	Trans.SrcRect := FloatRect(Logo.BoundsRect);

	ScrollPix  := 0;
	ScrollChar := 0;
end;

constructor TSplashScreen.Create;
const
	I = 50;
	X = 60 * 3;
	Y = 60;
var
	Button: TCWEButton;
	n: Integer;
const
	URL = 'http://hukka.yiff.fi/porotracker/';
begin
	inherited;

	//Tune := nil;

	Counter.X := 0;
	Counter.Y := 0;
	Counter.Z := 0;
	Counter.S := 0;

	Box := TCWEControl.Create(Self, '', 'Logo',
		Types.Rect(3, 4, Console.Width-3, Console.Height-12), True);
	Box.SetBorder(True, True, True, False);
	Box.ColorBack := 0;

	Button := TCWEButton.Create(Self, 'Continue', 'bOK',
		Types.Rect(32, Console.Height-3, Console.Width-32, Console.Height-2));

	TCWEURLLabel.Create(Self, URL, 'website',
		Console.CenterAt(Length(URL), Console.Height-6));

	ActiveControl := Button;
	Button.OnChange := OKClick;

	ZoomingIn := 0.01;

	SetXYZ(Angle, 0, 0, 0);		// Angle of rotation about the following pivots:
	SetXYZ(Pivot, 0, 0, 0);		// Pivots x, y and z.
	SetXYZ(Camera, 0, 0, 300);	// Position of the camera.

	SetXYZ(Vec[1], -X, -Y,  I);
	SetXYZ(Vec[2],  X, -Y,  I);
	SetXYZ(Vec[3],  X,  Y,  I);
	SetXYZ(Vec[4], -X,  Y,  I);

	Logo := TBitmap32.Create;
	Scroll := TBitmap32.Create;
	Pb := TBitmap32.Create;

	Trans := TProjectiveTransformation.Create;
	Rasterizer := TRegularRasterizer.Create;

	for n := 0 to NUMSTARS do
		InitStar(n);

	Init;
end;

destructor TSplashScreen.Destroy;
begin
	Rasterizer.Free;
	Logo.Free;
	Scroll.Free;
	Pb.Free;
	Trans.Free;

	inherited Destroy;
end;

procedure TSplashScreen.Paint;
var
	Y: Integer;
const
	X = 6;
begin
	Console.WriteHeader('Propulse v' + VERSION + ' '+#7+' '+ Build.CompileDate, 1);

	Y := Console.Height - 10;

	Console.WriteCentered(' 2016-2017 hukka (Joel Toivonen)', Y+0);
	Console.WriteCentered(' Original playroutine by 8bitbubsy (Olav Sorensen)', Y+2);
//	Console.WriteCentered(' http://hukka.yiff.fi/porotracker/', Y+4);

	inherited;
end;

procedure TSplashScreen.MakeGradient(frequency1, frequency2, frequency3,
	phase1, phase2, phase3: Single);
var
	i: Integer;
const
	center = 128;
	width  = 127;
begin
	for i := 0 to LogoHeight-1 do
	with TColor32Entry(Gradient[i]) do
	begin
		R := Round(Sin(frequency1 * i + phase1) * width + center);
		G := Round(Sin(frequency2 * i + phase2) * width + center);
		B := Round(Sin(frequency3 * i + phase3) * width + center);
	end;
end;

procedure TSplashScreen.Update(Draw: Boolean = True);
var
	n, cl: Integer;
	XD, YD, ZD, ZX, ZY, YX, YZ, XY, XZ: Single;
	ROTOFFSET, P: TFloatXYZ;
	Pnt: array[1..4] of TFloatPoint;
	X0OFFSET, Y0OFFSET: Single;
	XOFFSET, YOFFSET: Integer;
	sX, sY: Integer;
	C: AnsiChar;
	SCX, SCY: Single;
const
	Turn = 0;
begin
	SCX := Sin(Counter.X);
	SCY := Sin(Counter.Y);

	Angle.X := SCX * 0.74;
	Angle.Y := SCY * 0.58;
	Angle.Z := Cos(Counter.Z) * 0.25;

	Counter.X := Counter.X + 0.005;
	Counter.Y := Counter.Y + 0.018;
	Counter.Z := Counter.Z + 0.024;
	Counter.S := Counter.S + 0.012;

	if ZoomingIn < 1.0 then
		ZoomingIn := ZoomingIn * 1.02;

	Scale := (265 + (Sin(Counter.S) * 100)) * ZoomingIn;

	XOFFSET := -Round(Sin(Counter.Y / 2) * 130);
	YOFFSET := -Round(Cos(Counter.X) * 110);
	X0OFFSET := SCX * 10;
	Y0OFFSET := Cos(Counter.Z) * 10;

	Inc(ScrollPix);
	if ScrollPix >= Console.Font.Width then
	begin
		Scroll.Draw(
			Types.Rect(0, 0, Logo.Width, Scroll.Height),
			Types.Rect(Console.Font.Width, 0, Scroll.Width, Scroll.Height),
			Scroll);

		Inc(ScrollChar);
		if ScrollChar > Length(ScrollText) then
			ScrollChar := 1;

		if ScrollText[ScrollChar] = '%' then
		begin
			C := ScrollText[ScrollChar+1];

			if C = '%' then
				ScrollColor := 12
			else
			if C = 'W' then
				ScrollColor := 11
			else
			if C = 'R' then
				ScrollColor := 4
			else
			if C = 'Y' then
				ScrollColor := 5
			else
			if C = 'G' then
				ScrollColor := 6;

			Inc(ScrollChar, 2);
		end;

		Console.BlitChar(Scroll, Logo.Width, 0,
			Ord(ScrollText[ScrollChar]),
			Console.Palette[ScrollColor], clBlack32);

		ScrollPix := 0;
	end;

	Trans.BeginUpdate;

	for n := 1 to 4 do
	begin
		XD := Vec[n].X - Pivot.X;
		YD := Vec[n].Y - Pivot.Y;
		ZD := Vec[n].Z - Pivot.Z;

		ZX := XD * Cos(Angle.Z) - YD * Sin(Angle.Z) - XD;
		ZY := XD * Sin(Angle.Z) + YD * Cos(Angle.Z) - YD;

		YX := (XD + ZX) * Cos(Angle.Y) - ZD * Sin(Angle.Y) - (XD + ZX);
		YZ := (XD + ZX) * Sin(Angle.Y) + ZD * Cos(Angle.Y) - ZD;

		XY := (YD + ZY) * Cos(Angle.X) - (ZD + YZ) * Sin(Angle.X) - (YD + ZY);
		XZ := (YD + ZY) * Sin(Angle.X) + (ZD + YZ) * Cos(Angle.X) - (ZD + YZ);

		SetXYZ(ROTOFFSET, YX + ZX, ZY + XY, XZ + YZ);

		P.Z := ( Vec[n].Z + ROTOFFSET.Z + Camera.Z );
		P.X := ( Vec[n].X + ROTOFFSET.X + Camera.X ) / P.Z * SCALE;
		P.Y := ( Vec[n].Y + ROTOFFSET.Y + Camera.Y ) / P.Z * SCALE;

		P.X := P.X + (RectWidth(DR)  div 2);
		P.Y := P.Y + (RectHeight(DR) div 2);

		Pnt[n].X := P.X;
		Pnt[n].Y := P.Y;
	end;

	Trans.X0 := Pnt[1].X;	Trans.Y0 := Pnt[1].Y;
	Trans.X1 := Pnt[2].X;	Trans.Y1 := Pnt[2].Y;
	Trans.X2 := Pnt[3].X;	Trans.Y2 := Pnt[3].Y;
	Trans.X3 := Pnt[4].X;	Trans.Y3 := Pnt[4].Y;

	if not Draw then
	begin
		Trans.EndUpdate;
		Exit;
	end;

	Logo.Draw(
		Types.Bounds(0, LogoHeight + 1, Logo.Width, Scroll.Height),
		Types.Bounds(ScrollPix, 0, Logo.Width, Scroll.Height),
		Scroll);

	Pb.BeginUpdate;
	Pb.Clear(clBlack32);
	Transform(Pb, Logo, Trans, Rasterizer);
	Pb.EndUpdate;

	Console.Bitmap.Draw(DR.Left, DR.Top, Pb);

	MakeGradient(
		SCX * 0.15,
		Cos(Counter.Y) * 0.11,
		Cos(Counter.X) * 0.13,
		Cos(Counter.X) * 2.3,
		SCY * 2.7,
		Cos(Counter.Z) * 1.9
		);

	for sY := 0 to LogoHeight-1 do
	begin
		n := Gradient[sY];
		for sX := 0 to Logo.Width-1 do
			if Logo.Pixel[sX, sY] <> clBlack32 then
				Logo.Pixel[sX, sY] := n;
	end;


	for n := 0 to NUMSTARS do
	with Stars[n] do
	begin
		XD := Sin(P {+ Turn}) * R + X0OFFSET;
		YD := Cos(P {+ Turn}) * R + Y0OFFSET;
		sX := Round(XD / Z * Pb.Width)  + Mid.X + XOFFSET;
		sY := Round(YD / Z * Pb.Height) + Mid.Y + YOFFSET;
		Z := Z - S;

		if (Z <= 0) or
			(sX < DR.Left) or (sY < DR.Top) or
			(sX >= DR.Right) or (sY >= DR.Bottom) then
				InitStar(n)
		else
		//if TColor32Entry(Console.Bitmap.Pixel[sX, sY]).G = 0 then
		if (Console.Bitmap.Pixel[sX, sY] and $FF0000) = 0 then
		begin
			cl := 275 - Round(Z);
			cl := Min(Max(cl, 1), 255);
			Console.Bitmap.Pixel[sX, sY] := Gray32(cl);
		end;
	end;

	Trans.EndUpdate;
end;


end.

