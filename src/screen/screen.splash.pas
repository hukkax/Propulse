unit Screen.Splash;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text,
	Graphics32, PCX;

type
	TSplashEffect = class
	const
		AnimLength = 50 * 9;
		FADESPEED  = 0.025;

		LogoWidth  = 72;
		LogoHeight = 29;
	private
		Fading:				ShortInt;
		Brightness:			Single;
		GradientIndex,
		LogoType,
		LogoTypeCount,
		LogoFrameCount:		Byte;
		LogoFrame,
		LogoAnimDir:		ShortInt;
		DrawBackground,
		BrightnessChars: 	Boolean;
		Counter:			Cardinal;
		Gradient,
		Logo:				TPCXImage;
		PixelChar:			array [0..15] of Byte;
		PlasmaCtrX1,
		PlasmaCtrY1,
		PlasmaCtrX2,
		PlasmaCtrY2:		Single;

		procedure 	NextAnimation;
	public
		Rect: 		TRect;

		constructor	Create(W, H: Cardinal);
		destructor	Destroy; override;
		procedure	Render(var Buffer: TBitmap32; DestX, DestY: Cardinal);
	end;

	TSplashScreen = class(TCWEScreen)
	private
		TRANSCOLOR:	TColor32;
		Mid: 		TPoint;
		DR: 		TRect;
		Scroll: 	TBitmap32;
		ScrollPix,
		ScrollChar: Cardinal;
		ScrollColor:Byte;
		ScrollText: AnsiString;

		{function 	BoxMouseDown(Ctrl: TCWEControl;
					Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;}
	public
		Box:		TCWEControl;
		Effect: 	TSplashEffect;

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
	SysUtils,
	Math,
	BuildInfo,
	ProTracker.Util,
	Screen.Editor;


const
	SCRSEP = '%R'#3'%%';

	Scroll_Text: AnsiString = '                  ' +
	'%Ghukka%% presents %WPropulse ' + ProTracker.Util.VERSION + '%% § ' +
	'%WPT2PLAY%% playroutine and coding help by %G8bitbubsy%% § ' +
	'Thanks to %Gmuzzy%% § %Gwuffe%% § %GTempest%% § ' +
	'For full credits press F1 now § ' +
	'Greets to %WVoid § %Wexec § %Wsvenonacid § %Y#kukkakoodi § %Y#protracker § '+
	'%GHaikz § %GArchyx § %GHofnarr § %GKaarlo § %GCrank § %Gamaneog § ' +
	'%%' + '                   ';


// ============================================================================
// TSplashEffect
// ============================================================================

procedure InterpolatePalette(i1, i2: Byte; Col1, Col2: TColor32);
var
	x: Integer;
	R, G, B,
	dR, dG, dB: Single;
begin
	x := i2 - i1;

	R := RedComponent(Col1);
	G := GreenComponent(Col1);
	B := BlueComponent(Col1);

	dR := (RedComponent(Col2)   - R) / x;
	dG := (GreenComponent(Col2) - G) / x;
	dB := (BlueComponent(Col2)  - B) / x;

	for x := i1 to i2 do
	begin
		Console.Palette[x+16] :=
			Color32(Trunc(R), Trunc(G), Trunc(B));
		R := R + dR;
		G := G + dG;
		B := B + dB;
	end;
end;

constructor TSplashEffect.Create(W, H: Cardinal);
begin
	Gradient := TPCXImage.Create;
	Gradient.LoadFromFile(DataPath + 'gradient.pcx');

	Logo := TPCXImage.Create;
	Logo.LoadFromFile(DataPath + 'logo.pcx');

	Rect := Bounds(
		(W div 2) - (LogoWidth  div 2),
		(H div 2) - (LogoHeight div 2),
		Logo.Width, LogoHeight);

	LogoFrameCount := Logo.Height div LogoHeight; // lol
	LogoTypeCount  := Logo.Width  div LogoWidth;

	InterpolatePalette(0, 127, $5088FF, $222266);

	PixelChar[0]  := 000;
	PixelChar[1]  := 155;
	PixelChar[2]  := 156;
	PixelChar[3]  := 157;
	PixelChar[4]  := 158;
	PixelChar[5]  := 159;
	PixelChar[6]  := 160;
	PixelChar[7]  := 161;
	PixelChar[8]  := 162;
	PixelChar[9]  := 163;
	PixelChar[10] := 164;
	PixelChar[11] := 165;
	PixelChar[12] := 166;
	PixelChar[13] := 167;
	PixelChar[14] := 168;
	PixelChar[15] := 168;

	Counter := 0;
	Brightness := 0;
	Fading := 1;
	LogoType := 0;
	LogoAnimDir := Fading;
	BrightnessChars := False;
	DrawBackground  := True;
	LogoFrame := -33;
end;

destructor TSplashEffect.Destroy;
begin
	Logo.Free;
	Gradient.Free;
	inherited Destroy;
end;

procedure TSplashEffect.NextAnimation;
var
	C: TColor32;
	x, i: Integer;
begin
	BrightnessChars := (Random(100) <= 40);
	DrawBackground  := (Random(100) <= 60);

	Inc(LogoType);
	if LogoType >= LogoTypeCount then
		LogoType := 0;
	LogoFrame := -26;
	LogoAnimDir := 1;

	case Random(14) of
		0..1:
		begin
			BrightnessChars := False;
			InterpolatePalette(000, 045, $8ef367, $ee2818);
			InterpolatePalette(045, 076, $ee2818, $3838d3);
			InterpolatePalette(076, 100, $3838d3, $0bcfee);
			InterpolatePalette(100, 127, $0bcfee, $8ef367);
		end;
		2..8:
		begin
			Console.Palette[16] := Random($FFFFFF);
			x := 0;
			repeat
				i := x;
				x := x + 20 + Random(80);
				if x > 127 then x := 127;
				InterpolatePalette(i, x, Console.Palette[i+16], Random($FFFFFF));
			until x >= 127;
		end;
		9..10:
		begin
			InterpolatePalette(0, 127,
				Color32(Random(70), Random(70), Random(70)),
				Color32(120+Random(130), 120+Random(130), 120+Random(130)))
		end;
		else
			C := Random($FFFFFF);
			i := 64 + (Random(40)-20);
			InterpolatePalette(0,  i,  Random($FFFFFF), C);
			InterpolatePalette(i, 127, C, Random($FFFFFF));
	end;

	if Random(4) = 2 then
	begin
		i := 100 - Random(30);
		InterpolatePalette(i, 127,
			Console.Palette[i+16],
			Color32(Random(70), Random(70), Random(70)));
	end;

	PlasmaCtrX1 := Random(180);
	PlasmaCtrY1 := Random(180);
	PlasmaCtrX2 := Random(180);
	PlasmaCtrY2 := Random(180);
end;

function PlasmaFunc1(X, Y: Integer): Integer; inline;
begin
	Result := Trunc(64 + 63 * (Sin(Hypot(27-Y, 72-X)/16)));
end;

function PlasmaFunc2(X, Y: Integer): Integer; inline;
begin
	Result := Trunc(64 + 63 * Sin(X/(37+15*Cos(Y/34))) * Cos(Y/(21+11*Sin(X/27))));
end;

procedure TSplashEffect.Render(var Buffer: TBitmap32; DestX, DestY: Cardinal);
var
	X, Y, lx, ly, i, c,
	PX1, PY1, PX2, PY2: Integer;
begin
	Inc(GradientIndex);
	if GradientIndex >= Gradient.Height then
		GradientIndex := 0;

	lx := LogoType * LogoWidth;
	ly := CLAMP(LogoFrame, 0, LogoFrameCount-1) * LogoHeight;

	Inc(LogoFrame, LogoAnimDir);
	if LogoFrame >= LogoFrameCount then
		LogoFrame := LogoFrameCount-1;

	PlasmaCtrX1 := PlasmaCtrX1 + 0.3;
	PlasmaCtrY1 := PlasmaCtrY1 + 0.25;
	PlasmaCtrX2 := PlasmaCtrX2 + 0.8;
	PlasmaCtrY2 := PlasmaCtrY2 + 0.5;

	PX1 := Trunc(Sin(PlasmaCtrX1 / 100) * 140);
	PY1 := Trunc(Cos(PlasmaCtrY1 / 80)  * 116);
	PX2 := Trunc(Sin(PlasmaCtrX2 / 84)  * 180);
	PY2 := Trunc(Cos(PlasmaCtrY2 / 71)  * 223);

	c := PixelChar[Trunc(15 * Brightness)];

	if BrightnessChars then
	begin
		for Y := 0 to LogoHeight-1 do
		for X := 0 to LogoWidth-1 do
		if Logo.Pixels[X+lx, Y+ly] = 1 then
		begin
			i := PlasmaFunc1(X + PX1, Y + PY1) + PlasmaFunc2(X + PX2, Y + PY2);
			Console.PutChar(DestX + Rect.Left + X, DestY + Rect.Top + Y,
				PixelChar[Gradient.Pixels[Trunc(i * Brightness), GradientIndex]], i div 2 + 16);
		end
		else
			Console.PutChar(DestX + Rect.Left + X, DestY + Rect.Top + Y,
				PixelChar[0], 15);
	end
	else
	begin
		for Y := 0 to LogoHeight-1 do
		for X := 0 to LogoWidth-1 do
		begin
			i := PlasmaFunc1(X + PX1, Y + PY1) + PlasmaFunc2(X + PX2, Y + PY2);
			i := i div 2 + 16;
			if Logo.Pixels[X+lx, Y+ly] = 0 then
			begin
				if DrawBackground then
					i := 128 - (i div 2)
				else
				begin
					Console.PutChar(DestX+Rect.Left+X, DestY+Rect.Top+Y, PixelChar[0], 15);
					Continue;
				end;
			end;
			Console.PutChar(DestX + Rect.Left + X, DestY + Rect.Top + Y, c, i);
		end;
	end;

	Inc(Counter);

	if Fading <> 0 then
	begin
		if Fading > 0 then
		begin
			Brightness := Brightness + FADESPEED;
			if Brightness >= 1.0 then
			begin
				Brightness := 1.0;
				Fading := 0;
			end;
		end
		else
		if Fading < 0 then
		begin
			Brightness := Brightness - FADESPEED;
			if Brightness <= 0.0 then
			begin
				Brightness := 0.0;
				Fading := 1;
				NextAnimation;
			end;
		end
	end
	else
	if Counter > AnimLength then
	begin
		Fading := -1; // fade out
		Counter := 0;
		LogoAnimDir := -1;
		LogoFrame := LogoFrameCount + 45;
	end;
end;

// ============================================================================
// TSplashScreen
// ============================================================================

procedure TSplashScreen.Show;
begin
	Randomize;
	ScrollText := StringReplace(Scroll_Text, '§', SCRSEP, [rfReplaceAll]);
end;

procedure TSplashScreen.OKClick(Sender: TCWEControl);
begin
	Leave;
	if Sender <> nil then
		ChangeScreen(TCWEScreen(Editor));
end;

procedure TSplashScreen.Leave;
begin
	ScrollText := '';
end;

// Called at Create and subsequently if font size changes
procedure TSplashScreen.Init;
var
	W, H: Integer;
begin
	Box.GetPixelRect(DR);
	W := RectWidth(DR);
	H := RectHeight(DR);
	Mid.X := W div 2;
	Mid.Y := H div 2;

	Scroll.SetSize(W + Console.Font.Width, Console.Font.Height);

	ScrollPix  := 0;
	ScrollChar := 0;

	Effect := TSplashEffect.Create(RectWidth(Box.Rect), RectHeight(Box.Rect));
end;

constructor TSplashScreen.Create;
var
	Button: TCWEButton;
const
	URL = 'http://hukka.yiff.fi/porotracker/';
begin
	inherited;

	Box := TCWEControl.Create(Self, '', 'Logo',
		Types.Rect(4, 4-1, Console.Width-4, Console.Height-12-1), True);

	Box.SetBorder(True, True, True, False);
	Box.ColorBack := 15;
//	Box.OnMouseDown := BoxMouseDown;
//	Box.WantMouse := True;

	Button := TCWEButton.Create(Self, 'Continue', 'bOK',
		Types.Rect(32, Console.Height-2, Console.Width-32, Console.Height-1));

	TCWEURLLabel.Create(Self, URL, 'website',
		Console.CenterAt(Length(URL), Console.Height-5));

	ActiveControl := Button;
	Button.OnChange := OKClick;

	Scroll := TBitmap32.Create;

	Effect := nil;
	Init;
end;

destructor TSplashScreen.Destroy;
begin
	Effect.Free;
	Scroll.Free;
	inherited Destroy;
end;

procedure TSplashScreen.Paint;
var
	Y: Integer;
begin
	if TRANSCOLOR <> Console.Palette[TConsole.COLOR_PANEL] then
	begin
		TRANSCOLOR := Console.Palette[TConsole.COLOR_PANEL];
		Scroll.Clear(TRANSCOLOR);
	end;

	Console.WriteHeader('Propulse v' + VERSION + ' '+#7+' '+ Build.CompileDate, 1);

	Y := Console.Height - 9;

	Console.WriteCentered(' 2016-2017 hukka (Joel Toivonen)', Y+0);
	Console.WriteCentered(' Original playroutine by 8bitbubsy (Olav Sorensen)', Y+2);

	inherited;
end;

procedure TSplashScreen.Update(Draw: Boolean = True);
var
	C: AnsiChar;
begin
	Effect.Render(Console.Bitmap, Box.Rect.Left, Box.Rect.Top);

	Inc(ScrollPix);
	if ScrollPix >= Console.Font.Width then
	begin
		// scroll buffer left by one char
		Scroll.Draw(0, 0,
			Types.Rect(Console.Font.Width, 0, Scroll.Width, Scroll.Height),
			Scroll);

		Inc(ScrollChar);
		if ScrollChar > Length(ScrollText) then
			ScrollChar := 1;

		if ScrollText[ScrollChar] = '%' then
		begin
			C := ScrollText[ScrollChar+1];

			if C = '%' then
				ScrollColor := 8 //12
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
				ScrollColor := 0; //6;

			Inc(ScrollChar, 2);
		end;

		Console.BlitChar(Scroll, Scroll.Width-Console.Font.Width-1, 0,
			Ord(ScrollText[ScrollChar]),
			Console.Palette[ScrollColor], TRANSCOLOR);

		ScrollPix := 0;
	end;

	Console.Bitmap.Draw(DR.Left, DR.Bottom + 5,
		Types.Bounds(ScrollPix, 0, RectWidth(DR)-1, Scroll.Height-1),
		Scroll);
end;

{function TSplashScreen.BoxMouseDown(Ctrl: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := True;
end;}

end.

