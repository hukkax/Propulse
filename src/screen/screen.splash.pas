unit Screen.Splash;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text,
	Graphics32, PCX;

type
	TConsolePixel = record
		Char,
		Color:		Byte;
	end;

	TSplashEffect = class
	const
		AnimLength = 50 * 5;
		FADESPEED  = 0.025;
	private
		Fading:				ShortInt;
		Brightness:			Single;
		BrightnessChars: 	Boolean;
		Counter:			Cardinal;
		Logo:				TPCXImage;
		Pix:				array [0..15] of TConsolePixel;
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
		Effect: 	TSplashEffect;

		{function 	BoxMouseDown(Ctrl: TCWEControl;
					Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;}
	public
		Box:		TCWEControl;

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

	Scroll_Text: AnsiString = '                                ' +
	'%Ghukka%% presents %WPropulse ' + ProTracker.Util.VERSION + '%% § ' +
	'%WPT2PLAY%% playroutine and coding help by %G8bitbubsy%% § ' +
	'Thanks to %Gmuzzy%% § %Gwuffe%% § %GTempest%% § ' +
	'For full credits press F1 now § ' +
	'Greets to %WVoid § %Wexec § %Wsvenonacid § %Y#kukkakoodi § %Y#protracker § '+
	'%GHaikz § %GArchyx § %GHofnarr § %GKaarlo § %GCrank § %Gamaneog § ' +
	'%%' + '  ';


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

	procedure SetPix(i, cc, ci: Byte);
	begin
		Pix[i].Char  := cc;
		Pix[i].Color := ci;
	end;

begin
	Logo := TPCXImage.Create;
	Logo.LoadFromFile(DataPath + 'logo.pcx');

	Rect := Bounds(
		(W div 2) - (Logo.Width  div 2),
		(H div 2) - (Logo.Height div 2),
		Logo.Width, Logo.Height);

	InterpolatePalette(0, 127, $002266, $4488FF);

	SetPix(0,  000, 16);
	SetPix(1,  155, 17);
	SetPix(2,  156, 18);
	SetPix(3,  157, 19);
	SetPix(4,  158, 20);
	SetPix(5,  159, 21);
	SetPix(6,  160, 22);
	SetPix(7,  161, 23);
	SetPix(8,  162, 24);
	SetPix(9,  163, 25);
	SetPix(10, 164, 26);
	SetPix(11, 165, 27);
	SetPix(12, 166, 28);
	SetPix(13, 167, 29);
	SetPix(14, 168, 30);
	SetPix(15, 168, 31);

	Counter := 0;
	Brightness := 0;
	Fading := 1;
end;

destructor TSplashEffect.Destroy;
begin
	Logo.Free;
	inherited Destroy;
end;

procedure TSplashEffect.NextAnimation;
var
	C, C2: TColor32;
begin
	BrightnessChars := (Random(4) = 1);

	case Random(5) of
	1:
		begin
			BrightnessChars := False;
			InterpolatePalette(000, 045, $8ef367, $ee2818);
			InterpolatePalette(045, 076, $ee2818, $3838d3);
			InterpolatePalette(076, 100, $3838d3, $0bcfee);
			InterpolatePalette(100, 127, $0bcfee, $8ef367);
		end;

	2..4:
		begin
			C := Random($FFFFFF); C2 := Random($FFFFFF);
			InterpolatePalette(0,  48,  Random($FFFFFF), C);
			InterpolatePalette(48, 92,  C, C2);
			InterpolatePalette(92, 127, C2, Random($FFFFFF));
		end

	else
		C := Random($FFFFFF);
		InterpolatePalette(0,  64,  Random($FFFFFF), C);
		InterpolatePalette(64, 127, C, Random($FFFFFF));
	end;

	PlasmaCtrX1 := Random(100);
	PlasmaCtrY1 := Random(100);
	PlasmaCtrX2 := Random(100);
	PlasmaCtrY2 := Random(100);
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
	X, Y, PX1, PY1, PX2, PY2, i, ii: Integer;
begin
	PlasmaCtrX1 := PlasmaCtrX1 + 0.3;
	PlasmaCtrY1 := PlasmaCtrY1 + 0.25;
	PlasmaCtrX2 := PlasmaCtrX2 + 0.8;
	PlasmaCtrY2 := PlasmaCtrY2 + 0.5;

	PX1 := Trunc(Sin(PlasmaCtrX1 / 100) * 140);
	PY1 := Trunc(Cos(PlasmaCtrY1 / 80)  * 116);
	PX2 := Trunc(Sin(PlasmaCtrX2 / 84)  * 180);
	PY2 := Trunc(Cos(PlasmaCtrY2 / 71)  * 223);

	ii := Pix[Trunc(15 * Brightness)].Char;

	for Y := 0 to Logo.Height-1 do
	for X := 0 to Logo.Width-1 do
	if Logo.Pixels[X, Y] > 0 then
	begin
		i := PlasmaFunc1(X + PX1, Y + PY1) + PlasmaFunc2(X + PX2, Y + PY2);

		if BrightnessChars then
			ii := Pix[Trunc(i div 16 * Brightness)].Char;

		Console.PutChar(DestX + Rect.Left + X, DestY + Rect.Top + Y,
			ii, Trunc(i * Brightness) div 2 + 16);
	end;

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
	begin
		Inc(Counter);
		if Counter > AnimLength then
		begin
			Fading := -1; // fade out
			Counter := 0;
		end;
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
		Types.Rect(3, 4-1, Console.Width-3, Console.Height-12-1), True);

	Box.SetBorder(True, True, True, False);
	Box.ColorBack := 0;//15;
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

		Console.BlitChar(Scroll, Scroll.Width-8, 0,
			Ord(ScrollText[ScrollChar]),
			Console.Palette[ScrollColor], TRANSCOLOR);

		ScrollPix := 0;
	end;

	{Console.Bitmap.DrawColorKey(DR.Left, DR.Bottom - Scroll.Height-1,
		Types.Rect(ScrollPix, 0, RectWidth(DR)+ScrollPix-1, Scroll.Height),
		TRANSCOLOR, Scroll);}

	Console.Bitmap.Draw(DR.Left, DR.Bottom + 5,
		Types.Rect(ScrollPix, 0, RectWidth(DR)+ScrollPix-1, Scroll.Height),
		Scroll);
end;

{function TSplashScreen.BoxMouseDown(Ctrl: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := True;
end;}

end.

