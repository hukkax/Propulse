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
	private
		Fading:		ShortInt;
		Brightness:	Single;
		AnimType,
		AnimCount:	Byte;
		Counter:	Cardinal;
		Logo,
		Mask,
		Buffer:		TPCXImage;
		Pix:		array [0..15] of TConsolePixel;
		procedure NextAnimation;
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

		function 	BoxMouseDown(Ctrl: TCWEControl;
					Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
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

{const
	TRANSCOLOR = $FF000000;}


implementation

uses
	MainWindow,
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
		Console.Palette[x] :=
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

	Mask := TPCXImage.Create;
	Mask.LoadFromFile(DataPath + 'logomask.pcx');

	Buffer := TPCXImage.Create(Logo.Width, Logo.Height);

	Rect := Bounds(
		(W div 2) - (Logo.Width  div 2),
		(H div 2) - (Logo.Height div 2),
		Logo.Width, Logo.Height);

	AnimCount := Mask.Height div Logo.Height;

	InterpolatePalette(16, 31, $002266, $4488FF);

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

	AnimType := 0;
	Counter := 0;

	Brightness := 0;
	Fading := 1;
end;

destructor TSplashEffect.Destroy;
begin
	Logo.Free;
	Mask.Free;
	Buffer.Free;
	inherited Destroy;
end;

procedure TSplashEffect.NextAnimation;
begin
	InterpolatePalette(16, 31, Random($FFFFFF), Random($FFFFFF));
	Inc(AnimType);
	if AnimType >= AnimCount then
		AnimType := 0;
end;

procedure TSplashEffect.Render(var Buffer: TBitmap32; DestX, DestY: Cardinal);
var
	X, Y, i, ii, MaskY: Integer;
	P: PByte;
begin
	MaskY := Logo.Height * AnimType;

	for Y := 0 to Logo.Height-1 do
	for X := 0 to Logo.Width-1 do
	begin
		P := @Mask.Pixels[X, Y + MaskY];
		i := P^;
		if (i > 0) then
		begin
			ii := i;
			if ii >= 16 then ii := 15 - (ii - 16); // 16..31 => 15..1
			ii := Trunc(ii * Brightness);
			if (Logo.Pixels[X, Y] > 0) then
				Console.PutChar(DestX + Rect.Left + X, DestY + Rect.Top + Y,
				Pix[ii].Char, Pix[ii].Color);

			if Brightness > 0.3 then
			begin
				if i < 30 then
					Inc(i)
				else
					i := 1;
				P^ := i;
			end;
		end;
	end;

	if Fading <> 0 then
	begin
		if Fading > 0 then
		begin
			Brightness := Brightness + 0.01;
			if Brightness >= 1.0 then
			begin
				Brightness := 1.0;
				Fading := 0;
			end;
		end
		else
		if Fading < 0 then
		begin
			Brightness := Brightness - 0.01;
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

{for x := 0 to 15 do
	Console.PutChar(DestX+X, DestY, Pix[15].Char, x+16);}
end;

// ============================================================================
// TSplashScreen
// ============================================================================

procedure TSplashScreen.Show;
begin
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
	Fn: String;
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

	Box := TCWEControl.Create(Self, '', 'Logo',
		Types.Rect(3, 4-1, Console.Width-3, Console.Height-12-1), True);

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
const
	X = 6;
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
//	Console.WriteCentered(' http://hukka.yiff.fi/porotracker/', Y+4);

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

function TSplashScreen.BoxMouseDown(Ctrl: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := True;
end;

end.

