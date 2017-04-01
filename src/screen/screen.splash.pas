unit Screen.Splash;

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text,
	Graphics32;

type
	// adapted from http://lodev.org/cgtutor/tunnel.html
	TFxTunnel = class
	private
		Distances,
		Angles: 	array of array of Integer;
		AnimCounter,
		MoveCounter,
		Animation,
		Movement: 	Single;
		Size: 		TPoint;
	public
		Texture:   	TBitmap32;

		constructor	Create(W, H: Integer);
		destructor	Destroy; override;
		procedure	Render(var Buffer: TBitmap32; X, Y: Integer);
	end;

	TSplashScreen = class(TCWEScreen)
	private
		Mid: TPoint;
		DR: TRect;
		Scroll: TBitmap32;
		ScrollPix, ScrollChar: Cardinal;
		ScrollColor: Byte;
		ScrollText: AnsiString;
		Tunnel: TFxTunnel;

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

const
	TRANSCOLOR = $FF000000;


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
// TFxTunnel
// ============================================================================

constructor TFxTunnel.Create(W, H: Integer);
var
	Ratio, ww, hh, x, y: Integer;
	z: Single;
begin
	Ratio := Random(30) + 10;

	Size := Point(W, H);
	ww := W * 2;
	hh := H * 2;

	SetLength(Distances, ww, hh);
	SetLength(Angles,    ww, hh);

	Texture := TBitmap32.Create;
	LoadImage(DataPath + 'stexture.pcx', Texture);

	// Precalc pixel/coordinate mappings
	for y := 0 to hh-1 do
	for x := 0 to ww-1 do
	begin
		z := Sqrt((x - W) * (x - W) + (y - H) * (y - H));
		if z <> 0.0 then
			Distances[x, y] := Trunc(Ratio * Texture.Height / z) mod Texture.Height
		else
			Distances[x, y] := 0;
		Angles[x, y] := Trunc(
			0.5 * Texture.Width * ArcTan2(y - h, x - w) / Pi);
	end;

	Animation := 300.0;
	Movement  := 300.0;
	AnimCounter := Random(90);
	MoveCounter := Random(270);
end;

destructor TFxTunnel.Destroy;
begin
	Texture.Free;
	inherited Destroy;
end;

procedure TFxTunnel.Render(var Buffer: TBitmap32; X, Y: Integer);
var
	shiftX, shiftY, shiftlookX, shiftlookY,
	xx, yy, tx, ty: Integer;
begin
	AnimCounter := AnimCounter + 0.005;
	if AnimCounter >= 360.0 then
		AnimCounter := AnimCounter - 360.0;

	MoveCounter := MoveCounter + 0.007;
	if MoveCounter >= 360.0 then
		MoveCounter := MoveCounter - 360.0;

	Animation := Animation + (Sin(AnimCounter) * 0.7) + 3.5;
	Movement  := Movement  + (Cos(MoveCounter) * 1.3) + 0.6;

    shiftX := Trunc(Texture.Width  *  1.0 * AnimCounter);
    shiftY := Trunc(Texture.Height * 0.25 * AnimCounter);
    shiftLookX := Size.X div 2 + Trunc(Size.X div 2 * Sin(AnimCounter));
    shiftLookY := Size.Y div 2 + Trunc(Size.Y div 2 * Sin(AnimCounter * 2.0));

	for yy := 0 to Size.Y-1 do
	for xx := 0 to Size.X-1 do
	begin
		tx := Trunc(Angles[xx + shiftlookX, yy+shiftlookY] + Movement + shiftlookX) mod Texture.Width;
		ty := Trunc(Distances[xx + shiftlookX, yy + shiftlookY] + Animation + shiftlookY) mod Texture.Height;
		Buffer.Pixel[X+xx, Y+yy] := Texture.Pixel[tx, ty];
	end;
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

	if Assigned(Tunnel) then
		Tunnel.Free;
	Tunnel := TFxTunnel.Create(W, H);

	Scroll.SetSize(W + Console.Font.Width, Console.Font.Height);
	Scroll.Clear(TRANSCOLOR);

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

	Box := TCWEControl.Create(Self, '', 'Logo',
		Types.Rect(3, 4, Console.Width-3, Console.Height-12), True);
	Box.SetBorder(True, True, True, False);
	Box.ColorBack := 0;
	Box.OnMouseDown := BoxMouseDown;
	Box.WantMouse := True;

	Button := TCWEButton.Create(Self, 'Continue', 'bOK',
		Types.Rect(32, Console.Height-3, Console.Width-32, Console.Height-2));

	TCWEURLLabel.Create(Self, URL, 'website',
		Console.CenterAt(Length(URL), Console.Height-6));

	ActiveControl := Button;
	Button.OnChange := OKClick;

	Scroll := TBitmap32.Create;

	Tunnel := nil;
	Init;
end;

destructor TSplashScreen.Destroy;
begin
	Tunnel.Free;
	Scroll.Free;
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

procedure TSplashScreen.Update(Draw: Boolean = True);
var
	C: AnsiChar;
begin
	Tunnel.Render(Console.Bitmap, DR.Left, DR.Top);

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

		Console.BlitChar(Scroll, Scroll.Width-8, 0,
			Ord(ScrollText[ScrollChar]),
			Console.Palette[ScrollColor], TRANSCOLOR);

		ScrollPix := 0;
	end;

	Console.Bitmap.DrawColorKey(DR.Left, DR.Bottom - Scroll.Height-1,
		Types.Rect(ScrollPix, 0, RectWidth(DR)+ScrollPix-1, Scroll.Height),
		TRANSCOLOR, Scroll);
end;

function TSplashScreen.BoxMouseDown(Ctrl: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := True;
end;

end.

