unit Screen.Splash;

{$I propulse.inc}

interface

uses
	Classes, Types,
	TextMode, CWE.Core, CWE.Widgets.Text,
	Graphics32;

type
	TSplashEffect = class
	public
		Rect: 		TRect;

		constructor	Create(R: TRect; W, H: Cardinal); overload;
		destructor	Destroy; override;
		procedure	Render(var Buffer: TBitmap32; DestX, DestY: Cardinal); virtual; abstract;
	end;

	TSplashScreen = class(TCWEScreen)
	private
		TRANSCOLOR:	TColor32;
		//Mid: 		TPoint;
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

	procedure InterpolatePalette(i1, i2: Byte; Col1, Col2: TColor32);

var
	SplashScreen: TSplashScreen;


implementation

uses
	SysUtils,
	BuildInfo,
	{$IFDEF MIDI}ProTracker.MIDI,{$ENDIF}
	ProTracker.Util,
	Screen.Editor,
	FX.VectorLogo;


const
	SCRSEP = '%R'#3'%%';

	Scroll_Text: AnsiString = '                  ' +
	'%Ghukka%% presents %WPropulse Tracker ' + ProTracker.Util.VERSION + '%% § ' +
	'%WPT2PLAY%% playroutine and coding help by %G8bitbubsy%% § ' +
	'Thanks to %Gmuzzy%% § %Gwuffe%% § %GTempest%% § %GMark Knopper%% § ' +
	'For full credits press F1 § ' +
	'Greets to %WVoid § %Wexec § %Wsvenonacid § %Y#kukkakoodi § %Y#protracker § '+
	'%GHaikz § %GArchyx § %GHofnarr § %GKaarlo § %GCrank § %GLarkku § %Gamaneog § ' +
	'%%' + '                   ';


// ============================================================================
// Utility
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

// ============================================================================
// TSplashEffect
// ============================================================================

constructor TSplashEffect.Create(R: TRect; W, H: Cardinal);
begin
	inherited Create;
	Rect := R;
end;

destructor TSplashEffect.Destroy;
begin
	inherited Destroy;
end;

// ============================================================================
// TSplashScreen
// ============================================================================

procedure TSplashScreen.Show;
{$IFDEF MIDI}
var
	Ctrl: TMIDIController;
{$ENDIF}
begin
	Randomize;
	ScrollText := StringReplace(Scroll_Text, '§', SCRSEP, [rfReplaceAll]);
	ScrollChar := 0;

	{$IFDEF MIDI}
//	if (Options.Midi.Enabled) and
	for Ctrl in MIDI.Controllers do
		if (Ctrl <> nil) and (Ctrl.ScrollText <> nil) then
			Ctrl.ScrollText.Text := ' PROPULSE tracker by hukka ';
	{$ENDIF}
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
{var
	W, H: Integer;}
begin
	Box.GetPixelRect(DR);
	{W := RectWidth(DR);
	H := RectHeight(DR);
	Mid.X := W div 2;
	Mid.Y := H div 2;}

	Scroll.SetSize(RectWidth(DR) + Console.Font.Width, Console.Font.Height);

	ScrollPix  := 0;
	ScrollChar := 0;

	if Effect <> nil then
		Effect.Free;
//	Effect := TSplashEffectPlasma.Create(DR, RectWidth(Box.Rect), RectHeight(Box.Rect));
	Effect := TSplashEffectVector.Create(DR, RectWidth(Box.Rect), RectHeight(Box.Rect));
end;

constructor TSplashScreen.Create;
var
	Button: TCWEButton;
begin
	inherited;

	Box := TCWEControl.Create(Self, '', 'Logo',
		Types.Rect(4, 4-1, Console.Width-4, Console.Height-11), True);

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
//		TRANSCOLOR := Console.Palette[TConsole.COLOR_PANEL];
		TRANSCOLOR := $FF0000FF;
		Scroll.Clear(TRANSCOLOR);
	end;

	Console.WriteHeader('Propulse Tracker v' + VERSION + ' '+#7+' '+ Build.CompileDate, 1);

	Y := Console.Height - 9;

	Console.WriteCentered(' (C) 2016-2018 hukka (Joel Toivonen)', Y+0);
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

{		Console.BlitChar(Scroll, Scroll.Width-Console.Font.Width-1, 0,
			Ord(ScrollText[ScrollChar]),
			{Console.Palette[ScrollColor], TRANSCOLOR);}
		Console.BlitChar(Scroll, Scroll.Width-Console.Font.Width-1, 0,
			Ord(ScrollText[ScrollChar]),
			$FFFFFFFF, TRANSCOLOR);

		ScrollPix := 0;
	end;

{	Console.Bitmap.Draw(DR.Left, DR.Bottom + 5,
		Types.Bounds(ScrollPix, 0, RectWidth(DR)-1, Scroll.Height-1),
		Scroll);}

	Console.Bitmap.Draw(DR.Left, DR.Top,
		Types.Bounds(ScrollPix, 0, RectWidth(DR)-1, Scroll.Height-1),
		Scroll);
	Console.Bitmap.Draw(DR.Left, DR.Bottom-Console.Font.Height,
		Types.Bounds(ScrollPix, 0, RectWidth(DR)-1, Scroll.Height-1),
		Scroll);
end;

{function TSplashScreen.BoxMouseDown(Ctrl: TCWEControl;
	Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Result := True;
end;}

end.

