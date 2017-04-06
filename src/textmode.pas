unit TextMode;

interface

{.$DEFINE SHOWREPAINT}
{.$DEFINE SPEEDTEST}

uses
	Classes, Types,
	Graphics32;

type
	TConsole = class;

	TConsoleChar = record
		Char,
		ColorBack,
		ColorFore: Byte;
		Changed: Boolean;
	end;

	TConsoleFont = class //record
		Width,
		Height:			Integer;
		Bitmap:			TBitmap32;
		PixelRow:		array[0..255] of array of Cardinal;
		GlyphRect:		array[0..255] of TRect;
		IsGlyphEmpty:	array[0..255] of Boolean;

		function 	LoadFromFile(Filename: String; const Console: TConsole): Boolean;
		destructor	Destroy; override;
	end;

	TConsoleCursor = record
		X, Y: 		Byte;
		ColorBack,
		ColorFore:	Byte;
		Visible: 	Boolean;
	end;

	TConsole = class
	class var
		COLOR_TEXT,
		COLOR_PANEL,
		COLOR_3DDARK,
		COLOR_3DLIGHT,
		COLOR_LIGHT,
		COLOR_BLANK,
		COLOR_LINK:	Byte;
	private
		Locked:		Boolean;
		LockCount:	Integer;
	public
		Width,
		Height: 	Integer;
		Cursor: 	TConsoleCursor;
		Bitmap: 	TBitmap32;
		Buffer: 	packed array of packed array of TConsoleChar;
		Palette: 	array[0..15+16] of TColor32;
		Font: 		TConsoleFont;
		OnChange: 	procedure of Object;

		constructor Create(_Width, _Height: Integer; const FontFile, PaletteFile: String);
		destructor  Destroy; override;

		function 	LoadFont(const Filename: String): Boolean;
		function 	LoadPalette(const Filename: String): Boolean;

		function	PixelWidth(x: Word): Cardinal; inline;
		function	PixelHeight(y: Word): Cardinal; inline;
		function	ToPixels(x, y: Word): TPoint; inline;
		function 	GetPixelRect(const R: TRect; Grow: ShortInt = 0): TRect; inline;
		function 	CenterAt(CtrlWidth, Y: Byte): TRect; inline;

		procedure	Clear(Force: Boolean; ch: Byte = 32; fg: Integer = -1; bg: Integer = -1);
		procedure	SetCursor(Show: Boolean; X: Byte = 255; Y: Byte = 255);

		procedure	Paint;
		procedure	Refresh;
		procedure	BeginUpdate;
		procedure	EndUpdate;

		procedure	DoDrawChar(const x, y: Byte); inline;
		procedure	DrawChar(const x, y: Byte); inline;
		procedure	PutChar(X, Y: Word; nChar: Byte;
					nFgCol: Integer = -1; nBgCol: Integer = -1;
					Force: Boolean = False);
		procedure 	BlitChar(var Buffer: TBitmap32; X, Y: Word;
					nChar: Byte; nFgCol: TColor32; nBgCol: TColor32 = 0);
		procedure 	BlitText(var Buffer: TBitmap32; X, Y: Word;
					const Text: AnsiString; nFgCol: TColor32; nBgCol: TColor32 = 0);
		procedure	SetColor(X, Y: Word;
					nFgCol: Integer = -1; nBgCol: Integer = -1);
		procedure	Write(const sText: AnsiString; X, Y: Word;
					nFgCol: Integer = -1; nBgCol: Integer = -1);
		procedure 	WriteCentered(const sText: AnsiString; Y: Word;
					Left: Integer = 0; Right: Integer = -1;
					nFgCol: Integer = -1; nBgCol: Integer = -1);
		procedure 	WriteHeader(const sText: AnsiString; Y: Word;
					nFgCol: Integer = -1; nBgCol: Integer = -1;
					Left: Integer = 1; Right: Integer = -1);
		procedure 	FillRect(const R: TRect; const nChar: AnsiChar = ' ';
					nFgCol: Integer = -1; nBgCol: Integer = -1);
		procedure 	FrameRect(R: TRect; Sunken: Boolean = False; Fat: Boolean = True;
					nBgCol: ShortInt=-1; nLightCol: ShortInt=-1; nDarkCol: ShortInt=-1);
		procedure 	FrameRectPx(R: TRect; Sunken: Boolean = False; Fat: Boolean = False;
					nBgCol: ShortInt=-1; nLightCol: ShortInt=-1; nDarkCol: ShortInt=-1);

		{$IFDEF SPEEDTEST}
		function	SpeedTest(Times: Word): Cardinal;
		{$ENDIF}
	property
		IsLocked:	Boolean read Locked;
	end;


	function FindImageFile(const Filename: String): String;
	function LoadImage(var Filename: String): TBitmap32; overload;
	function LoadImage(const Filename: String; var Bmp: TBitmap32): String; overload;


implementation

uses
	{$IFDEF SPEEDTEST}
	TimeMeasurer, Dialogs,
	{$ENDIF}
	SysUtils;


// ==========================================================================
// Utility
// ==========================================================================

// Tries to find the working filename for an image file (BMP or PNG)
// given a filename with or without an extension.
//
function FindImageFile(const Filename: String): String;
var
	sFilename: String;
begin
	if FileExists(Filename) then Exit(Filename); // original filename

	sFilename := ChangeFileExt(Filename, '.pcx');
	if (Filename <> sFilename) and (FileExists(sFilename)) then
		Exit(sFilename);

{	sFilename := ChangeFileExt(Filename, '.bmp');
	if (Filename <> sFilename) and (FileExists(sFilename)) then
		Exit(sFilename);

	sFilename := ChangeFileExt(Filename, '.png');
	if (Filename <> sFilename) and (FileExists(sFilename)) then
		Exit(sFilename);}

	Result := '';
//	showmessage('not found ' + filename);
end;

function LoadImage(var Filename: String): TBitmap32;
var
	sFilename: String;
begin
	sFilename := FindImageFile(Filename);
	if sFilename <> '' then
	begin
		Result := TBitmap32.Create;
		Result.LoadFromFile(sFilename);
		Filename := sFilename;
	end
	else
		Result := nil;
end;

function LoadImage(const Filename: String; var Bmp: TBitmap32): String;
begin
	Result := FindImageFile(Filename);
	if Result <> '' then
	begin
		if not Assigned(Bmp) then
			Bmp := TBitmap32.Create;
		Bmp.LoadFromFile(Result);
	end;
end;

{$IFDEF SPEEDTEST}
function TConsole.SpeedTest(Times: Word): Cardinal;
var
	i, x, y: Integer;
	Time: TTimeMeasurer;
begin
	Result := 0;

	BeginUpdate;
	Time.Init;
	Time.Start;

	for y := 0 to Height-1 do
	for x := 0 to Width-1 do
	begin
		//{
		Buffer[x,y].Char := Random(255);
		Buffer[x,y].ColorBack := Random(15);
		Buffer[x,y].ColorFore := Random(15);
		//}
		Buffer[x,y].Changed := True;
	end;

	for i := 0 to Times do
		for y := 0 to Height-1 do
		for x := 0 to Width-1 do
			DoDrawChar(x,y);

	Time.Stop;
	EndUpdate;

	Result := Time.Milliseconds;
	ShowMessage(IntToStr(Result) + ' ms for ' + IntToStr(Times) + ' redraws.');
end;
{$ENDIF}

{ TConsoleFont }

destructor TConsoleFont.Destroy;
begin
	if Assigned(Bitmap) then
		Bitmap.Free;
	inherited Destroy;
end;

function TConsoleFont.LoadFromFile(Filename: String;
	const Console: TConsole): Boolean;
var
	g, x, y, ox, oy, CharsPerRow: Integer;
begin
	if not FileExists(Filename) then
		Filename := ChangeFileExt(Filename, '.pcx');

	Result := FileExists(Filename);
	if not Result then Exit;

	if Bitmap = nil then
		Bitmap := TBitmap32.Create;

	Result := (LoadImage(Filename, Bitmap) <> '');
	if not Result then Exit;

	// determine if font bitmap has all glyphs in one row or column
	// or if it's arranged in a 16x16 matrix
	if (Bitmap.Width <= 32) then
	begin
		CharsPerRow := 1;
		Width  := Bitmap.Width;
		Height := Bitmap.Height div 256;
	end
	else
	if Bitmap.Width > (Bitmap.Height * 10) then
	begin
		CharsPerRow := 256;
		Width  := Bitmap.Width  div CharsPerRow;
		Height := Bitmap.Height;
	end
	else
	begin
		CharsPerRow := 16;
		Width  := Bitmap.Width  div CharsPerRow;
		Height := Bitmap.Height div CharsPerRow;
	end;

	x := 0; y := 0;
	for g := 0 to 255 do
	begin
		GlyphRect[g] := Bounds(x*Width, y*Height, Width, Height);
		Inc(x);
		if x >= CharsPerRow then
		begin
			x := 0;
			Inc(y);
		end;
	end;

	for g := 0 to 255 do
	begin
		SetLength(PixelRow[g], Height);

		ox := GlyphRect[g].Left;
		oy := GlyphRect[g].Top;
		IsGlyphEmpty[g] := True;

		for y := 0 to Height-1 do
		begin
			for x := Width-1 downto 0 do
			begin
				PixelRow[g,y] := PixelRow[g,y] shl 1;
				if (Bitmap.Pixel[x+ox, y+oy] and $FFFFFF) <> 0 then
				begin
					PixelRow[g,y] := PixelRow[g,y] or 1;
					IsGlyphEmpty[g] := False;
				end;
			end;
		end;
	end;
end;

{ TConsole }

constructor TConsole.Create(_Width, _Height: Integer;
	const FontFile, PaletteFile: String);

	procedure SetPalette(const i, r, g, b: Byte);
	begin
		Palette[i] := Color32(r*4, g*4, b*4, 255);
	end;
begin
	inherited Create;

	Width := _Width;
	Height := _Height;
	SetLength(Buffer, Width, Height);

	// Impulse Tracker palette, TODO load from file
	SetPalette(00,00,00,00);
	SetPalette(01,31,22,17);
	SetPalette(02,45,37,30);
	SetPalette(03,58,58,50);
	SetPalette(04,44,00,21);
	SetPalette(05,63,63,21);
	SetPalette(06,17,38,18);
	SetPalette(07,19,03,06);
	SetPalette(08,08,21,00);
	SetPalette(09,06,29,11);
	SetPalette(10,14,39,29);
	SetPalette(11,55,58,56);
	SetPalette(12,40,40,40);
	SetPalette(13,35,05,21);
	SetPalette(14,22,16,15);
	SetPalette(15,13,12,11);

	COLOR_TEXT		:= 0;
	COLOR_PANEL		:= 2;
	COLOR_3DDARK	:= 1;
	COLOR_3DLIGHT	:= 3;
	COLOR_LIGHT		:= 11;
	COLOR_BLANK		:= 0;
	COLOR_LINK		:= 9;

	LoadPalette(PaletteFile);
	LoadFont(FontFile);

	Clear(False);
end;

destructor TConsole.Destroy;
begin
	Bitmap.Free;
	Font.Free;

	inherited Destroy;
end;

procedure TConsole.BeginUpdate;
begin
	Inc(LockCount);
	Locked := (LockCount > 0);
end;

procedure TConsole.EndUpdate;
begin
	if LockCount > 0 then
		Dec(LockCount);
	Locked := (LockCount > 0);
end;

function TConsole.CenterAt(CtrlWidth, Y: Byte): TRect;
begin
	Result := Types.Bounds((Width div 2) - (CtrlWidth div 2) - 1, Y, CtrlWidth, 1);
end;

function TConsole.LoadFont(const Filename: String): Boolean;
begin
	if Font <> nil then
		Font.Free;
	Font := TConsoleFont.Create;

	Result := Font.LoadFromFile(Filename, Self);
	if Result then
	begin
		if Bitmap = nil then
			Bitmap := TBitmap32.Create;
		Bitmap.SetSize(Width*Font.Width, Height*Font.Height);
	end;
end;

function TConsole.LoadPalette(const Filename: String): Boolean;
var
	x, y: Integer;
	pal: TBitmap32;
	Sl: TStringList;
	Fn: String;
begin
	Fn := Filename;

	if ExtractFileExt(Fn) = '.ini' then
	begin
		Result := FileExists(Fn);
		if not Result then Exit;
	end
	else
	begin
		pal := LoadImage(Fn);
		Result := (pal <> nil);
		if Result then
		begin
			for y := 0 to 15 do
			for x := 0 to 15 do
				Palette[y*16+x] := pal.Pixel[x,y];
			pal.Free;
			Exit;
		end;
	end;

	if not Result then
	begin
		Fn := Filename + '.ini';
		if not FileExists(Fn) then Exit;
	end;

	Sl := TStringList.Create;
	Sl.Free;
end;

function TConsole.PixelWidth(x: Word): Cardinal;
begin
	Result := x * Font.Width;
end;

function TConsole.PixelHeight(y: Word): Cardinal;
begin
	Result := y * Font.Height;
end;

function TConsole.ToPixels(x, y: Word): TPoint;
begin
	Result := Point(PixelWidth(x), PixelHeight(y));
end;

function TConsole.GetPixelRect(const R: TRect; Grow: ShortInt = 0): TRect;
begin
	Result := Rect(
		R.Left*Font.Width - Grow,
		R.Top*Font.Height - Grow,
		R.Right*Font.Width   + Grow,
		R.Bottom*Font.Height + Grow);
end;

procedure TConsole.SetColor(X, Y: Word; nFgCol, nBgCol: Integer);
begin
	if (X < Width) and (Y < Height) then
	with Buffer[X, Y] do
	begin
		if (nFgCol >= 0) and (ColorFore <> nFgCol) then
		begin
			ColorFore := nFgCol;
			Changed := True;
		end;
		if (nBgCol >= 0) and (ColorBack <> nBgCol) then
		begin
			ColorBack := nBgCol;
			Changed := True;
		end;
		if Changed then
			DoDrawChar(X, Y); // inline
	end;
end;

procedure TConsole.SetCursor(Show: Boolean; X: Byte = 255; Y: Byte = 255);
begin
	if X <> 255 then
		Cursor.X := X;
	if Y <> 255 then
		Cursor.Y := Y;
	Cursor.Visible := Show;
end;

procedure TConsole.Clear(Force: Boolean; ch: Byte = 32; fg: Integer = -1; bg: Integer = -1);
var
	x, y: Integer;
begin
	if bg < 0 then bg := COLOR_BLANK;
	if fg < 0 then fg := COLOR_TEXT;

	SetCursor(True, 0, 0);
	for y := 0 to Height-1 do
	for x := 0 to Width-1 do
		PutChar(x, y, ch, fg, bg, Force);
end;

procedure TConsole.Refresh;
var
	x, y: Integer;
begin
	for y := 0 to Height-1 do
	for x := 0 to Width-1 do
		Buffer[x,y].Changed := True;
	Paint;
end;

procedure TConsole.DoDrawChar(const x, y: Byte);
var
	oy, sx, sy, ch: Integer;
	bchar: TConsoleChar;
	tmpcolor: TColor32;
	Scanline: Cardinal;
	ScanlinePtr: PColor32;
begin
	{$IFDEF SHOWREPAINT}
	if Locked then
	begin
		if (x < Width) and (y < Height) then
			Buffer[x,y].ColorBack := Random(15);
	end;
	{$ENDIF}

	if {(Locked) or} (x >= Width) or (y >= Height) then Exit;

	sx := Font.Width;
	sy := Font.Height;
	bchar := Buffer[x,y];
	ch := bchar.Char;

	{$IFDEF SHOWREPAINT}
	Bitmap.FillRectS(x*sx, y*sy, x*sx+sx, y*sy+sy, Palette[Random(15)]);
	{$ELSE}
	Bitmap.FillRectS(x*sx, y*sy, x*sx+sx, y*sy+sy, Palette[bchar.ColorBack]);
	{$ENDIF}

	if Font.IsGlyphEmpty[ch] then Exit;

	tmpcolor := Palette[bchar.ColorFore];

	for oy := 0 to Font.Height-1 do
	begin
		Scanline := Font.PixelRow[ch, oy];
		ScanlinePtr := Bitmap.PixelPtr[x*sx, y*sy + oy];
		while Scanline > 0 do
		begin
			if (Scanline and 1) <> 0 then
				ScanlinePtr^ := tmpcolor;
			Inc(ScanlinePtr);
			Scanline := Scanline shr 1;
		end;
	end;
end;

procedure TConsole.DrawChar(const x, y: Byte);
begin
//	if not Locked then
	DoDrawChar(x, y);
end;

procedure TConsole.PutChar(X, Y: Word; nChar: Byte;
	nFgCol: Integer = -1; nBgCol: Integer = -1;
	Force: Boolean = False);
begin
	if (X < Width) and (Y < Height) then
	with Buffer[X, Y] do
	begin
		if Char <> nChar then
		begin
			Char := nChar;
			Changed := True;
		end;
		if (nFgCol >= 0) and (ColorFore <> nFgCol) then
		begin
			ColorFore := nFgCol;
			Changed := True;
		end;
		if (nBgCol >= 0) and (ColorBack <> nBgCol) then
		begin
			ColorBack := nBgCol;
			Changed := True;
		end;
		if (Force) or (Changed) then
		begin
			//if not Locked then
				DoDrawChar(X, Y);
			Changed := False;
		end;
	end;
end;

// X,Y = pixel coords
procedure TConsole.BlitChar(var Buffer: TBitmap32; X, Y: Word;
	nChar: Byte; nFgCol: TColor32; nBgCol: TColor32 = 0);
var
	oy, sx, sy: Integer;
	Scanline: Cardinal;
	ScanlinePtr: PColor32;
begin
	sx := Font.Width;
	sy := Font.Height;

	if nBgCol <> 0 then
		Buffer.FillRectS(x, y, x+sx, y+sy, nBgCol);

	if Font.IsGlyphEmpty[nChar] then Exit;

	for oy := 0 to sy-1 do
	begin
		Scanline := Font.PixelRow[nChar, oy];
		ScanlinePtr := Buffer.PixelPtr[x, y+oy];
		while Scanline > 0 do
		begin
			if (Scanline and 1) <> 0 then
				ScanlinePtr^ := nFgCol;
			Inc(ScanlinePtr);
			Scanline := Scanline shr 1;
		end;
	end;
end;

procedure TConsole.BlitText(var Buffer: TBitmap32; X, Y: Word;
	const Text: AnsiString; nFgCol: TColor32; nBgCol: TColor32 = 0);
var
	n: Integer;
begin
	for n := 1 to Length(Text) do
		BlitChar(Buffer, X + (Font.Width * (n-1)), Y, Ord(Text[n]), nFgCol, nBgCol);
end;

procedure TConsole.Write(const sText: AnsiString; X, Y: Word;
	nFgCol: Integer = -1; nBgCol: Integer = -1);
var
	i: Integer;
begin
	for i := 1 to Length(sText) do
		PutChar(X+i-1, Y, Ord(sText[i]), nFgCol, nBgCol);
end;

procedure TConsole.WriteCentered(const sText: AnsiString; Y: Word;
	Left: Integer = 0; Right: Integer = -1;
	nFgCol: Integer = -1; nBgCol: Integer = -1);
var
	X, i: Integer;
begin
	if Right <= Left then
		Right := Width - 2;

	X := ((Right - Left + 1) div 2) - (Length(sText) div 2) + Left;
	for i := 0 to Length(sText)-1 do
		PutChar(X+i, Y, Ord(sText[i+1]), nFgCol, nBgCol);
end;

procedure TConsole.WriteHeader(const sText: AnsiString; Y: Word;
	nFgCol: Integer = -1; nBgCol: Integer = -1;
	Left: Integer = 1; Right: Integer = -1);
var
	X: Integer;
begin
	if Right <= Left then
		Right := Width - 1;
	if nBgCol < 0 then nBgCol := COLOR_PANEL;
	if nFgCol < 0 then nFgCol := COLOR_TEXT;

	for x := Left to Right-1 do
		PutChar(x, Y, 154, COLOR_3DDARK, nBgCol);

	if sText <> '' then
		WriteCentered(' ' + sText + ' ', Y, Left, Right-1, nFgCol);
end;

procedure TConsole.FillRect(const R: TRect; const nChar: AnsiChar = ' ';
	nFgCol: Integer = -1; nBgCol: Integer = -1);
var
	x, y: Integer;
begin
	for y := R.Top to R.Bottom-1 do
	for x := R.Left to R.Right-1 do
		PutChar(x, y, Ord(nChar), nfgCol, nBgCol);
	{with Buffer[x,y] do
	begin
		Char := Ord(nChar);
		if nFgCol >= 0 then ColorFore := nFgCol;
		if nBgCol >= 0 then ColorBack := nBgCol;
		Changed := True;
	end;}
	if nBgCol >= 0 then
		Bitmap.FillRectS(GetPixelRect(R), Palette[nBgCol]);
end;

procedure TConsole.FrameRectPx(R: TRect; Sunken: Boolean = False; Fat: Boolean = False;
	nBgCol: ShortInt=-1; nLightCol: ShortInt=-1; nDarkCol: ShortInt=-1);
var
	x1, y1, x2, y2: Integer;
	col: TColor32;
label
	Draw;
begin
	x1 := R.Left * Font.Width  - 1;
	y1 := R.Top  * Font.Height - 1;
	x2 := R.Right  * Font.Width;
	y2 := R.Bottom * Font.Height;

	if nBgCol >= 0 then
		Bitmap.FillRectS(x1, y1, x2, y2, Palette[nBgCol]);

{	if nBgCol < 0 then
		nBgCol    := COLOR_PANEL;}
	if nLightCol < 0 then
		nLightCol := COLOR_3DLIGHT;
	if nDarkCol < 0 then
		nDarkCol  := COLOR_3DDARK;

Draw:
	if Sunken then
		col := Palette[nDarkCol]
	else
		col := Palette[nLightCol];

	Bitmap.HorzLine(x1, y1, x2, col);
	Bitmap.VertLine(x1, y1, y2, col);

	if Sunken then
		col := Palette[nLightCol]
	else
		col := Palette[nDarkCol];

	Bitmap.HorzLine(x1, y2, x2, col);
	Bitmap.VertLine(x2, y1, y2, col);

	if Fat then
	begin
		Dec(x1); Dec(y1);
		Inc(x2); Inc(y2);
		Fat := False;
		goto Draw;
	end;
end;

procedure TConsole.FrameRect(R: TRect; Sunken: Boolean = False; Fat: Boolean = True;
	nBgCol: ShortInt=-1; nLightCol: ShortInt=-1; nDarkCol: ShortInt=-1);
var
	i, x, y, os: Integer;
const
	Force = True;
begin
	if not Fat then
		os := 128
	else
		os := 128 + 14;

	if nBgCol in [0..15] then
		FillRect(R, ' ', -1, nBgCol);
//	else
		nBgCol := COLOR_PANEL;

{	if nBgCol < 0 then
		nBgCol    := COLOR_PANEL;}
	if nLightCol < 0 then
		nLightCol := COLOR_3DLIGHT;
	if nDarkCol < 0 then
		nDarkCol  := COLOR_3DDARK;

	Dec(R.Left);
	Dec(R.Top);
	//Inc(R.Right);
	//Inc(R.Bottom);

	if Sunken then
	begin
		i := nLightCol;
		nLightCol := nDarkCol;
		nDarkCol := i;
	end;

	for y := R.Top to R.Bottom do
	begin
		PutChar(R.Left,  y, os+4, nLightCol, nBgCol, Force);
		PutChar(R.Right, y, os+3, nDarkCol,  nBgCol, Force);
	end;

	for x := R.Left to R.Right do
	begin
		PutChar(x, R.Top,    os+6, nLightCol, nBgCol, Force);
		PutChar(x, R.Bottom, os+1, nDarkCol,  nBgCol, Force);
	end;

	PutChar(R.Left,  R.Top,    os+11);
	PutChar(R.Right, R.Top,    os+10);
	PutChar(R.Left,  R.Bottom, os+9);
	PutChar(R.Right, R.Bottom, os+8);

//	PutChar(R.Right, R.Top,    os+3);
//	PutChar(R.Left,  R.Bottom,   128+5);
end;

procedure TConsole.Paint;
var
	x, y: Integer;
begin
	for y := 0 to Height-1 do
	for x := 0 to Width-1 do
		if Buffer[x,y].Changed then
		begin
			DoDrawChar(x,y);
			Buffer[x,y].Changed := False;
		end;
end;

end.
