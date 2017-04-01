unit PCX;

{$mode delphi}

interface

uses
	SysUtils, Classes, Graphics32;

type
	TPCXPalette   = array [0..255] of array[0..2] of Byte;
	TPCXPalette32 = array [0..255] of TColor32;

	TPCXImage = class
		BitsPerPixel,
		Width,
		Height:		Integer;
		Palette:	TPCXPalette32;
		Pixels: 	array of array of Byte;
	end;

	function 	PCX_CreateImage(W, H: Integer): TPCXImage;
	function 	PCX_LoadPalette(Filename: String): TPCXPalette;
	function 	PCX_LoadPalette32(Filename: String): TPCXPalette32;
	function 	PCX_LoadImage(Filename: String): TPCXImage;
	procedure 	PCX_LoadImageInto(Filename: String; var buffer: array of Byte);
	procedure 	PCX_ClearBuffer(var image: TPCXImage; val: Byte);
	procedure 	PCX_RenderBuffer(var image: TPCXImage; var buffer: TBitmap32);


implementation

uses
	FileStreamEx;


function FileToString(Filename: String): RawByteString;
var
	FStrm: TFileStream;
begin
	Result := '';

	Filename := StringReplace(Filename, '\', '/', [rfReplaceAll]);

	if (Filename = '') or (not FileExists(Filename)) then Exit;
	try
		FStrm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
	except
		on EFOpenError do Exit;
	end;
	try
		SetLength(Result, FStrm.Size);
		SetCodePage(Result, 1252, False);
		FStrm.Read(Result[1], FStrm.Size);
	finally
		FStrm.Free;
	end;
end;


function PCX_LoadPalette(Filename: String): TPCXPalette;
var
	J, X, Y: Integer;
	P: String;
begin
	J := 1;
	P := FileToString(Filename);
	if Length(P) > 768 then
		P := Copy(P, Length(P)-767, 768);
	for Y := 0 to 255 do
		for X := 0 to 2 do
		begin
			Result[Y,X] := Ord(P[J]);
			Inc(J);
		end;
end;

function PCX_LoadPalette32(Filename: String): TPCXPalette32;
var
	J, r, g, X, Y: Integer;
	P: RawByteString;
begin
	J := 1;
	P := FileToString(Filename);
	if Length(P) > 768 then
		P := Copy(P, Length(P)-767, 768);
	for Y := 0 to 255 do
	begin
		r := Ord(P[J]);
		g := Ord(P[J+1]);
		Result[Y] := Color32(r, g, Ord(P[J+2]));
		Inc(J, 3);
	end;
end;

function PCX_CreateImage(W, H: Integer): TPCXImage;
begin
	Result := TPCXImage.Create;
	Result.Width := W;
	Result.Height := H;
	SetLength(Result.Pixels, Result.Width, Result.Height);
	PCX_ClearBuffer(Result, 0);
end;

procedure PCX_LoadImageInto(Filename: String; var buffer: array of Byte);
var
	tmp: TPCXImage;
	X, Y, i: Integer;
begin
	i := 0;
	tmp := PCX_LoadImage(Filename);
	for Y := 0 to tmp.Height-1 do
		for X := 0 to tmp.Width-1 do
		begin
			if i >= Length(buffer) then Break;
			buffer[i] := tmp.Pixels[X,Y];
			Inc(i);
		end;
end;

function PCX_LoadImage(Filename: String): TPCXImage;
var
	Bf: RawByteString;
	fsize: Int64;
	b: Byte;
	tavu1, tavu2, y,
	position, bytesperline, bytestoread: Integer;
	PCXFile: TFileStreamEx;
	OK: Boolean;
label
	Done;
begin
	OK := False;
	if not FileExists(Filename) then Exit(nil);

	PCXFile := TFileStreamEx.Create(Filename, fmOpenRead, fmShareDenyNone);

	Result := TPCXImage.Create;
	fsize := PCXFile.Size;

	if PCXFile.ReadByte <> $A then goto Done;
	PCXFile.SeekTo(3);
	Result.BitsPerPixel := PCXFile.ReadByte;

	PCXFile.SeekTo(8);
	Result.Width  := PCXFile.ReadWord + 1;
	Result.Height := PCXFile.ReadWord + 1;

	Bf := '';
	SetCodePage(Bf, 1252, False);
	PCXFile.SeekTo(128);
	position := PCXFile.Position;

	case Result.BitsPerPixel of
		1:	bytesperline := Result.Width div 8;
		4:	bytesperline := Result.Width div 2;
	else
		bytesperline := Result.Width;
	end;
	bytestoread := Result.Height * bytesperline;

	while Length(Bf) < bytestoread do
	begin
		tavu1 := PCXFile.ReadByte;
		Inc(position);
		if position > fsize then Break;
		if tavu1 > 192 then
		begin
			tavu2 := PCXFile.ReadByte;
			Inc(position);
			while tavu1 > 192 do
			begin
				Bf := Bf + Chr(tavu2);
				Dec(tavu1);
			end;
		end
		else
			Bf := Bf + Chr(tavu1);
	end;

	OK := True;
	SetLength(Result.Pixels, Result.Width, Result.Height);
	position := 1;

	case Result.BitsPerPixel of

		1:
		begin
			// get pixels
			for tavu1 := 0 to Result.Height-1 do
			for tavu2 := 0 to bytesperline-1 do
			begin
				b := Ord(Bf[position]);
				Inc(position);
				for y := 0 to 7 do
				begin
					if (b and 128) <> 0 then
						Result.Pixels[tavu2*8+y,tavu1] := 1
					else
						Result.Pixels[tavu2*8+y,tavu1] := 0;
					b := b shl 1;
				end;
			end;

			Result.Palette[0] := $000000;
			Result.Palette[1] := $FFFFFF;
		end;

		4:
		begin
		end;

		8:
		begin
			// get pixels
			for tavu1 := 0 to Result.Height-1 do
			for tavu2 := 0 to Result.Width-1 do
			begin
				Result.Pixels[tavu2,tavu1] := Ord(Bf[position]);
				Inc(position);
			end;

			// read palette
			if (fsize > 768) then
			begin
				PCXFile.SeekTo(fsize - 768);
				for Y := 0 to 255 do
					Result.Palette[Y] := Color32(
						PCXFile.ReadByte, PCXFile.ReadByte, PCXFile.ReadByte);
			end;
		end;
	else
		OK := False;
	end;

	{PCXFile.Free;
	PCXFile := TFileStreamEx.Create(Filename + '.dat', fmCreate or fmOpenWrite);
	//PCXFile.Write(Bf[1], Length(Bf));
	for tavu1 := 0 to Result.Height-1 do
	for tavu2 := 0 to Result.Width-1 do
		PCXFile.WriteByte(Result.Pixels[tavu2,tavu1]);}

Done:
	if not OK then FreeAndNil(Result);
	PCXFile.Free;
end;

procedure PCX_ClearBuffer(var image: TPCXImage; val: Byte);
var
	X, Y: Integer;
begin
	for Y := 0 to image.Height-1 do
	for X := 0 to image.Width-1 do
		image.Pixels[X, Y] := val;
end;

procedure PCX_RenderBuffer(var image: TPCXImage; var buffer: TBitmap32);
var
	X, Y: Integer;
begin
	for Y := 0 to image.Height-1 do
	for X := 0 to image.Width-1 do
		buffer.SetPixel(X, Y, image.Palette[image.Pixels[X, Y]]);
end;

end.

