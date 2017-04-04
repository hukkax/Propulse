unit PCX;

{$mode delphi}

interface

uses
	SysUtils, Classes, Graphics32;

type
	TPCXImage = class
	public
		BitsPerPixel,
		Width,
		Height:		Integer;
		Palette:	TPalette32;
		Pixels: 	array of array of Byte;

		constructor Create(W, H: Integer); overload;
		function 	LoadFromFile(const Filename: String): Boolean;
		procedure 	Clear(val: Byte);
		procedure 	DrawTo(var buffer: TBitmap32);
	end;


implementation

uses
	FileStreamEx;


constructor TPCXImage.Create(W, H: Integer);
begin
	inherited Create;
	Width := W;
	Height := H;
	SetLength(Pixels, W, H);
	Clear(0);
end;

function TPCXImage.LoadFromFile(const Filename: String): Boolean;
var
	Bf: RawByteString;
	fsize: Int64;
	b: Byte;
	tavu1, tavu2, y,
	position, bytesperline, bytestoread: Integer;
	PCXFile: TFileStreamEx;
label
	Done;
begin
	Result := False;
	if not FileExists(Filename) then Exit;

	PCXFile := TFileStreamEx.Create(Filename, fmOpenRead, fmShareDenyNone);

	fsize := PCXFile.Size;

	if PCXFile.ReadByte <> $A then goto Done;
	PCXFile.SeekTo(3);
	BitsPerPixel := PCXFile.ReadByte;

	PCXFile.SeekTo(8);
	Width  := PCXFile.ReadWord + 1;
	Height := PCXFile.ReadWord + 1;

	Bf := '';
	SetCodePage(Bf, 1252, False);
	PCXFile.SeekTo(128);
	position := PCXFile.Position;

	case BitsPerPixel of
		1:	bytesperline := Width div 8;
		4:	bytesperline := Width div 2;
	else
		bytesperline := Width;
	end;
	bytestoread := Height * bytesperline;

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

	// read 16-color palette
	PCXFile.SeekTo($10);
	for Y := 0 to 15 do
		Palette[Y] := Color32(
			PCXFile.ReadByte, PCXFile.ReadByte, PCXFile.ReadByte);

	Result := True;
	SetLength(Pixels, Width, Height);
	position := 1;

	case BitsPerPixel of
	1:	begin
			// get pixels
			for tavu1 := 0 to Height-1 do
			for tavu2 := 0 to bytesperline-1 do
			begin
				b := Ord(Bf[position]);
				Inc(position);
				for y := 0 to 7 do
				begin
					if (b and 128) <> 0 then
						Pixels[tavu2*8+y,tavu1] := 1
					else
						Pixels[tavu2*8+y,tavu1] := 0;
					b := b shl 1;
				end;
			end;
		end;

	4:	begin
			// get pixels
			for tavu1 := 0 to Height-1 do
			for tavu2 := 0 to bytesperline-1 do
			begin
				b := Ord(Bf[position]);
				Inc(position);
				Pixels[tavu2*2+0,tavu1] := b shr 4;
				Pixels[tavu2*2+1,tavu1] := b and 15;
			end;
		end;

	8:	begin
			// get pixels
			for tavu1 := 0 to Height-1 do
			for tavu2 := 0 to Width-1 do
			begin
				Pixels[tavu2,tavu1] := Ord(Bf[position]);
				Inc(position);
			end;

			// read palette
			if (fsize > 768) then
			begin
				PCXFile.SeekTo(fsize - 768);
				for Y := 0 to 255 do
					Palette[Y] := Color32(
						PCXFile.ReadByte, PCXFile.ReadByte, PCXFile.ReadByte);
			end;
		end;
	else
		Result := False;
	end;

Done:
	PCXFile.Free;
end;

procedure TPCXImage.Clear(val: Byte);
var
	X, Y: Integer;
begin
	for Y := 0 to Height-1 do
	for X := 0 to Width-1 do
		Pixels[X, Y] := val;
end;

procedure TPCXImage.DrawTo(var buffer: TBitmap32);
var
	X, Y: Integer;
begin
	for Y := 0 to Height-1 do
	for X := 0 to Width-1 do
		buffer.SetPixel(X, Y, Palette[Pixels[X, Y]]);
end;

end.

