unit PCX;

interface

uses
	SysUtils, Classes, Graphics32;

type
	TPCXImage = class
	public
		BitsPerPixel,
		Width,
		Height:		Cardinal;
		Palette:	TPalette32;
		Pixels: 	packed array of packed array of Byte;

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
	Width  := W;
	Height := H;
	SetLength(Pixels, W, H);
	Clear(0);
end;

function TPCXImage.LoadFromFile(const Filename: String): Boolean;
var
	Bf: array of array of Byte;
	fsize: Int64;
	b: Byte;
	tavu1, tavu2, x, y,
	position, bytesperline: Integer;
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

	SetLength(Bf, Width, Height);
	SetLength(Pixels, Width, Height);

	PCXFile.SeekTo($42);
	bytesperline := PCXFile.ReadWord;
	//bytestoread := Height * bytesperline;
	//linepadding := ((bytesperline * (8 div BitsPerPixel)) - Width) div 8;

	PCXFile.SeekTo(128);
	position := PCXFile.Position;

	for y := 0 to Height-1 do
	begin
		x := 0;
		while x < bytesperline do
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
					Bf[x, y] := tavu2;
					Inc(x);
					Dec(tavu1);
					if x > bytesperline then Break;
				end;
			end
			else
			begin
				Bf[x, y] := tavu1;
				Inc(x);
			end;
		end;
		{if linepadding > 0 then
			PCXFile.Skip(linepadding);}
	end;

	// read 16-color palette
	PCXFile.SeekTo($10);
	for Y := 0 to 15 do
		Palette[Y] := Color32(
			PCXFile.ReadByte, PCXFile.ReadByte, PCXFile.ReadByte);

	Result := True;

	case BitsPerPixel of
	1:	begin
			// get pixels
			for tavu1 := 0 to Height-1 do
			for tavu2 := 0 to bytesperline-1 do
			begin
				b := Bf[tavu2, tavu1];

				for y := 0 to 7 do
				begin
					x := tavu2 * 8 + y;
					if x >= Width then Break;

					if (b and 128) <> 0 then
						Pixels[x,tavu1] := 1
					else
						Pixels[x,tavu1] := 0;

					b := Byte(b shl 1);
					Inc(x);
				end;
			end;
		end;

	4:	begin
			bytesperline := Width div 2;
			// get pixels
			for tavu1 := 0 to Height-1 do
			for tavu2 := 0 to bytesperline-1 do
			begin
				b := Bf[tavu2, tavu1];
				Pixels[tavu2*2+0,tavu1] := b shr 4;
				Pixels[tavu2*2+1,tavu1] := b and 15;
			end;
		end;

	8:	begin
			// get pixels
			for tavu1 := 0 to Height-1 do
			for tavu2 := 0 to Width-1 do
				Pixels[tavu2,tavu1] := Bf[tavu2, tavu1];

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

