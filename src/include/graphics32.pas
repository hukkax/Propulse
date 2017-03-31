unit Graphics32;

// minimal version of Graphics32, adapted from
// Gr32.pas and Gr32_LowLevel.pas

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *   J. Tulach <tulach at position.cz>
 *   Jouni Airaksinen <markvera at spacesynth.net>
 *   Timothy Weber <teejaydub at users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{$mode delphi}

interface

uses
	Classes, SysUtils, Graphics32_LowLevel;

type
	TColor32 = UInt32;
	PColor32 = ^TColor32;

	TColor32Array = array of TColor32;
	PColor32Array = ^TColor32Array;

	{ TBitmap32 }
	TBitmap32 = class
	private
		Raster:		TPoint;

     function 	GetPixelPtr(X, Y: Integer): PColor32;
	public
		Width,
		Height: 	Cardinal;
		BoundsRect,
		ClipRect:	TRect;
		Bits:		array of TColor32;

		procedure 	LoadFromFile(const Filename: String);
		procedure 	SaveToFile(const Filename: String);

		function	SaneCoords(X1, Y1, X2, Y2: Integer): Boolean; inline;
		function	ValidateCoords(var X1, Y1, X2, Y2: Integer): Boolean; inline;
		function 	ValidateX(var X: Integer): Boolean; inline;
		function 	ValidateY(var Y: Integer): Boolean; inline;
		procedure	SetSize(W, H: Cardinal);
		procedure 	MoveTo(X, Y: Integer); inline;
		procedure 	ResetClipRect;

		procedure 	Draw(DstX, DstY: Integer; Src: TBitmap32); overload;
		procedure 	Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TBitmap32); overload;
		procedure 	DrawColorKey(DstX, DstY: Integer; TransColor: TColor32; Src: TBitmap32); overload;
		procedure 	DrawColorKey(DstX, DstY: Integer;
					const SrcRect: TRect; TransColor: TColor32; Src: TBitmap32); overload;
		procedure 	FillRect(R: TRect; Value: TColor32); overload;
		procedure 	FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
        procedure 	FillRectS(R: TRect; Value: TColor32); overload;
		procedure 	FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
		procedure 	FrameRect(R: TRect; Value: TColor32); overload;
		procedure 	FrameRect(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
		procedure 	Clear(FillColor: TColor32);
		procedure 	HorzLine(X1, Y, X2: Integer; Value: TColor32);
		procedure 	VertLine(X, Y1, Y2: Integer; Value: TColor32);
		procedure 	Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
		procedure 	LineTo(X, Y: Integer; PenColor: TColor32);

		procedure	SetPixel(X, Y: Integer; Value: TColor32); inline;
		function	GetPixel(X, Y: Integer): TColor32; inline;

		property	Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
	    property 	PixelPtr[X, Y: Integer]: PColor32 read GetPixelPtr;

		constructor Create;
	end;

	function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; inline;
	function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32; inline;
	function RedComponent(Color: TColor32): Byte; inline;
	function GreenComponent(Color: TColor32): Byte; inline;
	function BlueComponent(Color: TColor32): Byte; inline;


implementation

uses
	Math,
	hkaFileUtils,
	PCX;

{ ============================================================================}
{ Utility }
{ ============================================================================}

// Result := (A shl 24) or (R shl 16) or (G shl  8) or B;
function Color32(R, G, B: Byte; A: Byte = $FF): TColor32;
{$IFDEF CPU32}
asm
	MOV		AH, A
	SHL		EAX, 16
	MOV		AH, DL
	MOV		AL, CL
end;
{$ELSE}
begin
	Result := $FF000000 + (R shl 16) + (G shl 8) + B;
end;
{$ENDIF}

function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;
begin
	Result := Alpha shl 24 + Intensity shl 16 +
		Intensity shl 8 + Intensity;
end;

function RedComponent(Color: TColor32): Byte;
begin
	Result := (Color and $FF0000) shr 16;
end;
function GreenComponent(Color: TColor32): Byte;
begin
	Result := (Color and $00FF00) shr 8;
end;
function BlueComponent(Color: TColor32): Byte;
begin
	Result := Color and $0000FF;
end;

{ ============================================================================}
{ TBitmap32 }
{ ============================================================================}

function TBitmap32.SaneCoords(X1, Y1, X2, Y2: Integer): Boolean;
begin
	Result :=
		(X2 > X1) and (Y2 > Y1) and
		(X2 <= ClipRect.Right)  and (Y2 <= ClipRect.Bottom) and
		(X1 >= ClipRect.Left)   and (Y1 >= ClipRect.Top);
end;

function TBitmap32.ValidateX(var X: Integer): Boolean;
begin
	Result := False;
	if X < ClipRect.Left   then X := ClipRect.Left
	else
	if X >= ClipRect.Right then X := ClipRect.Right-1
	else
		Result := True;
end;

function TBitmap32.ValidateY(var Y: Integer): Boolean;
begin
	Result := False;
	if Y < ClipRect.Top     then Y := ClipRect.Top
	else
	if Y >= ClipRect.Bottom then Y := ClipRect.Bottom-1
	else
		Result := True;
end;

function TBitmap32.ValidateCoords(var X1, Y1, X2, Y2: Integer): Boolean;
var
	Z: Integer;
begin
	if X1 < ClipRect.Left   then X1 := ClipRect.Left;
	if Y1 < ClipRect.Top    then Y1 := ClipRect.Top;
	if X2 > ClipRect.Right  then X2 := ClipRect.Right;
	if Y2 > ClipRect.Bottom then Y2 := ClipRect.Bottom;
{	if X1 > X2 then
	begin
		Z := X1;
		X1 := X2;
		X2 := Z;
	end;
	if Y1 > Y2 then
	begin
		Z := Y1;
		Y1 := Y2;
		Y2 := Z;
	end;}
	Result := (X1 < X2) and (Y1 < Y2);
end;

procedure TBitmap32.SetSize(W, H: Cardinal);
begin
	Width  := W;
	Height := H;
	SetLength(Bits, W*H);
	ResetClipRect;
end;

function TBitmap32.GetPixelPtr(X, Y: Integer): PColor32;
begin
	Result := @Bits[X + Y * Width];
end;

function TBitmap32.GetPixel(X, Y: Integer): TColor32;
begin
	Result := Bits[X + Y * Width];
end;

constructor TBitmap32.Create;
begin
	ResetClipRect;
end;

procedure TBitmap32.SetPixel(X, Y: Integer; Value: TColor32);
begin
	Bits[X + Y * Width] := Value;
end;

procedure TBitmap32.LoadFromFile(const Filename: String);
var
	Bm: TPCXImage;
begin
	Bm := PCX_LoadImage(Filename);
	if Bm <> nil then
	begin
		SetSize(Bm.Width, Bm.Height);
		PCX_RenderBuffer(Bm, Self);
		Bm.Free;
	end;
end;

procedure TBitmap32.SaveToFile(const Filename: String);
var
	S: RawByteString;
	X, Y: Integer;
begin
	S := '';
	for Y := 0 to Height-1 do
	begin
		for X := 0 to Width-1 do
			if (Pixel[X,Y] and $FFFFFF) = 0 then
				S := S + '.'
			else
				S := S + '#';
		S := S + #13;
	end;
	StringToFile(Filename, S);
end;

procedure TBitmap32.Clear(FillColor: TColor32);
begin
	FillLongword(Bits[0], Width * Height, FillColor);
end;

procedure TBitmap32.FillRect(R: TRect; Value: TColor32);
begin
	FillRectS(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
	y: Integer;
begin
	for y := Y1 to Y2-1 do
		FillLongWord(Bits[y * Width + x1], X2 - X1, Value);
end;

procedure TBitmap32.FillRectS(R: TRect; Value: TColor32);
begin
	FillRectS(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
	if (X2 > X1) and (Y2 > Y1) and (X1 < ClipRect.Right) and (Y1 < ClipRect.Bottom) then
	begin
		if ValidateCoords(X1, Y1, X2, Y2) then
			FillRect(X1, Y1, X2, Y2, Value);
	end;
end;

procedure TBitmap32.FrameRect(R: TRect; Value: TColor32);
begin
	FrameRect(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.FrameRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
	if SaneCoords(X1, Y1, X2, Y2) then
	begin
		Dec(Y2); Dec(X2);
		HorzLine(X1, Y1, X2, Value);
		if Y2 > Y1 then
			HorzLine(X1, Y2, X2, Value);
		if Y2 > Y1 + 1 then
		begin
			VertLine(X1, Y1+1, Y2-1, Value);
			if X2 > X1 then
				VertLine(X2, Y1+1, Y2-1, Value);
		end;
	end;
end;

procedure TBitmap32.HorzLine(X1, Y, X2: Integer; Value: TColor32);
begin
	if ValidateY(Y) then
	begin
		ValidateX(X1); ValidateX(X2);
		FillLongWord(Bits[X1 + Y * Width], X2 - X1 + 1, Value);
	end;
end;

procedure TBitmap32.VertLine(X, Y1, Y2: Integer; Value: TColor32);
var
	I, NH, NL: Integer;
	P: PColor32;
begin
	if ValidateX(X) then
	begin
		ValidateY(Y1); ValidateY(Y2);
		if Y2 < Y1 then Exit;
		P := PixelPtr[X, Y1];
		I := Y2 - Y1 + 1;
		NH := I shr 2;
		NL := I and $03;
		for I := 0 to NH - 1 do
		begin
			P^ := Value; Inc(P, Width);
			P^ := Value; Inc(P, Width);
			P^ := Value; Inc(P, Width);
			P^ := Value; Inc(P, Width);
		end;
		for I := 0 to NL - 1 do
		begin
			P^ := Value; Inc(P, Width);
		end;
	end;
end;

procedure TBitmap32.Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
	Dy, Dx, Sy, Sx, I, Delta: Integer;
	P: PColor32;
begin
	ValidateCoords(X1, Y1, X2, Y2);
	Dx := X2 - X1;
	Dy := Y2 - Y1;
	if Dx > 0 then
		Sx := 1
	else
	if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then VertLine(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLine(X1, Y2 + 1, Y1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    if Dy > 0 then Sy := 1
    else if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if X2 > X1 then HorzLine(X1, Y1, X2 - 1, Value)
      else HorzLine(X2 + 1, Y1, X1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
    begin
      Delta := Dx shr 1;
      for I := 0 to Dx - 1 do
      begin
        P^ := Value;
        Inc(P, Sx);
        Inc(Delta, Dy);
        if Delta >= Dx then
        begin
          Inc(P, Sy);
          Dec(Delta, Dx);
        end;
      end;
    end
    else // Dx < Dy
    begin
      Delta := Dy shr 1;
      for I := 0 to Dy - 1 do
      begin
        P^ := Value;
        Inc(P, Sy);
        Inc(Delta, Dx);
        if Delta >= Dy then
        begin
          Inc(P, Sx);
          Dec(Delta, Dy);
        end;
      end;
    end;
    if L then P^ := Value;
end;

procedure TBitmap32.LineTo(X, Y: Integer; PenColor: TColor32);
begin
	Line(Raster.X, Raster.Y, X, Y, PenColor, False);
	MoveTo(X, Y);
end;

procedure TBitmap32.MoveTo(X, Y: Integer);
begin
	Raster.X := X;
	Raster.Y := Y;
end;

procedure TBitmap32.ResetClipRect;
begin
	ClipRect   := Rect(0, 0, Width, Height);
	BoundsRect := Rect(0, 0, Width, Height);
end;

procedure TBitmap32.Draw(DstX, DstY: Integer; Src: TBitmap32);
var
	SrcP, DstP: PColor32;
	W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	ValidateX(DstX);

	W  := Min(Src.Width, Self.Width);
	if W <= 0 then Exit;
	if DstX+W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	Y2 := Min(Self.Height, Y1+Src.Height);

	SrcP := Src.PixelPtr[0,0];
	DstP := Self.PixelPtr[DstX, Y1];

	try
		for Y := Y1 to Y2-1 do
		begin
			MoveLongWord(SrcP^, DstP^, W);
			Inc(SrcP, Src.Width);
			Inc(DstP, Width);
		end;
	finally
		// not super sure if this is necessary
		asm
			EMMS;
		end;
	end;
end;

procedure TBitmap32.Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TBitmap32);
var
	SrcP, DstP: PColor32;
	W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	ValidateX(DstX);

	W := Min(Src.Width, SrcRect.Right - SrcRect.Left);
	W := Min(W, Self.Width);
	if W <= 0 then Exit;
	if DstX+W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	Y  := Min(Src.Height, SrcRect.Bottom - SrcRect.Top);
	Y2 := Min(Self.Height, Y1+Y);

	SrcP := Src.PixelPtr[SrcRect.Left, SrcRect.Top];
	DstP := Self.PixelPtr[DstX, Y1];

	try
		for Y := Y1 to Y2-1 do
		begin
			MoveLongWord(SrcP^, DstP^, W);
			Inc(SrcP, Src.Width);
			Inc(DstP, Width);
		end;
	finally
		// not super sure if this is necessary
		asm
			EMMS;
		end;
	end;
end;

procedure TBitmap32.DrawColorKey(DstX, DstY: Integer;
	TransColor: TColor32; Src: TBitmap32);
var
	SP, DP, SrcP, DstP: PColor32;
	X, W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	ValidateX(DstX);

	W  := Min(Src.Width, Self.Width);
	if W <= 0 then Exit;
	if DstX+W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	Y2 := Min(Self.Height, Y1+Src.Height);

	SrcP := Src.PixelPtr[0,0];
	DstP := Self.PixelPtr[DstX, Y1];

	for Y := Y1 to Y2-1 do
	begin
		SP := SrcP;
		DP := DstP;
		for X := 0 to W-1 do
		begin
			if TransColor <> SP^ then DP^ := SP^;
			Inc(SP); Inc(DP);
		end;
		Inc(SrcP, Src.Width);
		Inc(DstP, Width);
	end;
end;

procedure TBitmap32.DrawColorKey(DstX, DstY: Integer;
	const SrcRect: TRect; TransColor: TColor32; Src: TBitmap32);
var
	SP, DP, SrcP, DstP: PColor32;
	X, W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	ValidateX(DstX);

	W := Min(Src.Width, SrcRect.Right - SrcRect.Left);
	W := Min(W, Self.Width);
	if W <= 0 then Exit;
	if DstX+W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	Y  := Min(Src.Height, SrcRect.Bottom - SrcRect.Top);
	Y2 := Min(Self.Height, Y1+Y);

	SrcP := Src.PixelPtr[SrcRect.Left, SrcRect.Top];
	DstP := Self.PixelPtr[DstX, Y1];

	for Y := Y1 to Y2-1 do
	begin
		SP := SrcP;
		DP := DstP;
		for X := 0 to W-1 do
		begin
			if TransColor <> SP^ then DP^ := SP^;
			Inc(SP); Inc(DP);
		end;
		Inc(SrcP, Src.Width);
		Inc(DstP, Width);
	end;
end;

end.

