unit hkaFileUtils;

{$mode delphi}

interface

uses
	Classes, Types;

type
	TByteArray = array of Byte;
	PByteArray = ^TByteArray;

	TFileAccessor = record
	private
		FPosition: Integer;
		function  GetPosition: Integer;
		procedure SetPosition(const Value: Integer);
	public
		Data: RawByteString;

		property Position: Integer read GetPosition write SetPosition;

		function Load(const Filename: String): Boolean;

		function SeekTo(const NewPosition: Integer; FromEnd: Boolean = False): Integer;
		function Skip(const Amount: Integer): Integer;

		function Read8: Integer;
		function Read16(BigEndian: Boolean = False): Integer; overload;
		function Read24(BigEndian: Boolean = False): Integer; overload;
		function Read32(BigEndian: Boolean = False): Integer; overload;
		function ReadString(Length: Integer = -1; EndChar: Integer = 0): RawByteString;

		function ReadBytes(var Buffer: AnsiString; Size: Cardinal; Index: Word = 1): Cardinal; overload;
		function ReadBytes(var Buffer: Pointer; Size: Cardinal): Cardinal; overload;
		function ReadBytes(Buffer: PByte; Size: Cardinal): Cardinal; overload;
	end;


function CountChars(S: String; C: Char): Word;
function FileToBytes(Filename: String; var Data: TByteArray): Cardinal;
function FileToStringW(Filename: String; Maxlen: Integer = -1): RawByteString;
function FileToString(Filename: String; Maxlen: Integer = -1): RawByteString;
function StringToFileW(Filename: String; const S: RawByteString): Integer;
function StringToFile(Filename: String; const S: RawByteString): Integer;
function GetString(const S: RawByteString; var Pos: Integer;
	SLength: Integer = -1; EndChar: Integer = 0): RawByteString;
function GetString2(const S: RawByteString; Pos: Integer;
	SLength: Integer = -1; EndChar: Integer = 0): RawByteString;

function GetVal16(const S: RawByteString; var Pos: Integer; LittleEndian: Boolean): Integer; overload;
function GetVal24(const S: RawByteString; var Pos: Integer; LittleEndian: Boolean): Integer; overload;
function GetVal32(const S: RawByteString; var Pos: Integer; LittleEndian: Boolean): Integer; overload;
function GetVal8 (const S: RawByteString; var Pos: Integer): Integer;
function GetVal16(const S: RawByteString; var Pos: Integer): Integer; overload;
function GetVal24(const S: RawByteString; var Pos: Integer): Integer; overload;
function GetVal32(const S: RawByteString; var Pos: Integer): Integer; overload;
function GetVal16R(const S: RawByteString; var Pos: Integer): Integer;
function GetVal24R(const S: RawByteString; var Pos: Integer): Integer;
function GetVal32R(const S: RawByteString; var Pos: Integer): Integer;
function GetByte (const S: RawByteString; Pos: Integer): Byte;
function GetWord (const S: RawByteString; Pos: Integer): Word;
function GetDWord(const S: RawByteString; Pos: Integer): DWord;
function PutString(var S: RawByteString; Pos: Integer;
	SPut: RawByteString; Nullpad: Boolean = False): Integer;
function PutStringPadded(var S: RawByteString; Pos: Integer;
	SPut: RawByteString; SLength: Word): Integer;
function PutVal8 (var S: RawByteString; Pos: Integer; Val: Integer): Integer;
function PutVal16(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
function PutVal16R(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
function PutVal24(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
function PutVal32(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
function PutVal32R(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
function AddVal8 (var S: RawByteString; Val: Integer): Integer;
function AddVal16(var S: RawByteString; Val: Integer): Integer;
function AddVal16R(var S: RawByteString; Val: Integer): Integer;
function AddVal24(var S: RawByteString; Val: Integer): Integer;
function AddVal32(var S: RawByteString; Val: Integer): Integer;
function AddVal32R(var S: RawByteString; Val: Integer): Integer;
function BitSet(I, Bit: Byte; One: Boolean = True): Byte;
function BitGet(I, Bit: Byte): Boolean;
function ListFiles(const Mask: String; Subdirs: Boolean = False): TStrings;


implementation

uses SysUtils, Math;


function ListFiles(const Mask: String; Subdirs: Boolean = False): TStrings;
var
	Search: TSearchRec;
	Directory: String;
begin
	Result := TStringList.Create;

	Directory := ExtractFilePath(Mask);

	// Find all files
	if FindFirst(Mask, $23, Search) = 0 then
	begin
		repeat
			Result.Add(Directory + Search.name);
		until FindNext(Search) <> 0;
	end;

	// Subdirectories
	if Subdirs then
	if FindFirst(Directory + '*.*', FaDirectory, Search) = 0 then
	begin
		repeat
			if ((Search.Attr and FaDirectory) = FaDirectory) and
				(Search.Name[1] <> '.') then
					Result.AddStrings(
						ListFiles(Directory + Search.name + PathDelim +
							ExtractFileName(Mask)) );
		until FindNext(Search) <> 0;
	end;

	FindClose(Search);
end;


function BitSet(I, Bit: Byte; One: Boolean = True): Byte;
begin
	if One then
		Result := I or (1 shl Bit)
	else
		Result := I and not (1 shl Bit);
// Result := Value or (1 shl (Bit mod BitsPerInteger));
end;


function BitGet(I, Bit: Byte): Boolean;
begin
	Result := ((I shr Bit) and 1) <> 0;
end;


function CountChars(S: String; C: Char): Word;
var
	i: Integer;
begin
	Result := 0;
	for i := 1 to Length(S) do
		if S[i] = C then Inc(Result);
end;


function FileToBytes(Filename: String; var Data: TByteArray): Cardinal;
var
	FStrm: TFileStream;
begin
	Result := 0;

	if PathDelim = '\' then
		Filename := StringReplace(Filename, '/', '\', [rfReplaceAll]);

	if (Filename = '') or (not FileExists(Filename)) then Exit;
	try
		FStrm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
	except
		on EFOpenError do Exit;
	end;
	try
		SetLength(Data, FStrm.Size);
		FStrm.Read(Data[0], FStrm.Size);
		Result := FStrm.Size;
	finally
		FStrm.Free;
	end;
end;


function FileToStringW(Filename: String; Maxlen: Integer = -1): RawByteString;
var
	FStrm: TFileStream;
begin
	Result := '';

	if PathDelim = '\' then
		Filename := StringReplace(Filename, '/', '\', [rfReplaceAll]);

	if (Filename = '') or (not FileExists(Filename)) then Exit;
	try
		FStrm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
	except
		on EFOpenError do Exit;
	end;
	try
		if Maxlen < 0 then Maxlen := FStrm.Size;
		SetLength(Result, Min(Maxlen, FStrm.Size));
		SetCodePage(Result, 1252, False);
		FStrm.Read(Result[1], Min(Maxlen, FStrm.Size));
	finally
		FStrm.Free;
	end;
end;

function FileToString(Filename: String; Maxlen: Integer = -1): RawByteString; inline;
begin
	Result := FileToStringW(Filename, Maxlen);
end;


function StringToFileW(Filename: String; const S: RawByteString): Integer;
var
	OutStream: TMemoryStream;
begin
	OutStream := TMemoryStream.Create;
	try
		Result := Length(S);
		OutStream.Write(Pointer(S)^, Length(S));
		OutStream.SaveToFile(Filename);
	except
		on E : Exception do
			Result := 0;// - Abs(GetLastError);
	end;
	OutStream.Free;
end;

function StringToFile(Filename: String; const S: RawByteString): Integer; inline;
begin
	Result := StringToFileW(Filename, S);
end;

function GetVal16(const S: RawByteString; var Pos: Integer; LittleEndian: Boolean): Integer; inline;
begin
	if LittleEndian then
		Result := GetVal16(S, Pos)
	else
		Result := GetVal16R(S, Pos);
end;

function GetVal24(const S: RawByteString; var Pos: Integer; LittleEndian: Boolean): Integer; inline;
begin
	if LittleEndian then
		Result := GetVal24(S, Pos)
	else
		Result := GetVal24R(S, Pos);
end;

function GetVal32(const S: RawByteString; var Pos: Integer; LittleEndian: Boolean): Integer; inline;
begin
	if LittleEndian then
		Result := GetVal32(S, Pos)
	else
		Result := GetVal32R(S, Pos);
end;

function GetVal8(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if Pos > Length(S) then Result := 0 else
		Result := Ord(S[Pos]);
	Inc(Pos);
end;

function GetVal16(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 1) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos+1]) shl 8) + Ord(S[Pos]);
	Inc(Pos, 2);
end;

function GetVal24(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 2) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos+2]) shl 16) +
		(Ord(S[Pos+1]) shl 8) + Ord(S[Pos]);
	Inc(Pos, 3);
end;

function GetVal32(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 3) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos+3]) shl 24) + (Ord(S[Pos+2]) shl 16) +
		(Ord(S[Pos+1]) shl 8) + Ord(S[Pos]);
	Inc(Pos, 4);
end;

function GetVal16R(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 1) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos]) shl 8) + Ord(S[Pos+1]);
	Inc(Pos, 2);
end;

function GetVal24R(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 2) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos]) shl 16) +
		(Ord(S[Pos+1]) shl 8) + Ord(S[Pos+2]);
	Inc(Pos, 3);
end;

function GetVal32R(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 3) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos]) shl 24) + (Ord(S[Pos+1]) shl 16) +
		(Ord(S[Pos+2]) shl 8) + Ord(S[Pos+3]);
	Inc(Pos, 4);
end;


function GetByte(const S: RawByteString; Pos: Integer): Byte;
begin
	if Pos = 0 then Pos := 1;
	if Pos > Length(S) then Result := 0 else
		Result := Ord(S[Pos]);
end;

function GetWord(const S: RawByteString; Pos: Integer): Word;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 1) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos+1]) shl 8) + Ord(S[Pos]);
end;

function GetDWord(const S: RawByteString; Pos: Integer): DWord;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 3) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos+3]) shl 24) + (Ord(S[Pos+2]) shl 16) +
		(Ord(S[Pos+1]) shl 8) + Ord(S[Pos]);
end;

function GetVal16_R(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 1) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos]) shl 8) + Ord(S[Pos+1]);
	Inc(Pos, 2);
end;

function GetVal24_R(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 2) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos]) shl 16) +
		(Ord(S[Pos+1]) shl 8) + Ord(S[Pos+2]);
	Inc(Pos, 3);
end;

function GetVal32_R(const S: RawByteString; var Pos: Integer): Integer;
begin
	if Pos = 0 then Pos := 1;
	if (Pos + 3) > Length(S) then Result := 0 else
		Result := (Ord(S[Pos]) shl 24) + (Ord(S[Pos+1]) shl 16) +
		(Ord(S[Pos+2]) shl 8) + Ord(S[Pos+3]);
	Inc(Pos, 4);
end;

function GetString(const S: RawByteString; var Pos: Integer;
	SLength: Integer = -1; EndChar: Integer = 0): RawByteString;
begin
	// read N bytes:             SLength :=  N,  EndChar := -1
	// read N bytes or until #0: SLength :=  N;  EndChar :=  0
	// read S[1] bytes:          SLength := -2;  EndChar := -1
	Result := '';
	if SLength >= 0 then
		SLength := SLength + Pos           // read max. N bytes
	else
	if SLength = -1 then
		SLength := Length(S)               // read until NULL/EOF
	else
	if SLength = -2 then
		SLength := Pos + GetVal8(S, Pos)   // length told in first Byte
	else
	if SLength = -3 then
		SLength := Pos + GetVal16(S, Pos); // length told in first Word
	if SLength = -4 then
		SLength := Pos + GetVal32(S, Pos); // length told in first DWord
	if SLength < -1 then
		EndChar := -1;                     // until EOF
	while (Pos < SLength) and (Pos < Length(S)) do
	begin
		if Ord(S[Pos]) = EndChar then
		begin
			Inc(Pos);
			Break;
		end;
		Result := Result + S[Pos];
		Inc(Pos);
	end;
end;

{ like above, but Pos is not var }
function GetString2(const S: RawByteString; Pos: Integer;
	SLength: Integer = -1; EndChar: Integer = 0): RawByteString;
var
	p: Integer;
begin
	p := Pos;
	Result := GetString(S, p, SLength, EndChar);
end;

function PutVal8(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
begin
	if Pos < 1 then Pos := 1;
	if Length(S) < Pos then SetLength(S, Pos);
	S[Pos] := AnsiChar(Val and $FF);
	Result := Pos + 1;
end;

function PutVal16(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
begin
	if Pos < 1 then Pos := 1;
	if Length(S) < (Pos+1) then SetLength(S, Pos+1);
	S[Pos]   := AnsiChar (Val and $FF);
	S[Pos+1] := AnsiChar((Val shr 8) and $FF);
	Result := Pos + 2;
end;

function PutVal16R(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
begin
	if Length(S) < (Pos+1) then SetLength(S, Pos+1);
	S[Pos]   := AnsiChar((Val and $FF00) shr 8);
	S[Pos+1] := AnsiChar (Val and $FF);
	Result := Pos + 2;
end;

function PutVal24(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
begin
	if Length(S) < (Pos+2) then SetLength(S, Pos+2);
	S[Pos]   := AnsiChar (Val and $FF);
	S[Pos+1] := AnsiChar((Val shr 8)  and $FF);
	S[Pos+2] := AnsiChar((Val shr 16) and $FF);
	Result := Pos + 3;
end;

function PutVal32(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
begin
	if Length(S) < (Pos+3) then SetLength(S, Pos+3);
	S[Pos]   := AnsiChar (Val and $FF);
	S[Pos+1] := AnsiChar((Val shr 8)  and $FF);
	S[Pos+2] := AnsiChar((Val shr 16) and $FF);
	S[Pos+3] := AnsiChar((Val shr 24) and $FF);
	Result := Pos + 4;
end;

function PutVal32R(var S: RawByteString; Pos: Integer; Val: Integer): Integer;
begin
	if Length(S) < (Pos+3) then SetLength(S, Pos+3);

	S[Pos]   := AnsiChar((Val shr 24) and $FF);
	S[Pos+1] := AnsiChar((Val shr 16) and $FF);
	S[Pos+2] := AnsiChar((Val shr 8)  and $FF);
	S[Pos+3] := AnsiChar (Val and $FF);

	Result := Pos + 4;
end;

function AddVal8(var S: RawByteString; Val: Integer): Integer;
begin
	Result := PutVal8(S, Length(S)+1, Val);
end;
function AddVal16(var S: RawByteString; Val: Integer): Integer;
begin
	Result := PutVal16(S, Length(S)+1, Val);
end;
function AddVal16R(var S: RawByteString; Val: Integer): Integer;
begin
	Result := PutVal16R(S, Length(S)+1, Val);
end;
function AddVal24(var S: RawByteString; Val: Integer): Integer;
begin
	Result := PutVal24(S, Length(S)+1, Val);
end;
function AddVal32(var S: RawByteString; Val: Integer): Integer;
begin
	Result := PutVal32(S, Length(S)+1, Val);
end;
function AddVal32R(var S: RawByteString; Val: Integer): Integer;
begin
	Result := PutVal32R(S, Length(S)+1, Val);
end;


function PutString(var S: RawByteString; Pos: Integer;
	SPut: RawByteString; Nullpad: Boolean = False): Integer;
var
	N: Integer;
begin
	Result := 0;
	//if Nullpad then SPut := SPut + #0;
	if SPut = '' then
		Result := Pos
	else
		for N := 1 to Length(SPut) do
			Result := PutVal8(S, Pos+N-1, Ord(SPut[N]));
	if Nullpad then
		Result := PutVal8(S, Result, 0);
end;

function PutStringPadded(var S: RawByteString; Pos: Integer;
	SPut: RawByteString; SLength: Word): Integer;
var
	N: Integer;
begin
	Result := 0;
	if SLength < Length(SPut) then
		SPut := Copy(SPut, 1, SLength)
	else
		while Length(SPut) < SLength do
			SPut := SPut + ' ';
	for N := 1 to Length(SPut) do
		Result := PutVal8(S, Pos+N-1, Ord(SPut[N]));
end;


{ TFileAccessor }

function TFileAccessor.Load(const Filename: String): Boolean;
begin
	Data := FileToString(Filename);
	Result := (Length(Data) > 0);
	if Result then
		SeekTo(0);
end;

function TFileAccessor.ReadString(Length: Integer = -1; EndChar: Integer = 0): RawByteString;
begin
	Result := GetString(Data, FPosition, Length, EndChar);
end;

function TFileAccessor.GetPosition: Integer;
begin
	Result := FPosition - 1;
end;

procedure TFileAccessor.SetPosition(const Value: Integer);
begin
	FPosition := Value + 1;
end;

function TFileAccessor.Read8: Integer;
begin
	Result := GetVal8(Data, FPosition);
end;

function TFileAccessor.Read16(BigEndian: Boolean): Integer;
begin
	Result := GetVal16(Data, FPosition, not BigEndian);
end;

function TFileAccessor.Read24(BigEndian: Boolean): Integer;
begin
	Result := GetVal24(Data, FPosition, not BigEndian);
end;

function TFileAccessor.Read32(BigEndian: Boolean): Integer;
begin
	Result := GetVal32(Data, FPosition, not BigEndian);
end;

function TFileAccessor.ReadBytes(var Buffer: AnsiString; Size: Cardinal;
	Index: Word = 1): Cardinal;
var
	i, len: Integer;
begin
	for i := 0 to Size-1 do
	begin
		if FPosition > Length(Data) then Exit;
		Buffer[i+Index] := Data[FPosition];
		Inc(FPosition);
		Inc(Result);
	end;
end;

function TFileAccessor.ReadBytes(Buffer: PByte; Size: Cardinal): Cardinal;
var
	i, len: Integer;
begin
	for i := 0 to Size-1 do
	begin
		if FPosition > Length(Data) then Exit;
		Buffer^ := Ord(Data[FPosition]);
		Inc(Buffer);
		Inc(FPosition);
		Inc(Result);
	end;
end;

function TFileAccessor.ReadBytes(var Buffer: Pointer; Size: Cardinal): Cardinal;
var
	len: Cardinal;
	Src: Pointer;
begin
	len := Min(FPosition+Size, Length(Data)) - FPosition;
	Src := @Data[FPosition];
	Move(Src^, Buffer, len);
	Inc(FPosition, len);
	Inc(Result, len);
end;

function TFileAccessor.SeekTo(const NewPosition: Integer; FromEnd: Boolean): Integer;
begin
	if not FromEnd then
		FPosition := NewPosition + 1
	else
		FPosition := Length(Data) - NewPosition;

	if FPosition < 1 then
		FPosition := 1
	else
	if FPosition > Length(Data) then
		FPosition := Length(Data);

	Result := Position;
end;

function TFileAccessor.Skip(const Amount: Integer): Integer;
begin
	FPosition := FPosition + Amount;
	Result := Position;
end;

end.


