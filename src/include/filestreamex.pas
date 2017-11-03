unit FileStreamEx;

// Simple class to simplify file access, by hukka 2017-03-31
// methods ending in R read/write big-endian values

{$mode delphi}

interface

uses
	Classes, SysUtils;

type
	{ TByteDataReader }
	TByteDataReader = class
	private
		Pos,
		Size:		QWord;
	public
		Data:		array of Byte;

		function	LoadFromFile(const Filename: String): Int64;
		function	LoadFromStream(const Stream: TStream): Int64;

		function	Read8:		Byte; inline;
		function	Read16:		Word; inline;
		function	Read16R:	Word; inline;
		function	Read24:		Cardinal; inline;
		function	Read24R:	Cardinal; inline;
		function	Read32:		Cardinal; inline;
		function	Read32R:	Cardinal; inline;

		procedure	Skip(Len: Int64); inline;
		procedure	SeekTo(Offset: Int64); inline;

		property 	Position: QWord read Pos;
	end;

	{ TFileStreamEx }
	TFileStreamEx = class(TFileStream)
	public
		Bytes:		TByteDataReader;

		function	Read8:		Byte; inline;
		function	Read16:		Word; inline;
		function	Read16R:	Word; inline;
		function	Read24:		Cardinal; inline;
		function	Read24R:	Cardinal; inline;
		function	Read32:		Cardinal; inline;
		function	Read32R:	Cardinal; inline;
		function	Read64:		QWord; inline;
		function	Read64R:	QWord; inline;
		function	ReadString(StopAtZero: Boolean;
					MaxLen: Word = 0): AnsiString; overload;
		function 	ReadData: Int64;

		procedure	Write8  (Val: Byte); overload; inline;
		procedure	Write8  (Val: Char); overload; inline;
		procedure	Write16 (Val: Word); inline;
		procedure	Write16R(Val: Word); inline;
		procedure	Write32 (Val: Cardinal); inline;
		procedure	Write32R(Val: Cardinal); inline;
		procedure	Write64 (Val: QWord); inline;
		procedure	Write64R(Val: QWord); inline;
		procedure	WriteString(const Val: AnsiString);
		procedure	WriteData;

		procedure	Skip(Len: Int64); inline;
		procedure	SeekTo(Offset: Int64); inline;

		destructor  Destroy; override;
	end;


	function FileToString(const FileName: String): AnsiString;


implementation

uses Math;


function FileToString(const FileName: String): AnsiString;
var
	Stream: TFileStream;
begin
	Stream := TFileStream.Create(FileName, fmOpenRead);
	try
		SetLength(Result, Stream.Size);
		Stream.ReadBuffer(Pointer(Result)^, Length(Result));
	finally
		Stream.Free;
	end;
end;

{ TByteDataReader }

function TByteDataReader.LoadFromFile(const Filename: String): Int64;
var
	Stream: TFileStream;
begin
	Stream := TFileStream.Create(FileName, fmOpenRead);
	Result := LoadFromStream(Stream);
	Stream.Free;
end;

function TByteDataReader.LoadFromStream(const Stream: TStream): Int64;
var
	P: Int64;
begin
	Size := Stream.Size;
	P := Stream.Position;
	SetLength(Data, Size);
	Stream.Seek(0, soFromBeginning);
	Stream.Read(Data[0], Size);
	Stream.Seek(P, soFromBeginning);
	Pos := 0;
	Result := Size;
end;

function TByteDataReader.Read8: Byte;
begin
	if Pos > Size then
		Result := 0
	else
		Result := Data[Pos];
	Skip(1);
end;

function TByteDataReader.Read16: Word;
begin
	if (Pos+1) > Size then
		Result := 0
	else
		Result := Data[Pos+1] shl 8 + Data[Pos];
	Skip(2);
end;

function TByteDataReader.Read16R: Word;
begin
	if (Pos+1) > Size then
		Result := 0
	else
		Result := Data[Pos] shl 8 + Data[Pos+1];
	Skip(2);
end;

function TByteDataReader.Read24: Cardinal;
begin
	if (Pos+2) > Size then
		Result := 0
	else
		Result := Data[Pos+2] shl 16 + Data[Pos+1] shl 8 + Data[Pos];
	Skip(3);
end;

function TByteDataReader.Read24R: Cardinal;
begin
	if (Pos+2) > Size then
		Result := 0
	else
		Result := Data[Pos] shl 16 + Data[Pos+1] shl 8 + Data[Pos+2];
	Skip(3);
end;

function TByteDataReader.Read32: Cardinal;
begin
	if (Pos+3) > Size then
		Result := 0
	else
		Result := Data[Pos+3] shl 24 + Data[Pos+2] shl 16 + Data[Pos+1] shl 8 + Data[Pos];
	Skip(4);
end;

function TByteDataReader.Read32R: Cardinal;
begin
	if (Pos+3) > Size then
		Result := 0
	else
		Result := Data[Pos] shl 24 + Data[Pos+1] shl 16 + Data[Pos+2] shl 8 + Data[Pos+3];
	Skip(4);
end;

procedure TByteDataReader.Skip(Len: Int64);
begin
	Pos := Min(Size, Pos + Len);
end;

procedure TByteDataReader.SeekTo(Offset: Int64);
begin
	Pos := Min(Size, Offset);
end;

{ TFileStreamEx }

// ============================================================================
// Read
// ============================================================================

function TFileStreamEx.Read8: Byte;
begin
	if Position >= Size then
		Result := 0
	else
		Result := ReadByte;
end;

function TFileStreamEx.Read16: Word;
begin
	Result := LEtoN(ReadWord);
end;

function TFileStreamEx.Read16R: Word;
begin
	Result := BEtoN(ReadWord);
end;

function TFileStreamEx.Read24: Cardinal;
begin
	Result := LEtoN(ReadDWord) and $FFFFFF;
	Seek(-1, soFromCurrent);
end;

function TFileStreamEx.Read24R: Cardinal;
begin
	Result := BEtoN(ReadDWord) and $FFFFFF;
	Seek(-1, soFromCurrent);
end;

function TFileStreamEx.Read32: Cardinal;
begin
	Result := LEtoN(ReadDWord);
end;

function TFileStreamEx.Read32R: Cardinal;
begin
	Result := BEtoN(ReadDWord);
end;

function TFileStreamEx.Read64: QWord;
begin
	Result := LEtoN(ReadQWord);
end;

function TFileStreamEx.Read64R: QWord;
begin
	Result := BEtoN(ReadQWord);
end;

function TFileStreamEx.ReadString(StopAtZero: Boolean;
	MaxLen: Word = 0): AnsiString;
var
	x: Integer;
	B: Byte;
begin
	Result := '';
	if MaxLen = 0 then MaxLen := High(Word);

	for x := 1 to MaxLen do
	begin
		B := ReadByte;
		if (StopAtZero) and (B = 0) then
			Break;
		Result := Result + AnsiChar(B);
	end;
end;

function TFileStreamEx.ReadData: Int64;
{var
	P: Int64;}
begin
	if Assigned(Bytes) then Bytes.Free;
	Bytes := TByteDataReader.Create;
	Bytes.LoadFromStream(Self);
	{Result := Size;
	P := Position;
	SetLength(Data, Result+1);
	SeekTo(0);
	Read(Data[0], Result);
	SeekTo(P);}
end;

// ============================================================================
// Write
// ============================================================================

procedure TFileStreamEx.Write8(Val: Byte);
begin
	WriteByte(Val);
end;

procedure TFileStreamEx.Write8(Val: Char);
begin
	WriteByte(Ord(Val));
end;

procedure TFileStreamEx.Write16(Val: Word);
begin
	WriteWord(NtoLE(Val));
end;

procedure TFileStreamEx.Write16R(Val: Word);
begin
	WriteWord(NtoBE(Val));
end;

procedure TFileStreamEx.Write32(Val: Cardinal);
begin
	WriteDWord(NtoLE(Val));
end;

procedure TFileStreamEx.Write32R(Val: Cardinal);
begin
	WriteDWord(NtoBE(Val));
end;

procedure TFileStreamEx.Write64(Val: QWord);
begin
	WriteQWord(NtoLE(Val));
end;

procedure TFileStreamEx.Write64R(Val: QWord);
begin
	WriteQWord(NtoBE(Val));
end;

procedure TFileStreamEx.WriteString(const Val: AnsiString);
var
	x: Integer;
begin
	for x := 1 to Length(Val) do
		Write8(Val[x]);
end;

procedure TFileStreamEx.WriteData;
begin
	if System.Length(Bytes.Data) > 0 then
		Write(Bytes.Data[0], System.Length(Bytes.Data));
end;

procedure TFileStreamEx.Skip(Len: Int64);
begin
	Seek(Len, soCurrent);
end;

procedure TFileStreamEx.SeekTo(Offset: Int64);
begin
	Seek(Offset, soBeginning);
end;

destructor TFileStreamEx.Destroy;
begin
	if Assigned(Bytes) then
		Bytes.Free;
	inherited;
end;

end.

