unit FileStreamEx;

// Simple class to simplify file access, by hukka 2017-03-31
// methods ending in R read/write big-endian values

{$mode delphi}

interface

uses
	Classes, SysUtils;

type
	{ TFileStreamEx }
	TFileStreamEx = class(TFileStream)
	public
		function	Read8:		Byte; inline;
		function	Read16:		Word; inline;
		function	Read16R:	Word; inline;
		function	Read32:		Cardinal; inline;
		function	Read32R:	Cardinal; inline;
		function	ReadString(StopAtZero: Boolean;
					MaxLen: Word = 0): AnsiString; overload;

		procedure	Write8  (Val: Byte); overload; inline;
		procedure	Write8  (Val: Char); overload; inline;
		procedure	Write16 (Val: Word); inline;
		procedure	Write16R(Val: Word); inline;
		procedure	Write32 (Val: Cardinal); inline;
		procedure	Write32R(Val: Cardinal); inline;
		procedure	WriteString(const Val: AnsiString);
	end;

implementation

{ TFileStreamEx }

// ============================================================================
// Read
// ============================================================================

function TFileStreamEx.Read8: Byte;
begin
	Result := ReadByte;
end;

function TFileStreamEx.Read16: Word;
begin
	Result := BEtoN(ReadWord);
end;

function TFileStreamEx.Read16R: Word;
begin
	Result := LEtoN(ReadWord);
end;

function TFileStreamEx.Read32: Cardinal;
begin
	Result := BEtoN(ReadDWord);
end;

function TFileStreamEx.Read32R: Cardinal;
begin
	Result := LEtoN(ReadDWord);
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
		Result := Result + Chr(B);
	end;
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

procedure TFileStreamEx.WriteString(const Val: AnsiString);
var
	x: Integer;
begin
	for x := 1 to Length(Val) do
		Write8(Val[x]);
end;

end.

