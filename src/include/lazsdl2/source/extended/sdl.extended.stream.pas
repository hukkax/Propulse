unit SDL.Extended.Stream;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types;

type

  { TSDLStream }

  TSDLStream = class(TStream)
  private
    FSDL_RWops: SDL_RWops;
    FData: TStream;
    FOwnsStream: Boolean;
    function GetSDL_RWops: PSDL_RWops;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override; overload;
  public
    constructor Create(const Data: TStream; OwnsStream: Boolean); reintroduce;
    procedure BeforeDestruction; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override; overload;

    property SDL_RWops: PSDL_RWops read GetSDL_RWops;
  end;

implementation

function RW_Size(context: PSDL_RWops): SDL_SInt64; cdecl;
var
  FStream: TSDLStream;
begin
  FStream := TSDLStream(context^.hidden.unknown.data1);
  Result := FStream.Size;
end;

function RW_Seek(context: PSDL_RWops; offset: SDL_SInt64; whence: SDL_Rwops_seek): SDL_SInt64; cdecl;
var
  FStream: TSDLStream;
  Orgin: TSeekOrigin;
begin
  FStream := TSDLStream(context^.hidden.unknown.data1);

  case whence of
    RW_SEEK_SET: Orgin := soBeginning;
    RW_SEEK_CUR: Orgin := soCurrent;
    else
      Orgin := soEnd;
  end;

  Result := FStream.Seek(offset, Orgin);
end;

function RW_Read(context: PSDL_RWops; ptr: Pointer; size: SDL_Size_t; maxnum: SDL_Size_t): SDL_Size_t; {$IFNDEF GPC} cdecl; {$ENDIF}
var
  FStream: TSDLStream;
begin
  FStream := TSDLStream(context^.hidden.unknown.data1);
  Result := FStream.Read(ptr^, size * maxnum) div size;
end;

function RW_Write(context: PSDL_RWops; const ptr: Pointer; size: SDL_Size_t; num: SDL_Size_t): SDL_Size_t; {$IFNDEF GPC} cdecl; {$ENDIF}
var
  FStream: TSDLStream;
begin
  FStream := TSDLStream(context^.hidden.unknown.data1);
  Result := FStream.Write(ptr^, size * num) div size;
end;

function RW_Close(context: PSDL_RWops): SDL_SInt32; cdecl;
var
  FStream: TSDLStream;
begin
  FStream := TSDLStream(context^.hidden.unknown.data1);
  FStream.Free;
  Result := 0;
end;

{ TSDLStream }

function TSDLStream.GetSDL_RWops: PSDL_RWops;
begin
  Result := @FSDL_RWops;
end;

function TSDLStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FData.Read(Buffer, Count);
end;

function TSDLStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FData.Seek(Offset, Origin);
end;

function TSDLStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FData.Write(Buffer, Count);
end;

function TSDLStream.GetSize: Int64;
begin
  Result := FData.Size;
end;

procedure TSDLStream.SetSize(const NewSize: Int64);
begin
  FData.Size := NewSize;
end;

constructor TSDLStream.Create(const Data: TStream; OwnsStream: Boolean);
begin
  FData := Data;
  FOwnsStream := OwnsStream;

  FSDL_RWops._type := SDL_RWOPS_UNKNOWN;
  FSDL_RWops.size := @RW_Size;
  FSDL_RWops.seek := @RW_Seek;
  FSDL_RWops.read := @RW_Read;
  FSDL_RWops.write := @RW_Write;
  FSDL_RWops.close := @RW_Close;
  FSDL_RWops.hidden.unknown.data1 := Self;
end;

procedure TSDLStream.BeforeDestruction;
begin
  if FOwnsStream then
    FreeAndNil(FData);

  inherited BeforeDestruction;
end;

end.

