{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2014 by Mazen NEIFER of the Free Pascal development team
    and was adapted from wavopenal.pas copyright (c) 2010 Dmitry Boyarintsev.

    RIFF/WAVE sound file reader implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fpwavreader;

{$mode objfpc}{$H+}

interface

uses
	fpWavFormat, Classes;

type
	TSmplLoop = record
		CuePointID,
		LoopType,
		LoopStart,
		LoopEnd,
		Fraction,
		PlayCount: Cardinal;
	end;

	TSmplChunk = record
		MainChunk: record
			Manufacturer,
			Product,
			SamplePeriod,
			MIDIUnityNote,
			MIDIPitchFraction,
			SMPTEFormat,
			SMPTEOffset,
			NumSampleLoops,
			SamplerData: Cardinal;
		end;
		Exists:   Boolean;
		LoopInfo: TSmplLoop; // support only one loop for now
	end;

	{ TWavReader }
	TWavReader = class(TObject)
	private
		DataChunk:	TChunkHeader;
		ChunkPos:	Int64;
		EoF:		Boolean;
		FStream:	TStream;
		FFileName:	String;
	public
		fmt: 		TWaveFormat;
		smpl:		TSmplChunk;
		dataSize:	Cardinal;

		destructor	Destroy; override;
		function	LoadFromFile(const FileName: String): Boolean;
		function	LoadFromStream(AStream: TStream): Boolean;
		function	ReadBuf(var Buffer; BufferSize: Integer): Integer;
	end;

implementation

uses
  SysUtils;

procedure LEtoN(var fmt: TWaveFormat); overload;
begin
	with fmt, ChunkHeader do
	begin
		Size := LEtoN(Size);
		Format := LEtoN(Format);
		Channels := LEtoN(Channels);
		SampleRate := LEtoN(SampleRate);
		ByteRate := LEtoN(ByteRate);
		BlockAlign := LEtoN(BlockAlign);
		BitsPerSample := LEtoN(BitsPerSample);
	end;
end;

{ TWavReader }

destructor TWavReader.Destroy;
begin
	if (FFileName <> '') and Assigned(FStream) then
		FStream.Free;
	inherited Destroy;
end;

function TWavReader.LoadFromFile(const FileName: String): Boolean;
begin
	if (FFileName <> '') and Assigned(FStream) then
		FStream.Free;

	FStream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
	if Assigned(FStream) then
	begin
		Result := LoadFromStream(FStream);
		FFileName := FileName;
	end
	else
		Result := False;
end;

function TWavReader.LoadFromStream(AStream: TStream): Boolean;
var
	RIFF: TRiffHeader;
	datahdr: TChunkHeader;
begin
	FStream := AStream;
	FFileName := '';
	Result := FStream.Read(RIFF, SizeOf(RIFF)) = SizeOf(RIFF);
	RIFF.ChunkHeader.Size := LEtoN(RIFF.ChunkHeader.Size);
	Result := Result and
		(RIFF.ChunkHeader.ID = AUDIO_CHUNK_ID_RIFF) and
		(RIFF.Format = AUDIO_CHUNK_ID_WAVE);
	Result := Result and (FStream.Read(fmt, SizeOf(fmt)) = SizeOf(fmt));
	LEtoN(fmt);
	Result := Result and (fmt.ChunkHeader.ID = AUDIO_CHUNK_ID_fmt);
	if Result and (fmt.Format <> 1) then
	begin
		//writeln('WAVE file is using compression. Sorry, cannot load. Please provide uncompressed .wav');
		Exit(False);
	end;
	FStream.Read(datahdr, SizeOf(datahdr));
	dataSize := datahdr.Size;
	FStream.Seek(-SizeOf(datahdr), soFromCurrent);
end;

function Min(a, b: Integer): Integer;
begin
	if a < b then
		Result := a
	else
		Result := b;
end;

function TWavReader.ReadBuf(var Buffer; BufferSize: Integer): Integer;
var
	sz: Integer;
	p: TByteArray absolute Buffer;
	i: Integer;
	smplhdr: TChunkHeader;
begin
	i := 0;
	while (not EoF) and (i < bufferSize) do
	begin
		if ChunkPos >= DataChunk.Size then
		begin
			sz := FStream.Read(DataChunk, SizeOf(DataChunk));
			EoF := sz < SizeOf(DataChunk);
			if not EoF then
			begin
				DataChunk.Size := LEtoN(DataChunk.Size);
				if DataChunk.Id <> AUDIO_CHUNK_ID_data then
					ChunkPos := DataChunk.Size
				else
					ChunkPos := 0;
			end;
		end
		else
		begin
			sz := Min(BufferSize, DataChunk.Size - ChunkPos);
			sz := FStream.Read(p[i], sz);
			EoF := sz <= 0;
			Inc(ChunkPos, sz);
			Inc(i, sz);
		end;
	end;
	Result := i;

	// read smpl chunk if it exists
	smpl.Exists := False;
	if (BufferSize = dataSize) then
	begin
		i := FStream.Size - FStream.Position;
		if i >= 10 then
		begin
			FStream.Read(smplhdr, SizeOf(smplhdr));
			if 	(smplhdr.ID = AUDIO_CHUNK_ID_smpl) and
				(smplhdr.Size >= SizeOf(smpl.MainChunk)) then
			begin
				FStream.Read(smpl.MainChunk, SizeOf(smpl.MainChunk));
				smpl.Exists:= True;
				if smpl.MainChunk.NumSampleLoops > 0 then
					FStream.Read(smpl.LoopInfo, SizeOf(smpl.LoopInfo));
			end;
		end;
	end;
end;

end.

