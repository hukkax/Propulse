unit MIDIIO;

{$mode objfpc}{$H+}

{*******************************************************************************
*                                MIDI.PAS                                      *
*                                                                              *
*     This file is based on the MIDI device classes by Adrian Meyer            *
*     This file was taken from the ZIP archive 'demo_MidiDevices_D6.zip'       *
*     and partly changed by me, some changes take over from  'DAV_MidiIO.pas'  *
*                                                                              *
*                       latest changes 2015-04-13                              *
********************************************************************************
* V1.0  First release with simple MIDI Input/Output                            *
* V1.1  SysEx Input Event added, refactured error handling                     *
* V1.2  SysEx Output procedure added, changes sysex input for multiple ports   *
* V1.3  Changes by BREAKOUTBOX 2009-07  (www.breakoutbox.de)                   *
* V1.4  Changes adapted from DAV_MidiIO.pas - see http://www.ohloh.net/p/DAV   *
* V1.5  removed an Exception on sending SysEx Data to a CLOSED Output          *
* V1.6  added a Switch to choose between Exceptions and Other behaviore ...    *
* V1.7  replaced  pChar => pAnsiChar  Char => AnsiChar  string => AnsiString   *
*       to gain compatibility to DelphiXE and higher versions ..               *
*       Minimalized by hukka 2017-08-26 (SysEx, messaging, etc. removed)       *
*******************************************************************************}

interface

uses
	Classes, Messages, SysUtils, Math, Contnrs, MMSystem;


type
	TMIDIChannel      = 1..16;
	TMIDIDataByte     = 0..$7F;           //  7 bits
	TMIDIDataWord     = 0..$3FFF;         // 14 bits
	TMIDIStatusByte   = $80..$FF;
	TMIDIVelocity     = TMIDIDataByte;
	TMIDIKey          = TMIDIDataByte;
	TMIDINote         = TMIDIKey;

type
	// event if data is received
	TOnMidiInData = procedure(const DeviceIndex: Integer; const Status, Data1, Data2: Byte) of Object;

	EMidiDevices = Exception;

	// base class for MIDI devices
	TMidiDevices = class
	private
		fDevices:    TStringList;
		fMidiResult: MMResult;
		procedure SetMidiResult(const Value: MMResult);
	protected
		property  MidiResult: MMResult read fMidiResult write SetMidiResult;
		function  GetHandle(const DeviceIndex: integer): THandle;
	public
		constructor Create; virtual;
		destructor  Destroy; override;

		procedure Open(const DeviceIndex: integer); virtual; abstract;
		procedure Close(const DeviceIndex: integer); virtual; abstract;
		procedure CloseAll;
		function  IsOpen(DeviceIndex: Integer): Boolean;

		property  Devices: TStringList read fDevices;
	end;

	// MIDI input devices
	TMidiInput = class(TMidiDevices)
	private
		fOnMidiData: TOnMidiInData;
	public
		constructor Create; override;
		destructor  Destroy; override;

		procedure Open (const DeviceIndex: Integer); override;
		procedure Close(const DeviceIndex: Integer); override;

		property  OnMidiData: TOnMidiInData read fOnMidiData write fOnMidiData;
	end;

	// MIDI output devices
	TMidiOutput = class(TMidiDevices)
	public
		constructor Create; override;

		procedure Open (const DeviceIndex: Integer); override;
		procedure Close(const DeviceIndex: Integer); override;

		procedure Send(const DeviceIndex: Integer; const Status, Data1, Data2: byte);
		procedure SendSystemReset(const DeviceIndex: Integer);
		procedure SendAllSoundOff(const DeviceIndex: Integer; const Channel: byte);
	end;

	function MidiInput: TMidiInput;
	function MidiOutput: TMidiOutput;


implementation

// ================================================================================================
// TMidiBase
// ================================================================================================

constructor TMidiDevices.Create;
begin
	FDevices := TStringList.Create;
end;

destructor TMidiDevices.Destroy;
begin
	FreeAndNil(FDevices);
	inherited;
end;

// ================================================================================================

var
	gMidiInput: TMidiInput;
	gMidiOutput: TMidiOutput;

// ================================================================================================

function MidiInput: TMidiInput;
begin
	if not Assigned(gMidiInput) then
		gMidiInput := TMidiInput.Create;
	Result := gMidiInput;
end;

function MidiOutput: TMidiOutput;
begin
	if not Assigned(gMidiOutput) then
		gMidiOutput := TMidiOutput.Create;
	Result := gMidiOutput;
end;

procedure MidiInCallback(aMidiInHandle: PHMIDIIN; aMsg: Integer;
	aInstance, aMidiData, aTimeStamp: Integer); stdcall;
begin
	if (aMsg = MIM_DATA) and (Assigned(MidiInput.OnMidiData)) then
		MidiInput.OnMidiData(aInstance,
			(aMidiData and $000000FF),
			(aMidiData and $0000FF00) shr 8,
			(aMidiData and $00FF0000) shr 16);
end;

// ================================================================================================
// TMidiInput
// ================================================================================================

constructor TMidiInput.Create;
var
	i, AvailableMIDIinputs: Integer;
	lInCaps: TMidiInCaps;
begin
	inherited;
	try
		AvailableMIDIinputs := MidiInGetNumDevs;
	except
		AvailableMIDIinputs := 0;
	end;

	if AvailableMIDIinputs > 0 then
		for i := 0 to AvailableMIDIinputs-1 do
		begin
			MidiResult := midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
			if MidiResult = MMSYSERR_NOERROR then
				fDevices.Add(StrPas(lInCaps.szPname));
		end;
end;

destructor TMidiInput.Destroy;
begin
	inherited;
end;

procedure TMidiInput.Close(const DeviceIndex: Integer);
begin
	if GetHandle(DeviceIndex) <> 0 then
	begin
		MidiResult := midiInStop (GetHandle(DeviceIndex));
		MidiResult := midiInReset(GetHandle(DeviceIndex));
		MidiResult := midiInClose(GetHandle(DeviceIndex));
		FDevices.Objects[DeviceIndex] := nil;
	end;
end;

procedure TMidiDevices.CloseAll;
var
	i: integer;
begin
	for i := 0 to FDevices.Count-1 do
		Close(i);
end;

procedure TMidiInput.Open(const DeviceIndex: Integer);
var
	lHandle: THandle;
begin
	if GetHandle(DeviceIndex) <> 0 then Exit;

	MidiResult := midiInOpen(@lHandle, DeviceIndex, Cardinal(@midiInCallback),
		DeviceIndex, CALLBACK_FUNCTION);

	fDevices.Objects[DeviceIndex] := TObject(lHandle);

	MidiResult := midiInStart(lHandle);
end;

// ================================================================================================
// TMidiOutput
// ================================================================================================

constructor TMidiOutput.Create;
var
	i, AvailableMIDIoutputs: Integer;
	lOutCaps: TMidiOutCaps;
begin
	inherited;

	try
		AvailableMIDIoutputs := MidiOutGetNumDevs;
	except
		AvailableMIDIoutputs := 0;
	end;

	for i := 0 to AvailableMIDIoutputs-1 do
	begin
		MidiResult := MidiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));
		fDevices.Add(lOutCaps.szPname);
	end;
end;

procedure TMidiOutput.Open(const DeviceIndex: Integer);
var
	lHandle: THandle;
begin
	if GetHandle(DeviceIndex) <> 0 then Exit;
	MidiResult := midiOutOpen(@lHandle, DeviceIndex, 0, 0, CALLBACK_NULL);
	fDevices.Objects[DeviceIndex] := TObject(lHandle);
end;

procedure TMidiOutput.Close(const DeviceIndex: Integer);
begin
	if GetHandle(DeviceIndex) <> 0 then
	begin
		MidiResult := midiOutClose(GetHandle(DeviceIndex));
		fDevices.Objects[DeviceIndex] := nil;
	end;
end;

procedure TMidiOutput.Send(const DeviceIndex: Integer; const Status, Data1, Data2: Byte);
var
	lMsg: Cardinal;
begin
	if not Assigned(fDevices.Objects[DeviceIndex]) then Exit;
	lMsg:= Status or (Data1 shl 8) or (Data2 shl 16);
	MidiResult := MidiOutShortMsg(GetHandle(DeviceIndex), lMSG);
end;

// ================================================================================================
// Common MIDI Out messages
// ================================================================================================

// System Reset = Status Byte FFh
procedure TMidiOutput.SendSystemReset(const DeviceIndex: Integer); inline;
begin
	Send(DeviceIndex, $FF, $0, $0);
end;

// All Sound Off = Status + Channel Byte Bnh, n = Channel number
//                 Controller-ID = Byte 78h,  2nd Data-Byte = 00h
procedure TMidiOutput.SendAllSoundOff(const DeviceIndex: Integer; const Channel: Byte); inline;
begin
	Send(DeviceIndex, $B0 + Channel, $78, $0);
end;

procedure TMidiDevices.SetMidiResult(const Value: MMResult);
begin
	fMidiResult := Value;
end;

function TMidiDevices.GetHandle(const DeviceIndex: Integer): THandle;
begin
	 try
		if not InRange(DeviceIndex, 0, fDevices.Count - 1) then
			raise EMidiDevices.CreateFmt('%s: Device index out of bounds! (%d)',
				[ClassName,DeviceIndex]);
		Result:= THandle(fDevices.Objects[DeviceIndex]);
	except
		Result:= 0;
	end;
end;

function TMidiDevices.IsOpen(DeviceIndex: Integer): Boolean; inline;
begin
	Result := GetHandle(DeviceIndex) <> 0;
end;

// ================================================================================================

initialization
  gMidiInput  := nil;
  gMidiOutput := nil;

finalization
  FreeAndNil(gMidiInput);
  FreeAndNil(gMidiOutput);

end.

