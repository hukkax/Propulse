// ==========================================================================
// Generate waveform into sample data
// ==========================================================================
unit Dialog.GenerateWaveform;

interface

uses
	Types, Classes,
	CWE.Core, CWE.Dialogs,
	SampleEditor;


type
	TGenerateWaveformSettings = record
		Waveform:	TWavetype;
		Length:		Integer;		// length in samples
		SampleNote: Byte;
		DestNote:	Byte;
		DestOctave: Byte;

		procedure DialogCallback(ID: Word; Button: TDialogButton;
			ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
	end;

	procedure Dialog_GenerateWaveform;


implementation

uses
	Screen.Config,
	ProTracker.Util;

var
	GenerateWaveformSettings: TGenerateWaveformSettings;


procedure TGenerateWaveformSettings.DialogCallback(ID: Word; Button: TDialogButton;
	ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
begin
	if Dlg <> nil then Exit;

	if Button = btnOK then
		with GenerateWaveformSettings do
		SampleEdit.GenerateAudio(Waveform, Length,
			PeriodToHz(PeriodTable[SampleNote]),
			NoteHz[DestOctave * 12 + DestNote]);
end;

procedure Dialog_GenerateWaveform;
const
	W = 26;
	H = 10;
var
	Dlg: TCWEScreen;
	List: TCWEConfigList;
	Sect: AnsiString;
begin
	Dlg := ModalDialog.CreateDialog(0, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'Generate Waveform', False);

	List := TCWEConfigList.Create(Dlg, '', 'Waveform',
		Types.Rect(1, 2, W-1, H-3), True);
	List.ColumnWidth[1] := 8;
	List.Border.Pixel := True;
	List.Scrollbar.Visible := False;

	with ModalDialog do
	begin
		CreateConfigManager;
		Sect := 'Gen';

		ConfigManager.AddByte(Sect, '',
			@GenerateWaveformSettings.Waveform, Ord(GenerateWaveformSettings.Waveform)).
			SetInfo('Waveform', Ord(WAVE_SILENCE), Ord(WAVE_NOISE),
			['Silence', 'Sine', 'Square', 'Saw', 'Triangle', 'Noise']);
		ConfigManager.AddInteger(Sect, '',
			@GenerateWaveformSettings.Length, GenerateWaveformSettings.Length).
			SetInfo('Length', 1, $1FFFF, [], nil, '', 1, 100);
		ConfigManager.AddByte(Sect, '',
			@GenerateWaveformSettings.SampleNote, GenerateWaveformSettings.SampleNote)
			.SetInfo('Sample note', 0, 35, NoteNames, nil, '', 1, 12);
		ConfigManager.AddByte(Sect, '',
			@GenerateWaveformSettings.DestNote, GenerateWaveformSettings.DestNote)
			.SetInfo('Dest. Note', 0, 11,
				['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']);
		ConfigManager.AddByte(Sect, '',
			@GenerateWaveformSettings.DestOctave, GenerateWaveformSettings.DestOctave)
			.SetInfo('Dest. Octave', 0, 8, []);

		List.Init(ConfigManager);

		AddResultButton(btnOK,     'OK',     1, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-9, H-2);
//		ButtonCallback := GenerateWaveformSettings.DialogCallback; !!!

		Dialog.ActiveControl := List;
		Show;
	end;
end;

initialization

	with GenerateWaveformSettings do
	begin
		Waveform   := WAVE_SILENCE;
		Length     := 1024;
		SampleNote := 24; // C-3
		DestNote   := 0;
		DestOctave := 3;
	end;

end.

