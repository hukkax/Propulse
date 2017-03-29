// ==========================================================================
// Render song/pattern/selection to WAV/sample
// ==========================================================================
unit Dialog.RenderAudio;

interface

uses
	Types, Classes,
	CWE.Core,
	CWE.Dialogs;

const
	ACTION_RENDER			= 0;
	ACTION_SAMPLEOVERWRITE	= 1;

	BTN_SAM_SEL			= 0;	// Selection to sample
	BTN_FILE_SEL		= 1;	// Selection to file
	BTN_FILE_SONG		= 2;	// Song to file
	BTN_CANCEL			= 3;

type
	TAudioRender = class
	private
		class procedure CancelMessage;
	public
		class procedure SelToSample;
		class procedure SelToWAV;
		class procedure SongToWAV(Filename: String);
		class procedure DialogCallback(ID: Word; ModalResult: TDialogButton;
						Tag: Integer; Data: Variant; Dlg: TCWEDialog);
	end;

	TAudioRenderSettings = record
		Filename:		String;
		LoopTimes,
		FadeOutLength,
		DestNote:		Byte;
		Normalize,
		BoostHighs,
		FadeOut:		Boolean;
	end;

	procedure Dialog_Render(ToSample: Boolean; Filename: String = '');

// ==========================================================================

implementation

uses
	SysUtils,
	CWE.Widgets.Text,
	ProTracker.Util,
	ProTracker.Player,
	ProTracker.Sample,
	ProTracker.Editor,
	Screen.Editor,
	ConfigurationManager,
	Screen.Config;

var
	AudioRenderSettings: TAudioRenderSettings;



// ==========================================================================

class procedure TAudioRender.DialogCallback(ID: Word; ModalResult: TDialogButton;
	Tag: Integer; Data: Variant; Dlg: TCWEDialog);
begin
	if Dlg <> nil then Exit;

	if ModalResult = btnCancel then
	begin
		if ID <> ACTION_RENDER then
			CancelMessage;
		Exit;
	end;

	if ID = ACTION_RENDER then
	case Tag of

		BTN_FILE_SEL:
			SelToWAV;

		BTN_FILE_SONG:
			SongToWAV(AudioRenderSettings.Filename);

		BTN_SAM_SEL:
			if not IsEmptySample(Module.Samples[CurrentSample-1]) then
			begin
				ModalDialog.MessageDialog(ACTION_SAMPLEOVERWRITE,
					'Overwrite Sample',
					'Current sample is not empty. Proceed?',
					[btnYes, btnCancel], btnCancel, DialogCallback, 0);
			end
			else
				SelToSample;

	end
	else
	if ID = ACTION_SAMPLEOVERWRITE then
		SelToSample;
end;

// ==========================================================================
// Shows the render settings dialog
// ==========================================================================

procedure Dialog_Render(ToSample: Boolean; Filename: String = '');
var
	Dlg: TCWEScreen;

	function AddButton(const sCaption: AnsiString; aID: Word; X, Y: Byte): TCWEButton;
	begin
		with ModalDialog do
		begin
			Result := TCWEButton.Create(Dlg, sCaption, 'b' + Trim(sCaption),
				Types.Rect(X, Y, X+Length(sCaption)+2, Y+1));
			Result.ModalResult := Ord(btnOK);
			Result.Tag := aID;
			Result.OnChange := ButtonClickHandler;
		end;
	end;

var
	Sect: AnsiString;
	List: TCWEConfigList;
	LH, W, H, Y: Byte;
begin
	// ----------------------------------------------------------------------
	// Create the dialog
	// ----------------------------------------------------------------------

	// W  = 48-11-2;
	if ToSample then
	begin
		Sect := 'Selection To Sample';
		LH := 2;
		W := 24;
	end
	else
	begin
		Sect := 'Render To File';
		LH := 3;
		W := 34;
	end;

	H := LH + 4;

	Dlg := ModalDialog.CreateDialog(ACTION_RENDER, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - ((H+1) div 2), W, H+1),
		Sect);

	List := TCWEConfigList.Create(Dlg, '', 'Render Audio',
		Types.Rect(1, 2, W-1, 2+LH), True);

	if ToSample then
		List.ColumnWidth[1] := 3
	else
		List.ColumnWidth[1] := 10;

	with ModalDialog do
	begin
		CreateConfigManager;

		Sect :=	'';

		// Normalize volume: B
		ConfigManager.AddBoolean(Sect, '',
			@AudioRenderSettings.Normalize, AudioRenderSettings.Normalize).
			SetInfo('Normalize volume', 0, 1, CN_YESNO);

		if ToSample then
		begin
			// Sect := 'Selection to sample';

			// Destination note: X
			ConfigManager.AddByte(Sect, '',
				@AudioRenderSettings.DestNote, AudioRenderSettings.DestNote)
			.SetInfo('Destination note', 0, 35, NoteNames);

			// Boost highs: B
			ConfigManager.AddBoolean(Sect, '',
				@AudioRenderSettings.BoostHighs, AudioRenderSettings.BoostHighs).
				SetInfo('Boost highs', 0, 1, CN_YESNO);
		end
		else
		begin
			// Sect := 'Song to WAV';

			// Loop: I times
			ConfigManager.AddByte(Sect, '',
				@AudioRenderSettings.LoopTimes, AudioRenderSettings.LoopTimes).
				SetInfo('Song looping', 0, 5,
				['Don''t loop', 'Loop once', 'Loop 2x', 'Loop 3x', 'Loop 4x', 'Loop 5x']);

			// Enable fadeout: B
			ConfigManager.AddBoolean(Sect, '',
				@AudioRenderSettings.FadeOut, AudioRenderSettings.FadeOut).
				SetInfo('Fadeout at end', 0, 1, CN_YESNO);

			// Fadeout length: I seconds
			ConfigManager.AddByte(Sect, '',
				@AudioRenderSettings.FadeOutLength, AudioRenderSettings.FadeOutLength).
				SetInfo('Fadeout length (sec)', 1, 255, []);

			AudioRenderSettings.Filename := Filename;
		end;

		List.Init(ConfigManager);
		List.Scrollbar.Visible := False;
		List.Border.Pixel := True;
		List.Items.Delete(0); // 'general' heading

		Y := H - 1;

		if ToSample then
		begin
			AddButton('  Render  ', BTN_SAM_SEL, 1, Y);
			AddButton('Cancel', BTN_CANCEL, W-9, Y).ModalResult := Ord(btnCancel);
		end
		else
		begin
			AddButton('  Song  ', 	BTN_FILE_SONG, 	1, Y);
			AddButton('Selection', 	BTN_FILE_SEL, 	12,  Y);
			AddButton('Cancel', BTN_CANCEL, W-9, Y).ModalResult := Ord(btnCancel);
		end;

		ButtonCallback := TAudioRender.DialogCallback;

		Dialog.ActiveControl := List;
		Show;
	end;
end;

class procedure TAudioRender.SelToSample;
begin
	Module.RenderToSample(CurrentSample-1, PeriodTable[AudioRenderSettings.DestNote],
		PatternEditor.PrepareSelectionForRender,
		AudioRenderSettings.Normalize, AudioRenderSettings.BoostHighs);
	Editor.MessageText('Rendered selection to sample #' + IntToStr(CurrentSample));
	Module.SetModified;
end;

class procedure TAudioRender.SelToWAV;
begin
	if AudioRenderSettings.Filename <> '' then
	begin
		Module.PatternToWAV(AudioRenderSettings.Filename,
			PeriodTable[AudioRenderSettings.DestNote],
			PatternEditor.PrepareSelectionForRender,
			AudioRenderSettings.Normalize,
			AudioRenderSettings.BoostHighs);
		Editor.MessageText('Rendered selection to ' +
			ExtractFileName(AudioRenderSettings.Filename), True);
	end
	else
		Editor.MessageText('No filename given!');
end;

class procedure TAudioRender.SongToWAV(Filename: String);
var
	Fade: Byte;
begin
	if Filename = '' then
		Filename := AudioRenderSettings.Filename;

	if AudioRenderSettings.FadeOut then
		Fade := AudioRenderSettings.FadeOutLength
	else
		Fade := 0;

//	Window.Progress.Start;
	Module.RenderToWAV(Filename, AudioRenderSettings.LoopTimes, Fade);
//	Window.Progress.Finish;
end;

class procedure TAudioRender.CancelMessage;
begin
	Editor.MessageText('Canceled by user.');
end;

initialization

	with AudioRenderSettings do
	begin
		LoopTimes  := 1;
		FadeOutLength := 10;
		DestNote   := 24; // C-3
		Normalize  := True;
		BoostHighs := False;
		FadeOut    := True;
	end;

end.

