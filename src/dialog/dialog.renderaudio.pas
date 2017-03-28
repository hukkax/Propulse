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
		class procedure SongToWAV(const Filename: String = '');
		class procedure DialogCallback(ID: Word; ModalResult: TDialogButton;
						Tag: Integer; Data: Variant; Dlg: TCWEDialog);
	end;

	TAudioRenderSettings = record
		LoopTimes,
		FadeOutLength,
		DestNote:		Byte;
		Normalize,
		BoostHighs,
		FadeOut:		Boolean;
	end;

	procedure Dialog_Render;

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
			SongToWAV;

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

procedure Dialog_Render;
const
	W  = 48-11-2;
	BH = 1;
	H  = 16-3 + BH;
var
	Dlg: TCWEScreen;

	function AddButton(const sCaption: AnsiString; aID: Word; XRight: Boolean; Y: Byte): TCWEButton;
	var
		XX, XW: Byte;
	begin
		if XRight then
		begin
			XX := Dlg.Width div 2 + 2;
			XW := Dlg.Width;
		end
		else
		begin
			XX := 1;
			XW := Dlg.Width div 2;
		end;
		with ModalDialog do
		begin
			Result := TCWEButton.Create(Dlg, sCaption, 'b' + Trim(sCaption),
				Types.Rect(XX, Y, XW, Y+1));
			Result.ModalResult := Ord(btnOK);
			Result.Tag := aID;
			Result.OnChange := ButtonClickHandler;
		end;
	end;

var
	Sect: AnsiString;
	List: TCWEConfigList;
	Y: Byte;
begin
	// ----------------------------------------------------------------------
	// Create the dialog
	// ----------------------------------------------------------------------

	Dlg := ModalDialog.CreateDialog(ACTION_RENDER, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - ((H+BH) div 2), W, H+BH),
		 'Render Audio');

	List := TCWEConfigList.Create(Dlg, '', 'Render Audio',
		Types.Rect(1, 2, W-1, H-BH-3), True);
	List.ColumnWidth[1] := 12;

	with ModalDialog do
	begin
		CreateConfigManager;

		Sect :=	'';

		// Render: [Song to file] [Selection to file] [Selection to sample]
{		ConfigManager.AddByte(Sect, '',
			@DialogBytes[BYTE_RENDER_TYPE], ACTION_SAM_SEL).
			SetInfo('Render', ACTION_SAM_SEL, ACTION_FILE_SONG,
			['Selection to sample', 'Selection to file', 'Song to file']);}


		// Normalize volume: B
		ConfigManager.AddBoolean(Sect, '',
			@AudioRenderSettings.Normalize, AudioRenderSettings.Normalize).
			SetInfo('Normalize volume', 0, 1, CN_YESNO);

		Sect :=	'Selection to sample';

		// Destination note: X
		ConfigManager.AddByte(Sect, '',
			@AudioRenderSettings.DestNote, AudioRenderSettings.DestNote)
		.SetInfo('Destination note', 0, 35, NoteNames);

		// Boost highs: B
		ConfigManager.AddBoolean(Sect, '',
			@AudioRenderSettings.BoostHighs, AudioRenderSettings.BoostHighs).
			SetInfo('Boost highs', 0, 1, CN_YESNO);

		Sect := 'Song to WAV';

		// Loop: I times
		ConfigManager.AddByte(Sect, '',
			@AudioRenderSettings.LoopTimes, AudioRenderSettings.LoopTimes).
			SetInfo('Song looping', 0, 5,
			['Don''t loop', 'Loop once', 'Loop 2 times', 'Loop 3 times',
			 'Loop 4 times', 'Loop 5 times']);

		// Enable fadeout: B
		ConfigManager.AddBoolean(Sect, '',
			@AudioRenderSettings.FadeOut, AudioRenderSettings.FadeOut).
			SetInfo('Fadeout at end', 0, 1, CN_YESNO);

		// Fadeout length: I seconds
		ConfigManager.AddByte(Sect, '',
			@AudioRenderSettings.FadeOutLength, AudioRenderSettings.FadeOutLength).
			SetInfo('Fadeout length (sec)', 1, 255, []);

		List.Init(ConfigManager);
		List.Scrollbar.Visible := False;
		List.Border.Pixel := True;
		List.Items.Delete(0); // 'general' heading

		Y := H - BH - 2;
		AddButton('Sel. To Sample',	BTN_SAM_SEL, 	False, Y);
		AddButton('Sel. To WAV', 	BTN_FILE_SEL, 	True,  Y);
		AddButton('Song To WAV', 	BTN_FILE_SONG, 	False, Y+2);
		AddButton('Cancel', BTN_CANCEL, True, Y+2).ModalResult := Ord(btnCancel);

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
{	with Window do
	begin
		SaveDialog.Filter := FILTER_WAV;
		SaveDialog.DefaultExt := 'wav';
		if SaveDialog.Execute then
		begin
			Module.PatternToWAV(SaveDialog.Filename, PeriodTable[AudioRenderSettings.DestNote],
				PatternEditor.PrepareSelectionForRender,
				AudioRenderSettings.Normalize, AudioRenderSettings.BoostHighs);
			Editor.MessageText('Rendered selection to ' + ExtractFileName(SaveDialog.Filename));
		end
		else
			CancelMessage;
	end; !!! }
end;

class procedure TAudioRender.SongToWAV(const Filename: String = '');
var
	Fade: Byte;
	S: String;
begin
{	if AudioRenderSettings.FadeOut then
		Fade := AudioRenderSettings.FadeOutLength
	else
		Fade := 0;

	with Window do
	begin
		if Filename = '' then
		begin
			SaveDialog.Filter := FILTER_WAV;
			SaveDialog.DefaultExt := 'wav';
			S := Editor.lblFilename.Caption;
			if S = '' then
				S := Trim(Module.Info.Title) + '.wav'
			else
				S := ChangeFileExt(S, '.wav');
			SaveDialog.FileName := S;
			if not SaveDialog.Execute then
			begin
				CancelMessage;
				Exit;
			end
			else
				S := SaveDialog.Filename;
		end
		else
			S := Filename;

		Window.Progress.Start;
		Module.RenderToWAV(S, AudioRenderSettings.LoopTimes, Fade);
		Window.Progress.Finish;
	end; !!! }
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

