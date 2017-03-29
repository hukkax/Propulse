unit Screen.Samples;

interface

uses
	Classes, Types, SysUtils, SDL2,
	ShortcutManager, SampleView,
	TextMode, CWE.Core, CWE.Dialogs, CWE.Widgets.Text, CWE.Widgets.Numeric,
	ProTracker.Util, ProTracker.Player, ProTracker.Editor, ProTracker.Sample;

const
	ACTION_AMPLIFY_SAMPLE	= 1;
	ACTION_AMPLIFY_ALL		= 2;
	ACTION_RESAMPLE			= 3;
	ACTION_SWAPSAMPLES		= 4;
	ACTION_COPYSAMPLE		= 5;
	ACTION_REPLACESAMPLE	= 6;
	ACTION_FILTER_LOWPASS	= 7;
	ACTION_FILTER_HIGHPASS	= 8;

	// resample options
	CFG_FROM		= 0;
	CFG_TO			= 1;
	CFG_QUALITY		= 2;
	CFG_FROM_HZ		= 0;
	CFG_TO_HZ		= 1;
	CFG_NORMALIZE	= 0;
	CFG_BOOSTHIGHS	= 1;

type
	SampleListKeyNames = (
		keySampleNONE,
		keySampleMoveUp,
		keySampleMoveDown,
		keySampleInsertSlot,
		keySampleRemoveSlot,
		keySampleCopy,
		keySampleSwap,
		//Alt-X   Exchange sample (only in Sample List)
		keySampleClear,
		keySampleDelete,
		keySampleReplace,
		keySampleCutLeft,
		keySampleCutRight,
		keySampleReverse,
		keySampleInvert,
		keySampleCentralise,
		keySampleAmplify,
		keySampleAmplifyAll,
		keySampleResample,
		keySampleResampleQ,
		keySampleSave,
		keySampleShowAll,
		keySampleShowRange,
		keySampleZoomIn,
		keySampleZoomOut,
		keySampleSelectAll,
		keySampleSelectLoop,
		keySampleSelectPre,
		keySampleSelectPost,
		keySampleSelectNone,
		keySampleClipCut,
		keySampleClipCopy,
		keySampleClipPaste,
		keySampleClipMixPaste,
		keySampleFadeIn,
		keySampleFadeOut,
		keySampleCrossfade,
		keySampleEqualizer,
		keySampleFilterLo,
		keySampleFilterHi,
		keySampleGenerate
	);

	TSampleList = class(TCWEControl)
	private
		CursorInPlay: Boolean;
	const
		LENGTH_SAMPLETEXT = 22;
	public
		Cursor: 	TPoint;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function 	TextInput(var Key: Char): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;

		procedure 	Paint; override;
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWEEditList = class(TCWEList)
	public
		function	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
	end;

	TSampleScreen = class(TCWEScreen)
	private
		Updating:	Boolean;

		procedure 	SliderKeyDown(Sender: TCWEControl;
					var Key: Integer; Shift: TShiftState; var Handled: Boolean);
	public
		Waveform: 	TSampleView;
		SampleList: TSampleList;
		EditList:	TCWEEditList;
		sliLoopStart,
		sliLoopEnd,
		sliFineTune,
		sliVolume: 	TCWESlider;
		lblLoopStart,
		lblLoopEnd,
		lblSelection,
		lblLength:	TCWELabel;
		Cursor:		TPoint;

		procedure 	DialogCallback(ID: Word; Button: TDialogButton;
					ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
		procedure 	ResampleDialog(ShowDialog: Boolean);
		procedure 	DoResampleAfterDialog;

		procedure 	AfterSampleLoaded(const Sam: TSample);
		procedure 	LoadSample(const Filename: String);
		procedure	SaveSample;
		procedure 	ExchangeSamples(iFrom, iTo: Byte; ModifySamples, ModifyPatterns: Boolean);
		procedure 	InsertSampleSlot(iFrom: Byte);
		procedure 	DeleteSampleSlot(iFrom: Byte);

		procedure 	UpdateVUMeter;
		procedure 	UpdateSampleInfo;
		procedure	ValueChanged(Sender: TCWEControl);

		function 	GetSampleNames: TArrayOfAnsiString;

		procedure 	WaveformKeyDown(Sender: TCWEControl;
					var Key: Integer; Shift: TShiftState; var Handled: Boolean);

		constructor	Create(var Con: TConsole; const sCaption, sID: AnsiString); override;
	end;


var
	SampleScreen: TSampleScreen;
	SampleListKeys: TKeyBindings;


implementation


uses
	MainWindow, Math, Layout,
    soxr,
	SampleEditor,
	FloatSampleEffects,
	Screen.FileReqSample, Screen.Editor, Screen.Config, Dialog.ValueQuery;


procedure TSampleScreen.WaveformKeyDown(Sender: TCWEControl;
	var Key: Integer; Shift: TShiftState; var Handled: Boolean);
begin
	Handled := SampleList.KeyDown(Key, Shift);
end;

function TCWEEditList.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
begin
	Result := inherited;
	if not Result then
		Result := SampleScreen.SampleList.KeyDown(Key, Shift);
end;

procedure TSampleScreen.SliderKeyDown(Sender: TCWEControl;
	var Key: Integer; Shift: TShiftState; var Handled: Boolean);
begin
	Handled := True;

	case Key of

		SDLK_UP:
			Self.BrowseControls(True);

		SDLK_DOWN:
			Self.BrowseControls(False);

		SDLK_PAGEUP, SDLK_PAGEDOWN:
		begin
			if Key = SDLK_PAGEUP then
			begin
				if CurrentSample > 1 then
					Dec(CurrentSample);
			end
			else
			begin
				if CurrentSample < 31 then
					Inc(CurrentSample);
			end;

			SampleList.Paint;
			UpdateSampleInfo;
		end;

	else
		Handled := False;
	end;
end;


constructor TSampleScreen.Create;

	procedure AddEditCmd(const Name: String; Action: Integer);
	var
		LI: TCWEListItem;
	begin
		if Action < 0 then
			Action := LISTITEM_HEADER;
		LI := TCWEListItem.Create(Name, Action);
		EditList.Items.Add(LI);
	end;

var
	X, Y: Integer;
const
	W = 15;
begin
	inherited;

	RegisterScreenLayout(Self, 'SampleList');

	{
	X1=01 Y1=03
	X2=31 Y2=34
	Data=000B0607040E
	}
	SampleList := TSampleList.Create(Self, '', 'Sample List',
		Bounds(1, 3, 30, 31), True);

	{
	X1=33 Y1=03
	X2=79 Y2=13
	}
	Waveform := TSampleView.Create(Self, '', 'Sample Waveform',
		Types.Rect(33, 3, 79, 19), True);
	Waveform.Init(True, True);
	Waveform.OnKeyDown := WaveformKeyDown;

	X := 10; Y := 36;

	sliFineTune := TCWESlider.Create(Self, '', 'Sample Finetune',
		Bounds(X, Y, W, 1), True);
	sliFineTune.Init([-8, 7, 0]); // Min, Max, Default
	sliFineTune.CreateLabelControl;
	sliFineTune.OnChange  := ValueChanged;
	sliFineTune.OnKeyDown := SliderKeyDown;
	TCWELabel.Create(Self, 'Finetune', '', Bounds(1, Y, 8, 1), True);
	Inc(Y, 2);

	sliVolume := TCWESlider.Create(Self, '', 'Sample Default Volume',
		Bounds(X, Y, W, 1), True);
	sliVolume.Init([0, 64, 64]); // Min, Max, Default
	sliVolume.CreateLabelControl;
	sliVolume.OnChange  := ValueChanged;
	sliVolume.OnKeyDown := SliderKeyDown;
	TCWELabel.Create(Self, 'Volume', '', Bounds(1, Y, 8, 1), True);
	Inc(Y, 2);

	sliLoopStart := TCWESlider.Create(Self, '', 'Sample Loop Start',
		Bounds(X, Y, W, 1), True);
	sliLoopStart.Init([0, 64, 0]); // Min, Max, Default
	sliLoopStart.OnChange  := ValueChanged;
	sliLoopStart.OnKeyDown := SliderKeyDown;
	TCWELabel.Create(Self, 'Repeat', '', Bounds(1, Y, 8, 1), True);
	Inc(Y, 2);

	sliLoopEnd := TCWESlider.Create(Self, '', 'Sample Loop End',
		Bounds(X, Y, W, 1), True);
	sliLoopEnd.Init([0, 64, 0]); // Min, Max, Default
	sliLoopEnd.OnChange  := ValueChanged;
	sliLoopEnd.OnKeyDown := SliderKeyDown;
	TCWELabel.Create(Self, 'RepEnd', '', Bounds(1, Y, 8, 1), True);

	lblLength := TCWELabel.Create(Self, '', 'Sample Length Label',
		Bounds(Waveform.Rect.Left, Waveform.Rect.Bottom+2, 12, 1), True);
	lblSelection := TCWELabel.Create(Self, '', 'Sample Selection Label',
		Bounds(lblLength.Rect.Right+1, lblLength.Rect.Top, 33, 1), True);

	lblLoopStart := TCWELabel.Create(Self, '', 'Sample Loop Start Label',
		Bounds(sliLoopStart.Rect.Right+1, sliLoopStart.Rect.Top, 6, 1), True);
	lblLoopEnd := TCWELabel.Create(Self, '', 'Sample Loop End Label',
		Bounds(sliLoopEnd.Rect.Right+1, sliLoopEnd.Rect.Top, 6, 1), True);

	EditList := TCWEEditList.Create(Self, '', 'Edit Functions',
		Types.Rect(Waveform.Rect.Left, WaveForm.Rect.Bottom + 4,
			Waveform.Rect.Right-1, Console.Height - 1));

	AddHeader('Screen.Samples');

	RegisterLayoutControl(TCWEControl(SampleList),
		CTRLKIND_BOX, True, False, False);
	RegisterLayoutControl(TCWEControl(Waveform),
		CTRLKIND_BOX, False, True, True);
	RegisterLayoutControl(TCWEControl(sliFinetune),
		CTRLKIND_BOX, False, True, False);
	RegisterLayoutControl(TCWEControl(sliVolume),
		CTRLKIND_BOX, False, True, False);
	RegisterLayoutControl(TCWEControl(sliLoopStart),
		CTRLKIND_BOX, False, True, False);
	RegisterLayoutControl(TCWEControl(sliLoopEnd),
		CTRLKIND_BOX, False, True, False);
	RegisterLayoutControlClass(TCWEControl(Self),
		TCWELabel, CTRLKIND_LABEL, False, True, False);

	with Shortcuts do
	begin
		SampleListKeys := SetContext('Samples');

		//TODO: Sample.Centralise
		//TODO: Sample.Replace
		//TODO: Sample.Exchange sample (only in Sample List)

		Bind(keySampleInsertSlot,	'Slot.Insert',		'Alt+Insert');
		Bind(keySampleRemoveSlot,	'Slot.Delete',		'Alt+Delete');
		Bind(keySampleCopy,			'CopyFrom',			'Alt+P');
		Bind(keySampleSwap,			'Swap',				'Alt+S');
		Bind(keySampleReplace,		'Replace',			'Alt+R');
		Bind(keySampleCentralise,	'Centralize',		'Alt+H');
		Bind(keySampleMoveUp,		'Move.Up', 			'Alt+Up');
		Bind(keySampleMoveDown,		'Move.Down', 		'Alt+Down');
		Bind(keySampleClear,		'ClearName',		'Alt+C');
		Bind(keySampleDelete,		'Delete',			'Alt+D');
		Bind(keySampleCutLeft,		'Cut.PreLoop',		'Alt+B');
		Bind(keySampleCutRight,		'Cut.PostLoop',		'Alt+L');
		Bind(keySampleReverse,		'Reverse',			'Alt+G');
		Bind(keySampleInvert,		'Invert',			'Alt+I');
		Bind(keySampleAmplify,		'Amplify',			'Alt+M');
		Bind(keySampleAmplifyAll,	'Amplify.All',		'Alt+Shift+M');
		Bind(keySampleResample,		'Resample',			'Alt+E');
		Bind(keySampleResampleQ,	'Resample.Quick',	'Alt+F');
		Bind(keySampleSave,			'SaveSample',		['Alt+T', 'Alt+W', 'Alt+O']);

		Bind(keySampleShowAll,		'Show.All', 		'Alt+Ctrl+A');
		Bind(keySampleShowRange,	'Show.Range', 		'Alt+Ctrl+R');
		Bind(keySampleZoomIn,		'Zoom.In', 			'Keypad +');
		Bind(keySampleZoomOut,		'Zoom.Out', 		'Keypad -');
		Bind(keySampleSelectAll,	'Select.All', 		'Ctrl+A');
		Bind(keySampleSelectLoop,	'Select.Loop',		'Ctrl+L');
//		Bind(keySampleSelectPre,	'Select.PreLoop',	0);
//		Bind(keySampleSelectPost,	'Select.PostLoop',	0);
//		Bind(keySampleSelectNone,	'Select.None', 		0);
		Bind(keySampleClipCut,	   	'Cut', 				'Ctrl+X');
		Bind(keySampleClipCopy,	   	'Copy', 			'Ctrl+C');
		Bind(keySampleClipPaste,   	'Paste', 			'Ctrl+V');
		Bind(keySampleClipMixPaste,	'MixPaste',			'Ctrl+P');
//		Bind(keySampleFadeIn,		'Fade.In', 			0);
//		Bind(keySampleFadeOut,		'Fade.Out', 		0);
//		Bind(keySampleCrossfade,	'Crossfade',		0);
//		Bind(keySampleEqualizer,	'Equalize', 		0);
//		Bind(keySampleFilterLo,		'Filter.Lowpass',	0);
//		Bind(keySampleFilterHi,		'Filter.Highpass',	0);
//		Bind(keySampleGenerate,		'GenerateWaveform',	0);
	end;

	with EditList do
	begin
		Selection3D := True;
		WantHover := True;
		OnActivate := SampleEdit.OnCommand;

		Data[1].Value := 2;
		Data[2].Value := 3;
		Data[3].Value := 1;

		AddEditCmd('Display', -1);		// ----------------------------------

		AddEditCmd('Show All', 			actShowAll);
		AddEditCmd('Show Range', 		actShowRange);
		AddEditCmd('Zoom In', 			actZoomIn);
		AddEditCmd('Zoom Out', 			actZoomOut);

		AddEditCmd('Select', -1);		// ----------------------------------

		AddEditCmd('Select All', 		actSelectAll);
		AddEditCmd('Select Loop',		actSelectLoop);
		AddEditCmd('Select Pre-loop', 	actSelectPreLoop);
		AddEditCmd('Select Post-loop', 	actSelectPostLoop);
		AddEditCmd('Select None', 		actSelectNone);

		AddEditCmd('Clipboard', -1);	// ----------------------------------

		AddEditCmd('Cut', 				actCut);
		AddEditCmd('Copy', 				actCopy);
		AddEditCmd('Paste', 			actPaste);
		AddEditCmd('Mix Paste',			actMixPaste);

		AddEditCmd('Effects', -1);		// ----------------------------------

		AddEditCmd('Amplify', 				actAmplify);
		AddEditCmd('Fade In', 				actFadeIn);
		AddEditCmd('Fade Out', 				actFadeOut);
		AddEditCmd('Crossfade',				actCrossfade);
//		AddEditCmd('Equalizer', 			actEqualizer);

		AddEditCmd('Lowpass Filter',		actFilterLo);
		AddEditCmd('Highpass Filter',		actFilterHi);
		AddEditCmd('Filter (Decrease Treble)',	actFilterFlt);
		AddEditCmd('Boost  (Increase Treble)',	actFilterBst);

		AddEditCmd('Reverse', 				actReverse);
		AddEditCmd('Invert', 				actInvert);
		if SOXRLoaded then
		begin
			//AddEditCmd('Resample', 		actResample);
		end;
		AddEditCmd('Generate Waveform...',	actGenerate);

		SampleEdit.Waveform := Waveform;
		AdjustScrollbar;
	end;

	ActiveControl := SampleList;
	LoadLayout(Self);
end;

procedure TSampleScreen.DialogCallback(ID: Word; Button: TDialogButton;
	ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
var
	Sample: TSample;
	ctrl: TCWEControl;
	Pos, X1, X2: Integer;
begin
	if Dlg = nil then Exit;
	if not (Button in [btnYes, btnOK]) then Exit;

	Pos := -1;

	ctrl := Dlg.Dialog.FindControl('Slider');
	if ctrl <> nil then
		Pos := TCWESlider(ctrl).Position
	else
	begin
		ctrl := Dlg.Dialog.FindControl('List');
		if ctrl <> nil then
			Pos := TCWEList(ctrl).ItemIndex;
	end;

	case ID of

		ACTION_AMPLIFY_SAMPLE:
			if Pos >= 0.0 then
			begin
				SampleEdit.GetSelection(X1, X2);
				GetCurrentSample.Normalize(Max(Pos, 0.01) / 100, X1, X2);
				Module.SetModified;
			end;

		ACTION_AMPLIFY_ALL:
			if Pos >= 0.0 then
			begin
				for Sample in Module.Samples do
					Sample.Normalize(Max(Pos, 0.01) / 100);
				Module.SetModified;
			end;

		ACTION_RESAMPLE:
			DoResampleAfterDialog;

		ACTION_SWAPSAMPLES:
			if Pos >= 0 then
			begin
				ExchangeSamples(CurrentSample-1, Pos, True, True);
				CurrentSample := Pos+1;
			end;

		ACTION_REPLACESAMPLE:
			if Pos >= 0 then
				PatternEditor.ReplaceSample(CurrentSample, Pos);

		ACTION_COPYSAMPLE:
			if (Pos >= 0) and (Pos <> CurrentSample-1) then
			begin
				GetCurrentSample.Assign(Module.Samples[Pos]);
				Module.SetModified;
			end;

		ACTION_FILTER_LOWPASS:
			if Pos >= 0 then
				SampleEdit.FilterLo(Pos);

		ACTION_FILTER_HIGHPASS:
			if Pos >= 0 then
				SampleEdit.FilterHi(Pos);


	end;

	Waveform.DrawWaveform;
	UpdateSampleInfo;
end;

procedure TSampleScreen.AfterSampleLoaded(const Sam: TSample);
begin
	ChangeScreen(TCWEScreen(SampleScreen));
	Waveform.Sample := Sam;
	UpdateSampleInfo;
end;

procedure TSampleScreen.LoadSample(const Filename: String);
var
	Sample: TSample;
begin
	if not ValidFilename(Filename) then Exit;

	Sample := GetCurrentSample;

	if Sample.Length > 0 then
		Module.Stop;
	Sample.LoadFromFile(FileName);

	AfterSampleLoaded(Sample);
end;

procedure TSampleScreen.SaveSample;
begin
	SampleRequester.Show(True, Options.Dirs.Samples);
end;

procedure TSampleScreen.UpdateVUMeter;
var
	i, x, y, c, pos: Integer;
	Sam: TSample;
const
	PCH = 128 + 65;
begin
	x := SampleList.Rect.Left - 1;
	y := SampleList.Rect.Top;

	for i := 0 to Module.Samples.Count-1 do
	begin
		Sam := Module.Samples[i];
		pos := -1;
		for c := 0 to AMOUNT_CHANNELS-1 do
			if Module.Channel[c].Paula.Sample = i then
				pos := Module.Channel[c].Paula.PlayPos;

		if Sam.Age < 0 then					// never been played
			Console.PutChar(x, y+i, 32)
		else
		if Sam.Age > 0 then					// recently started playing
		begin
			Console.PutChar(x, y+i, PCH + Sam.Age, 11);
			Dec(Sam.Age);
		end
		else
		if pos >= 0 then					// still playing
			Console.PutChar(x, y+i, PCH, 11)
		else 								// not playing anymore
			Console.PutChar(x, y+i, PCH, 1);
	end;

	Sam := GetCurrentSample;
	if Waveform.Sample <> Sam then
	begin
		Waveform.Sample := Sam;
		UpdateSampleInfo;
	end
	else
	begin
		if Module.SampleChanged[Sam.Index-1] then
		begin
			Module.SampleChanged[Sam.Index-1] := False;
			Waveform.DrawWaveform;
		end
		else
			Waveform.Paint;
	end;
end;

procedure TSampleScreen.ResampleDialog(ShowDialog: Boolean);
const
	W = 33;
	H = 14;
	Sect = 'ResampleOptions';
	NoteNames: array[0..36] of AnsiString = (
		'C-1','C#1','D-1','D#1','E-1','F-1','F#1','G-1','G#1','A-1','A#1','B-1',
		'C-2','C#2','D-2','D#2','E-2','F-2','F#2','G-2','G#2','A-2','A#2','B-2',
		'C-3','C#3','D-3','D#3','E-3','F-3','F#3','G-3','G#3','A-3','A#3','B-3',
		'Specify Hz' );
var
	Dlg: TCWEScreen;
	List: TCWEConfigList;
begin
	if IsEmptySample(GetCurrentSample) then Exit;
	if not SOXRLoaded('Resample') then Exit;

	if not ShowDialog then
	begin
		// just resample using default config
		// TODO: figure out source Hz
		GetCurrentSample.Resample({c5speed,}44100,
			PeriodToHz(PeriodTable[Options.Import.Resampling.ResampleTo]),
			SOXRQuality[Options.Import.Resampling.Quality],
			Options.Import.Resampling.Normalize,
			Options.Import.Resampling.HighBoost
			);
		UpdateSampleInfo;
		Exit;
	end;

	Dlg := ModalDialog.CreateDialog(ACTION_RESAMPLE, Bounds(
		(Console.Width div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'Resample');

	List := TCWEConfigList.Create(Dlg, '', Sect,
		Types.Rect(2, 3, W-2, H-4), True);
	List.ColumnWidth[1] := 11;
	List.Scrollbar.Visible := False;

	with ModalDialog do
	begin
		CreateConfigManager;

		ConfigManager.AddByte(Sect, '',
			@DialogBytes[CFG_FROM], 36)
		.SetInfo('Resample from', 0, 36, NoteNames);

		ConfigManager.AddInteger(Sect, '',
			@DialogIntegers[CFG_FROM_HZ], 44100)
		.SetInfo('         Hz', 0, 44100, []);

		ConfigManager.AddByte(Sect, '',
		@DialogBytes[CFG_TO], 24)
		.SetInfo('Resample to', 0, 36, NoteNames);

		ConfigManager.AddInteger(Sect, '',
			@DialogIntegers[CFG_TO_HZ], 44100)
		.SetInfo('         Hz', 0, 44100, []);

		ConfigManager.AddByte(Sect, '',
		@DialogBytes[CFG_QUALITY], 4)
		.SetInfo('Quality', 0, 4,
		['Quick Cubic', 'Low', 'Medium', 'High', 'Very High']);

		ConfigManager.AddBoolean(Sect, '',
		@DialogBooleans[CFG_NORMALIZE], True)
		.SetInfo('Normalize levels', 0, 1, ['No', 'Yes']);

		ConfigManager.AddBoolean(Sect, '',
		@DialogBooleans[CFG_BOOSTHIGHS], True)
		.SetInfo('Boost highs', 0, 1, ['No', 'Yes']);

		List.Init(ConfigManager);

		AddResultButton(btnOK,     'OK',     2, H-2, True);
		AddResultButton(btnCancel, 'Cancel', W-10, H-2);
		ButtonCallback := DialogCallback;
		Show;
	end;
end;

procedure TSampleScreen.DoResampleAfterDialog;
const
	WantHz = 36;
var
	Sample: TSample;
	HzFrom, HzTo: Cardinal;
begin
	Sample := GetCurrentSample;

	if DialogBytes[CFG_FROM] = WantHz then
		HzFrom := DialogIntegers[CFG_FROM_HZ]
	else
		HzFrom := PeriodToHz(PeriodTable[DialogBytes[CFG_FROM]]);

	if DialogBytes[CFG_TO] = WantHz then
		HzTo := DialogIntegers[CFG_TO_HZ]
	else
		HzTo := PeriodToHz(PeriodTable[DialogBytes[CFG_TO]]);

	Sample.Resample(HzFrom, HzTo,
		SOXRQuality[DialogBytes[CFG_QUALITY]],
		DialogBooleans[CFG_NORMALIZE],
		DialogBooleans[CFG_BOOSTHIGHS]
	);

	UpdateSampleInfo;
end;

procedure TSampleScreen.ExchangeSamples(iFrom, iTo: Byte;
	ModifySamples, ModifyPatterns: Boolean);
var
	p, c, y: Integer;
	Note: PNote;
begin
	if (iFrom = iTo) then Exit;

	if ModifySamples then
	begin
		Module.Stop;
		Module.Samples.Move(iFrom, iTo);
	end;

	if ModifyPatterns then
	begin
		Inc(iFrom);
		Inc(iTo);
		for p := 0 to Module.Info.PatternCount do
		for c := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
		begin
			Note := @Module.Notes[p, c, y];
			if Note.Sample = iFrom then
				Note.Sample := iTo
			else
			if Note.Sample = iTo then
				Note.Sample := iFrom;
		end;
	end;

	Module.SetModified;
end;

procedure TSampleScreen.InsertSampleSlot(iFrom: Byte);
var
	p, c, y: Integer;
	Note: PNote;
begin
	if not IsEmptySample(Module.Samples[30]) then Exit;

	Module.Stop;
	Module.Samples.Move(30, iFrom);
	Module.IndexSamples;

	Inc(iFrom);
	for p := 0 to Module.Info.PatternCount do
	for c := 0 to AMOUNT_CHANNELS-1 do
	for y := 0 to 63 do
	begin
		Note := @Module.Notes[p, c, y];
		if Note.Sample >= iFrom then
			Note.Sample := Note.Sample + 1;
	end;

	Module.SetModified;
end;

procedure TSampleScreen.DeleteSampleSlot(iFrom: Byte);
var
	p, c, y: Integer;
	Note: PNote;
begin
	if not IsEmptySample(Module.Samples[iFrom]) then Exit;

	Module.Stop;
	Module.Samples.Move(iFrom, 30);
	Module.IndexSamples;

	Inc(iFrom);
	for p := 0 to Module.Info.PatternCount do
	for c := 0 to AMOUNT_CHANNELS-1 do
	for y := 0 to 63 do
	begin
		Note := @Module.Notes[p, c, y];
		if Note.Sample >= iFrom then
			Note.Sample := Note.Sample - 1;
	end;

	Module.SetModified;
end;

procedure TSampleScreen.ValueChanged(Sender: TCWEControl);
var
	i: Integer;
	Sample: TSample;
begin
	if (Updating) or (not (Sender.Focused)) then Exit;
	if not (CurrentSample in [1..31]) then Exit;

	Sample := GetCurrentSample;

	if Sender = sliFineTune then
	begin
		for i := Low(FineTunes) to High(FineTunes) do
			if FineTunes[i] = sliFineTune.Position then
			begin
				Sample.Finetune := i;
				Exit;
			end;
	end
	else
	if Sender = sliVolume then
	begin
		Sample.Volume := sliVolume.Position;
	end
	else
	if (Sender = sliLoopStart) or (Sender = sliLoopEnd) then
	begin
		Sample.LoopStart  := sliLoopStart.Position;
		Sample.LoopLength := sliLoopEnd.Position - sliLoopStart.Position;
		Sample.UpdateVoice;
		UpdateSampleInfo;
		Waveform.Paint;
	end;
end;

function TSampleScreen.GetSampleNames: TArrayOfAnsiString;
var
	i: Integer;
	S: AnsiString;
begin
	SetLength(Result, 31);
	for i := 0 to 30 do
	begin
		S := Copy(Module.Samples[i].Name, 1, 22);
		Result[i] := Format('%.2d "%21s" %.8d', [i+1, S, Module.Samples[i].Length]);
	end;
end;

procedure TSampleScreen.UpdateSampleInfo;
var
	Sample: TSample;
	sFmt: String;
	t: Integer;
begin
	Updating := True;
	Sample := GetCurrentSample;

	Console.BeginUpdate;

	Sample.LoopLength      := Max(Sample.LoopLength, 1);
	sliFineTune.Position   := FineTunes[Sample.Finetune];
	sliVolume.Position     := Sample.Volume;
	sliLoopEnd.Min         := Sample.LoopStart + 1;
	sliLoopEnd.Max         := Sample.Length;
	sliLoopEnd.Position    := Sample.LoopLength + Sample.LoopStart;
	sliLoopStart.Max       := Max(0, sliLoopEnd.Position - 1);
	sliLoopStart.Position  := Sample.LoopStart;

	if Options.Display.SampleAsBytes then
		t := 2
	else
		t := 1;

	if Waveform.Selection.L < 0 then
		sFmt := 'None'
	else
	if Waveform.Selection.R > Waveform.Selection.L then
	begin
		if Options.Display.SizesAsDecimal then
			sFmt := '%d-%d (%d bytes)'
		else
			sFmt := '%.5x-%.5x (%x bytes)';
		sFmt := Format(sFmt,
			[Waveform.Selection.L, Waveform.Selection.R, Waveform.Selection.Length]);
	end
	else
	begin
		if Options.Display.SizesAsDecimal then
			sFmt := '%d'
		else
			sFmt := '%.5x';
		sFmt := Format(sFmt, [Waveform.Selection.L]);
	end;
	lblSelection.SetCaption('Sel: '  + sFmt);

	if Options.Display.SizesAsDecimal then
		sFmt := '%d     '
	else
		sFmt := '%.5x';

	lblLength.SetCaption('Size: ' + Format(sFmt, [Sample.Length * t]));
	lblLoopStart.SetCaption(Format(sFmt, [Sample.LoopStart * t]));
	lblLoopEnd.SetCaption(Format(sFmt, [(Sample.LoopStart + Sample.LoopLength) * t]));

	Console.EndUpdate;
	Updating := False;
end;

{ TSampleList }

constructor TSampleList.Create;
begin
	inherited;

	SetData(0, 0,  'Cursor foreground');
	SetData(1, 11, 'Cursor background');
	SetData(2, 6,  'Play text enabled');
	SetData(3, 7,  'Play text disabled');
	SetData(4, 4,  'Last character color');
	SetData(5, 14, 'Selection background');

	ColorFore := 6;
	ColorBack := 3;
	Cursor.X := LENGTH_SAMPLETEXT;

	WantMouse := True;
	WantKeyboard := True;
	WantHover := False;
end;

procedure SampleChanged;
begin
	Module.SetModified;
	SampleScreen.Waveform.DrawWaveform;
	SampleScreen.UpdateSampleInfo;
end;

function TSampleList.TextInput;
var
	S: AnsiString;
	Sample: TSample;
begin
	Result := False;
	if (Focused) and (not CursorInPlay) and (not (ssCtrl in GetShiftState)) then
	begin
		// Modify sample name
		//
		Sample := GetCurrentSample;
		if Sample = nil then Exit;

		S := Sample.GetName;
		while Length(S) < Cursor.X do
			S := S + ' ';
		Insert(Key, S, Cursor.X+1);
		Sample.SetName(S);
		Inc(Cursor.X);

		Module.SetModified;
		Result := True;
		Paint;
	end;
end;

function TSampleList.KeyDown;
var
	i, PreviousSample: Integer;
	Sc: ControlKeyNames;
	Scl: SampleListKeyNames;
	Scn: EditorKeyNames;
	S: AnsiString;
	Sample: TSample;
	nv: Single;
	IsFocused: Boolean;
	Waveform: TSampleView;
begin
	Result := False;
	IsFocused := Focused;

	PreviousSample := CurrentSample;
	Sample := GetCurrentSample;
	CursorInPlay := (Cursor.X >= LENGTH_SAMPLETEXT);
	Waveform := SampleScreen.Waveform;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	// process waveform edit keys when list is not focused
	//
	if not IsFocused then
	case Sc of

		ctrlkeyHOME:
			with SampleScreen.Waveform do
			begin
				Result := True;
				Selection.SetRange(0, 0, Sample);
				Selection.Origin := 0;
			end;

		ctrlkeyEND:
			with Waveform do
			begin
				Result := True;
				i := Sample.ByteLength;
				Selection.SetRange(i, i, Sample);
				Selection.Origin := i;
			end;

		ctrlkeyPGUP,
		ctrlkeyPGDN,
		ctrlkeyLEFT,
		ctrlkeyRIGHT:
			Result := True;

		ctrlkeyDELETE:
			begin
				Result := True;
				SampleEdit.Cut;
			end;

	end;

	if Result then
	begin
		Waveform.DrawWaveform;
		SampleScreen.UpdateSampleInfo;
	end
	else
	case Sc of

		ctrlkeyUP:
			begin
				if CurrentSample > 1 then
					Dec(CurrentSample);
				Result := True;
			end;

		ctrlkeyDOWN:
			begin
				if CurrentSample < 31 then
					Inc(CurrentSample);
				Result := True;
			end;

		ctrlkeyLEFT:
			begin
				if Cursor.X > 0 then
					Dec(Cursor.X);
				Result := True;
			end;

		ctrlkeyRIGHT:
			begin
				if Cursor.X < LENGTH_SAMPLETEXT then
					Inc(Cursor.X);
				Result := True;
			end;

		ctrlkeyHOME:
			begin
				Cursor.X := 0;
				Result := True;
			end;

		ctrlkeyEND:
			begin
				Cursor.X := LENGTH_SAMPLETEXT;
				Result := True;
			end;

		ctrlkeyPGUP:
			begin
				CurrentSample := Max(1,  CurrentSample - 15);
				Result := True;
			end;

		ctrlkeyPGDN:
			begin
				CurrentSample := Min(31, CurrentSample + 15);
				Result := True;
			end;

		ctrlkeyDELETE:
			if not CursorInPlay then
			begin
				S := Sample.GetName;
				Delete(S, Cursor.X+1, 1);
				Sample.SetName(S);
				Result := True;
				Module.SetModified;
			end;

		ctrlkeyBACKSPACE:
			if not CursorInPlay then
			begin
				S := Sample.GetName;
				Delete(S, Cursor.X, 1);
				Sample.SetName(S);
				if Cursor.X > 0 then Dec(Cursor.X);
				Result := True;
				Module.SetModified;
			end;

		ctrlkeyRETURN:
			if IsFocused then
				SampleRequester.Show(False, Options.Dirs.Samples);

	else
		Result := False;

		if ((CursorInPlay) or (not IsFocused)) and (Shift = []) then
		begin
			// Play notes
			//
			Scn := EditorKeyNames(Shortcuts.Find(EditorKeys, Key, Shift));
			case Scn of

				EditorKeyNames(0): //keyNONE:
					Exit(False);

				keyNoteC_lo..keyNoteB_hi:
				begin
					i := Integer(Scn) - Integer(keyNoteC_lo) + (Integer(PatternEditor.HighOctave) * 12);
					if i < 36 then // go no higher than B-3
					begin
						with Waveform do // play selected range
						if (not IsFocused) and (Selection.Length > 0) then
							Module.PlaySample(i, CurrentSample, PatternEditor.Cursor.Channel, 64,
								Selection.L, (Selection.R - Selection.L) div 2)
						else
							Module.PlaySample(i, CurrentSample, PatternEditor.Cursor.Channel);

						Result := True;
					end;
				end;

				keySelectOctaveLo,
				keySelectOctaveHi:
				begin
					Editor.SetOctave(Scn = keySelectOctaveHi);
					Result := True;
				end;
			end;
		end
		{else
		if (IsFocused) and (not CursorInPlay) and (not (ssCtrl in Shift)) then
		begin
			// Modify sample name
			//
			ch := GetCharFromVirtualKey(Key);
			if ch <> '' then
			begin
				S := Sample.GetName;
				while Length(S) < Cursor.X do
					S := S + ' ';
				Insert(ch, S, Cursor.X+1);
				Sample.SetName(S);
				Inc(Cursor.X);
				Result := True;
				Module.SetModified;
			end;
		end};

		if not Result then
		begin
			Scl := SampleListKeyNames(Shortcuts.Find(SampleListKeys, Key, Shift));

			case Scl of

				keySampleShowAll:			SampleEdit.ProcessCommand(actShowAll);
				keySampleShowRange:			SampleEdit.ProcessCommand(actShowRange);

				keySampleZoomIn:			SampleEdit.ProcessCommand(actZoomIn);
				keySampleZoomOut:			SampleEdit.ProcessCommand(actZoomOut);

				keySampleSelectAll:			SampleEdit.ProcessCommand(actSelectAll);
				keySampleSelectLoop:		SampleEdit.ProcessCommand(actSelectLoop);
				keySampleSelectPre:			SampleEdit.ProcessCommand(actSelectPreLoop);
				keySampleSelectPost:		SampleEdit.ProcessCommand(actSelectPostLoop);
				keySampleSelectNone:		SampleEdit.ProcessCommand(actSelectNone);

				keySampleClipCut:			SampleEdit.ProcessCommand(actCut);
				keySampleClipCopy:			SampleEdit.ProcessCommand(actCopy);
				keySampleClipPaste:			SampleEdit.ProcessCommand(actPaste);
				keySampleClipMixPaste:		SampleEdit.ProcessCommand(actMixPaste);

				keySampleCutLeft:
				begin
					Module.Stop;
					Sample.PreLoopCut;
					SampleChanged;
				end;

				keySampleCutRight:
				begin
					Module.Stop;
					Sample.PostLoopCut;
					SampleChanged;
				end;

				keySampleReverse:
				begin
					Sample.Reverse;
					SampleChanged;
				end;

				keySampleInvert:
				begin
					Sample.Invert;
					SampleChanged;
				end;

				keySampleCentralise:
				begin
i := High(Sample.Data);
modaldialog.ShowMessage('Sample Info',
Format('%d bytes (%d words)   %d, %d',
[Sample.ByteLength, Sample.Length, Sample.Data[i-1], Sample.Data[i]]));
{					Sample.Centralise;
					SampleChanged;}
				end;

				keySampleAmplify:
					AskValue(ACTION_AMPLIFY_SAMPLE, 'Sample amplification %:',
						0, 300, Trunc(Sample.GetNormalizationValue * 100),
						SampleScreen.DialogCallback);

				keySampleAmplifyAll:
				begin
					nv := 1.0;
					for Sample in Module.Samples do
						nv := Min(Sample.GetNormalizationValue, nv);
					AskValue(ACTION_AMPLIFY_ALL, 'Sample amplification %:',
						0, 300, Trunc(nv * 100),
						SampleScreen.DialogCallback);
				end;

				keySampleResample,
				keySampleResampleQ:
					SampleScreen.ResampleDialog(Scl = keySampleResample);

				keySampleSave:
					SampleScreen.SaveSample;

				keySampleMoveUp:
					if (IsFocused) and (CurrentSample > 1) then
					begin
						SampleScreen.ExchangeSamples(
							CurrentSample-1, CurrentSample-2, True, True);
						Module.IndexSamples;
						Dec(CurrentSample);
					end;

				keySampleMoveDown:
					if (IsFocused) and (CurrentSample <= 30) then
					begin
						SampleScreen.ExchangeSamples(
							CurrentSample-1, CurrentSample, True, True);
						Module.IndexSamples;
						Inc(CurrentSample);
					end;

				keySampleInsertSlot:
					if IsFocused then
						SampleScreen.InsertSampleSlot(CurrentSample-1);

				keySampleRemoveSlot:
					if IsFocused then
						SampleScreen.DeleteSampleSlot(CurrentSample-1);

				keySampleCopy:
					if IsFocused then
						AskList(ACTION_COPYSAMPLE, 'Copy sample from:',
						CurrentSample-1, SampleScreen.GetSampleNames, SampleScreen.DialogCallback);

				keySampleSwap:
					if IsFocused then
						AskList(ACTION_SWAPSAMPLES, 'Swap sample with:',
						CurrentSample-1, SampleScreen.GetSampleNames, SampleScreen.DialogCallback);

				keySampleReplace:
					if IsFocused then
						AskList(ACTION_REPLACESAMPLE, 'Replace sample with:',
						CurrentSample-1, SampleScreen.GetSampleNames, SampleScreen.DialogCallback);

				keySampleClear:
					if IsFocused then
					begin
						Sample.SetName('');
						Module.SetModified;
					end;

				keySampleDelete:
				begin
					Module.Stop;
					Sample.Clear;
					SampleChanged;
				end;


			else
				Exit;
			end;
		end;

	end;

	// new sample selected, update slider values
	//
	if CurrentSample <> PreviousSample then
		SampleScreen.UpdateSampleInfo;

	Paint;
end;

function TSampleList.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	Dec(P.X, 3);
	if (P.X < 0) or (Button <> mbLeft) then Exit(False);

	CurrentSample := P.Y + 1;
	Cursor.X := Min(P.X, LENGTH_SAMPLETEXT);

	SampleScreen.UpdateSampleInfo;
	Paint;
	Result := True;
end;

procedure TSampleList.Paint;
var
	x, i: Integer;
	col: Byte;
	Sam: TSample;
begin
	if not Screen.Active then Exit;

	CursorInPlay := (Cursor.X >= LENGTH_SAMPLETEXT);

	Console.BeginUpdate;

	Console.FrameRect(Types.Rect(
		Rect.Left+3, Rect.Top, Rect.Right, Rect.Bottom), True, True, TConsole.COLOR_BLANK);

	for i := 1 to 31 do
	begin
		Sam := Module.Samples[i-1];
		if Sam = nil then Continue;

		Console.Write(Format('%.2d', [i]), Rect.Left, Rect.Top+i-1, 0);
		Console.Write(Sam.Name, Rect.Left+3, Rect.Top+i-1, ColorFore);

		if (i = CurrentSample) then
		begin
			for x := 0 to LENGTH_SAMPLETEXT-1 do
				Console.SetColor(Rect.Left+3+x, Rect.Top+i-1, -1,
					Data[5].Value);

			if not Focused then
				for x := 0 to 3 do
					Console.SetColor(Rect.Left+26+x, Rect.Top+i-1, -1,
						Data[5].Value)
			else
			if CursorInPlay then
				for x := 0 to 3 do
					Console.SetColor(Rect.Left+26+x, Rect.Top+i-1,
						Data[0].Value, Data[1].Value)
			else
			begin
				Console.SetColor(Rect.Left+3+Cursor.X, Rect.Top+i-1,
					Data[0].Value, Data[1].Value);
				for x := 0 to 3 do
					Console.SetColor(Rect.Left+26+x, Rect.Top+i-1,
						Data[0].Value, Data[5].Value);
			end;
		end;

		Console.PutChar(Rect.Left + 25, Rect.Top+i-1, 168,
			TConsole.COLOR_PANEL, TConsole.COLOR_BLANK); // separator

		Console.SetColor(Rect.Left+24, Rect.Top+i-1,
			Data[4].Value); // last char in sample name

		if (CursorInPlay) and (i = CurrentSample) and (Focused) then
			col := Data[0].Value
		else
		if IsEmptySample(Sam) then
			col := Data[3].Value //14
		else
			col := Data[2].Value; //6;

		Console.Write('Play', Rect.Left+26, Rect.Top+i-1, col);
	end;

	Console.EndUpdate;
end;


end.
