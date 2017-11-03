// ==========================================================================
// Remove unused stuff from module
// ==========================================================================
unit Dialog.Cleanup;

interface

uses
	Types, Classes,
	CWE.Core,
	CWE.Dialogs;

const
	ACTION_FINDUNUSED	= 0;
	ACTION_CLEANUP 		= 1;

	SAMPLES_REMOVE		= 0;
	SAMPLES_DUPLICATES	= 1;
	SAMPLES_REARRANGE	= 2;
	SAMPLES_LOCKNAMES	= 3;
	PATTERNS_REMOVE		= 4;
	PATTERNS_DUPLICATES = 5;
	PATTERNS_REARRANGE	= 6;


	procedure Dialog_Cleanup;

// ==========================================================================

implementation

uses
	SysUtils,
	Generics.Collections,
	CWE.Widgets.Text,
	ProTracker.Player,
	ProTracker.Sample,
	Screen.Editor,
	ConfigurationManager,
	Screen.Config,
	Screen.Samples;

var
	CleanLog: 			TStrings;
	Warnings: 			Word;
	UnusedPatterns,
	UnusedSamples,
	DuplicatedPatterns,
	DuplicatedSamples:	Byte;
	PatternUsage:		array[0..MAX_PATTERNS-1] of Byte;
	SampleUsage:		array[0..30] of Cardinal;
	UnusedPattern:		array[0..MAX_PATTERNS-1] of Boolean;
	UnusedSample:		array[0..30] of Boolean;
	DuplicatePattern:	array[0..MAX_PATTERNS-1] of ShortInt;
	DuplicateSample:	array[0..30] of ShortInt;

type
	TPattern = class
		OriginalIndex: Byte;
		Channel: array[0..AMOUNT_CHANNELS-1, 0..63] of TNote;
	end;

	TCleanup = class
	private
		class function  FindUnused(Prescan: Boolean = False): Cardinal;
		class procedure Cleanup;
	public
		class procedure DialogCallback(ID: Word; Button: TDialogButton;
			ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
	end;

// ==========================================================================

procedure CLog(const S: String); inline; overload;
begin
	CleanLog.Add(S);
end;

procedure CLog(const S: String; Data: array of const); overload;
begin
	CleanLog.Add(Format(S, Data));
end;

class procedure TCleanup.DialogCallback(ID: Word; Button: TDialogButton;
	ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
begin
	if Dlg <> nil then Exit;

	if Button = btnOK then
	case ID of

		ACTION_FINDUNUSED:
			FindUnused;

		ACTION_CLEANUP:
			Cleanup;

	end;
end;

function IsPatternDuplicateOf(i1, i2: Byte): Boolean;
var
	x, y: Integer;
	N1, N2: PNote;
begin
	Result := True;
	if (i1 >= MAX_PATTERNS) or (i2 >= MAX_PATTERNS) then Exit;
	for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
		begin
			N1 := @Module.Notes[i1, x, y];
			N2 := @Module.Notes[i2, x, y];
			if (N1.Sample <> N2.Sample) or (N1.Command <> N2.Command) or
			   (N1.Pitch <> N2.Pitch) or (N1.Parameter <> N2.Parameter) then
					Exit(False);
		end;
end;

procedure ClearPattern(i: Byte);
var
	x, y: Integer;
begin
	if i < MAX_PATTERNS then
		for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
			Module.Notes[i, x, y] := EmptyNote;
end;

procedure ReplaceSampleInPattern(patt, sOld, sNew: Byte);
var
	x, y: Integer;
	Note: PNote;
begin
	if (patt >= MAX_PATTERNS) or (Module.IsPatternEmpty(patt)) then Exit;
	for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
		begin
			Note := @Module.Notes[patt, x, y];
			if Note.Sample = sOld then Note.Sample := sNew;
		end;
end;

procedure ReplaceSampleInPatterns(sOld, sNew: Byte);
var
	patt: Integer;
begin
	for patt := 0 to MAX_PATTERNS-1 do
		ReplaceSampleInPattern(patt, sOld, sNew);
end;

// ==========================================================================
// Figure out what stuff is unused in the module
// ==========================================================================

class function TCleanup.FindUnused(Prescan: Boolean = False): Cardinal;
var
	x, y, i, s, p: Integer;
	Note: PNote;
	Savings: Cardinal;
	B: Boolean;
begin
	Savings := 0; // bytes saved if unused stuff is deleted
	Warnings := 0;

	// ----------------------------------------------------------------------
	// Find unused patterns
	// ----------------------------------------------------------------------

	Module.CountUsedPatterns;

	for p := 0 to MAX_PATTERNS-1 do
	begin
		PatternUsage[p] := 0;
		UnusedPattern[p] := False;
		DuplicatePattern[p] := -1;
	end;
	UnusedPatterns := 0;
	DuplicatedPatterns := 0;

	for i := 0 to Module.Info.OrderCount-1 do
	begin
		p := Module.OrderList[i];
		Inc(PatternUsage[p]);
	end;

	i := Module.OrderList.GetHighestUsed;
	for p := 0 to MAX_PATTERNS-1 do
	begin
		// ignore empty trailing patterns
		if (p > i) then
			UnusedPattern[p] := not Module.IsPatternEmpty(p)
		else
			UnusedPattern[p] := (PatternUsage[p] = 0);
		if UnusedPattern[p] then
		begin
			Inc(UnusedPatterns);
			Inc(Savings, 1024); // a pattern is always a kilobyte long
		end;
	end;

	// find duplicate patterns
	for p := 0 to MAX_PATTERNS-1 do
	begin
		if (UnusedPattern[p]) or (DuplicatePattern[p] >= 0) then Continue;

		for s := MAX_PATTERNS-1 downto 0 do
		begin
			if (s = p) or (UnusedPattern[s]) or (DuplicatePattern[s] >= 0) or
				(Module.IsPatternEmpty(s)) then Continue;

			if IsPatternDuplicateOf(p, s) then
			begin
				DuplicatePattern[s] := p;
				Inc(DuplicatedPatterns);
				Inc(Savings, 1024);
			end;
		end;
	end;

	// ----------------------------------------------------------------------
	// Find unused samples
	// ----------------------------------------------------------------------

	for s := 0 to 30 do
	begin
		SampleUsage[s] := 0;
		UnusedSample[s] := False;
		DuplicateSample[s] := -1;
	end;

	if Prescan then
		B := True
	else
		B := DialogBooleans[PATTERNS_REMOVE];

	// iterate pattern data
	for p := 0 to Module.Info.PatternCount do
	begin
		if (B) and (UnusedPattern[p]) then
		begin
			//CLog('Skip pattern %.2d', [p]);
			Continue; // don't count samples from an unused pattern as used
		end;
		for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
		begin
			Note := @Module.Notes[p, x, y];
			if Note.Sample in [1..31] then
				Inc(SampleUsage[Note.Sample-1]);
		end;
	end;

	UnusedSamples := 0;
	DuplicatedSamples := 0;

	for s := 0 to 30 do
	begin
		if SampleUsage[s] = 0 then
		begin
			UnusedSample[s] := True;
			if not IsEmptySample(Module.Samples[s]) then
			begin
				Inc(UnusedSamples);
				Inc(Savings, Module.Samples[s].ByteLength);
			end;
		end
		else
		if IsEmptySample(Module.Samples[s]) then
			Inc(Warnings); // referenced empty sample
	end;

	// find duplicate samples
	for s := 0 to 30 do
	begin
		if (UnusedSample[s]) or (DuplicateSample[s] >= 0) or
			(IsEmptySample(Module.Samples[s])) then Continue;

		for p := 30 downto 0 do
		begin
			if (p = s) or (UnusedSample[p]) or (DuplicateSample[p] >= 0) or
				(IsEmptySample(Module.Samples[p])) then Continue;

			if Module.Samples[s].IsDuplicateOf(Module.Samples[p]) then
			begin
				DuplicateSample[p] := s;
				Inc(DuplicatedSamples);
				Inc(Savings, Module.Samples[p].ByteLength);
			end;
		end;
	end;

	Result := Savings;
end;

// ==========================================================================
// Actually remove the unused stuff
// (After running Dialog_Cleanup and FindUnused!)
// ==========================================================================

class procedure TCleanup.Cleanup;
var
	i, x, y: Integer;
	Sample: TSample;
	SampleNames: array[0..30] of AnsiString;
	NewIndex: array[0..MAX_PATTERNS-1] of Byte;
	Pattern: TPattern;
	Patterns: TObjectList<TPattern>;
label
	Done;
begin
	Module.Stop;

	// Memorize sample names if locked
	for i := 0 to 30 do
		SampleNames[i] := Module.Samples[i].Name;

	// Remove unused samples
	if DialogBooleans[SAMPLES_REMOVE] then
	begin
		for i := 0 to 30 do
			if UnusedSample[i] then
				Module.Samples[i].Clear;
	end;

	if DialogBooleans[SAMPLES_DUPLICATES] then
	begin
		for i := 0 to 30 do
			if DuplicateSample[i] >= 0 then
			begin
				ReplaceSampleInPatterns(i+1, DuplicateSample[i]+1);
				Module.Samples[i].Clear;
			end;
	end;

	// Rearrange samples
	if DialogBooleans[SAMPLES_REARRANGE] then
	begin
		for i := 0 to 30 do
		begin
			Sample := Module.Samples[i];
			if IsEmptySample(Sample) then Continue;

			for y := 0 to i-1 do
			begin
				if IsEmptySample(Module.Samples[y]) then
				begin
					//Log('* Moving sample %d to %d', [i+1, y+1]);
					SampleScreen.ExchangeSamples(i, y, True, True);
					Break;
				end;
			end;
		end;
		Module.IndexSamples;
		//Log('');
	end;

	// Restore sample names if locked
	if DialogBooleans[SAMPLES_LOCKNAMES] then
		for i := 0 to 30 do
			Module.Samples[i].SetName(SampleNames[i]);

	if DialogBooleans[PATTERNS_DUPLICATES] then
	begin
		for x := 0 to MAX_PATTERNS-1 do
		begin
			if DuplicatePattern[x] < 0 then Continue;
			for i := 0 to Module.Info.OrderCount-1 do
				if Module.OrderList[i] = x then
					Module.OrderList[i] := DuplicatePattern[x];
			UnusedPattern[x] := True;
		end;
	end;

	if 	not (DialogBooleans[PATTERNS_REMOVE]) and
		not (DialogBooleans[PATTERNS_REARRANGE]) then
			goto Done;

	Patterns := TObjectList<TPattern>.Create;

	for i := 0 to Module.Info.PatternCount do
	begin
		if (DialogBooleans[PATTERNS_REMOVE]) and (UnusedPattern[i]) then Continue;

		Pattern := TPattern.Create;
		Pattern.OriginalIndex := i;
		NewIndex[i] := Patterns.Count;

		for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
			Pattern.Channel[x, y] := Module.Notes[i, x, y];

		Patterns.Add(Pattern);
	end;

	// Remove unused patterns
	if DialogBooleans[PATTERNS_REMOVE] then
	begin
		for i := 0 to Module.Info.OrderCount-1 do
			Module.OrderList[i] := {%H-}NewIndex[Module.OrderList[i]];

		for i := 0 to Module.Info.PatternCount do
			ClearPattern(i);

		for i := 0 to Patterns.Count-1 do
		for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
			Module.Notes[i, x, y] := Patterns[i].Channel[x, y];
	end;

	// Rearrange patterns
	if DialogBooleans[PATTERNS_REARRANGE] then
	begin
	end;

	Patterns.Free;
	Module.CountUsedPatterns;

Done:
	Module.SetModified;
	CurrentScreen.Paint;
end;

// ==========================================================================
// Show the initial cleanup dialog to find out what user wants to remove
// ==========================================================================

procedure Dialog_Cleanup;
const
	W  = 46;
	H  = 11;
	CITEM = #7' ';
var
	y, LH: Integer;
	Sect: AnsiString;
	Dlg: TCWEScreen;
	List: TCWEConfigList;
	ItemList: TCWEMemo;
	Sn: AnsiString;
	Savings: Cardinal;
begin
	// ----------------------------------------------------------------------
	// Find out what's there to clear and log it to memo
	// ----------------------------------------------------------------------

	Savings := TCleanup.FindUnused(True); // initial scan for potentially cleanable stuff
	CleanLog.Clear;

	if 	(UnusedPatterns = 0) and (UnusedSamples = 0) and
		(DuplicatedSamples = 0) and (DuplicatedPatterns = 0) then
	begin
		{if Warnings = 0 then
		begin
			ModalDialog.ShowMessage('Clean Up', 'Found nothing unused to clean up.');
			Exit;
		end
		else}
			CLog('Nothing to clean up.');
	end
	else
	begin
		CLog('Potential savings: %d bytes (%f KB)', [Savings, Savings / 1024]);

		if UnusedSamples > 0 then
		begin
			CLog('');
			CLog('Module contains %d unused samples:', [UnusedSamples]);
			for y := 0 to 30 do
			begin
				if SampleUsage[y] = 0 then
				begin
					if not IsEmptySample(Module.Samples[y]) then
					begin
						Sn := Module.Samples[y].Name;
						CLog(CITEM + '%.2d "%22s" %dB', [y+1, Sn, Module.Samples[y].ByteLength]);
					end;
				end;
			end;
		end;

		if DuplicatedSamples > 0 then
		begin
			CLog('');
			CLog('Module contains %d duplicate samples:', [DuplicatedSamples]);
			for y := 0 to 30 do
				if DuplicateSample[y] >= 0 then
					CLog(CITEM + 'Sample %.2d is a duplicate of sample %.2d', [y+1, DuplicateSample[y]+1]);
		end;

		if UnusedPatterns > 0 then
		begin
			CLog('');
			CLog('Module contains %d unused patterns:', [UnusedPatterns]);
			for y := 0 to MAX_PATTERNS-1 do
				if UnusedPattern[y] then
					CLog(CITEM + 'Pattern %.2d', [y]);
		end;

		if DuplicatedPatterns > 0 then
		begin
			CLog('');
			CLog('Module contains %d duplicate patterns:', [DuplicatedPatterns]);
			for y := 0 to MAX_PATTERNS-1 do
				if DuplicatePattern[y] >= 0 then
					CLog(CITEM + 'Pattern %.2d is a duplicate of pattern %.2d', [y, DuplicatePattern[y]]);
		end;

	end;

	if Warnings > 0 then
	begin
		CLog('');
		CLog('%d warnings:', [Warnings]);
		for y := 0 to 30 do
		begin
			if (SampleUsage[y] > 0) and (IsEmptySample(Module.Samples[y])) then
				CLog(CITEM + 'Empty sample %.2d referenced %d times', [y+1, SampleUsage[y]]);
		end;
	end;

	// ----------------------------------------------------------------------
	// Create the dialog
	// ----------------------------------------------------------------------

	LH := CleanLog.Count + 1;
	if LH > 30 then LH := 30;

	Dlg := ModalDialog.CreateDialog(ACTION_CLEANUP, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - ((H+LH) div 2), W, H+LH+2),
		 'Clean Up');

	List := TCWEConfigList.Create(Dlg, '', 'Cleanup',
		Types.Rect(1, 2, W-1, 10), True);
	List.ColumnWidth[1] := 3;

	with ModalDialog do
	begin
		CreateConfigManager;

		Sect :=	Format('Samples  (%d unused)', [UnusedSamples]);

		ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[SAMPLES_REMOVE], (UnusedSamples > 0)).
			SetInfo('Remove unused samples', 0, 1, CN_YESNO);
		ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[SAMPLES_DUPLICATES], {(DuplicatedSamples > 0)}False).
			SetInfo('Remove duplicate samples', 0, 1, CN_YESNO);
		ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[SAMPLES_REARRANGE], False).
			SetInfo('Rearrange samples', 0, 1, CN_YESNO);
		ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[SAMPLES_LOCKNAMES], False).
			SetInfo('Lock sample names', 0, 1, CN_YESNO);

		Sect :=	Format('Patterns (%d unused)', [UnusedPatterns]);

		ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[PATTERNS_REMOVE], (UnusedPatterns + DuplicatedPatterns > 0)).
			SetInfo('Remove unused patterns', 0, 1, CN_YESNO);
		ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[PATTERNS_DUPLICATES], (DuplicatedPatterns > 0)).
			SetInfo('Remove duplicate patterns', 0, 1, CN_YESNO);
		{ConfigManager.AddBoolean(Sect, '',
			@DialogBooleans[PATTERNS_REARRANGE], True).
			SetInfo('Rearrange patterns', 0, 1, CN_YESNO);}

		List.Init(ConfigManager);
		List.Scrollbar.Visible := False;
		List.Border.Pixel := True;

		AddResultButton(btnOK,     ' Clean up ', 1,    H+LH, True);
		AddResultButton(btnCancel, '  Cancel  ', W-11, H+LH);
		ButtonCallback := TCleanup.DialogCallback;

		ItemList := TCWEMemo.Create(Dlg, '', 'Items',
			Types.Rect(1, H, W-1, H+LH-1));
		ItemList.Scrollbar.Visible := False;
		ItemList.Border.Pixel := True;
		ItemList.WantKeyboard := False;

		// color-code the memo in a very dumb way
		y := 0;
		for Sect in CleanLog do
		begin
			if y = 0 then
				ItemList.Add(Sect, 11)		// first line
			else
			if Pos(CITEM, Sect) = 1 then	// removable item
				ItemList.Add(Sect, 2)
			else
			if Pos('warnings:', Sect) > 0 then
				ItemList.Add(Sect, 4)		// heading for warnings
			else
				ItemList.Add(Sect, 6);		// heading for samples/patterns
			Inc(y);
		end;

		Dialog.ActiveControl := List;
		Show;
	end;
end;


initialization

	CleanLog := TStringList.Create;

finalization

	CleanLog.Free;

end.

