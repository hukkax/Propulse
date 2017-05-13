unit MainWindow;

{$IFDEF UNIX}
	{$DEFINE DISABLE_FULLSCREEN}		// disable broken SDL fullscreen mode
	{.$DEFINE LIMIT_KEYBOARD_EVENTS}	// fix duplicate keyboard events on Linux with FCITX
{$ENDIF}

interface

uses
	Classes, Types, SysUtils, Generics.Collections,
	SDL.Api.libSDL2, SDL.Api.Types, SDL.Api.Events, SDL.Api.Render, SDL.Api.Keyboard, SDL.Api.Hints,
	ConfigurationManager, ShortcutManager, TextMode,
	CWE.Core, CWE.MouseCursor, CWE.Dialogs, CWE.Widgets.Text,
	ProTracker.Util, ProTracker.Player, ProTracker.Editor;

type
	GlobalKeyNames = (
		keyNONE,				keyMainMenu,
		keyProgramQuit,			keyProgramFullscreen,
		keyScreenHelp,			keyScreenPatternEditor,
		keyScreenSamples,		keyScreenAbout,
		keyScreenLoad,			keyScreenSave,
		keyScreenOrderList,		keyScreenLog,
		keyScreenLayout,		keyScreenConfig,
		keyPlaybackSong,		keyPlaybackPattern,
		keyPlaybackPlayFrom,	keyPlaybackStop,
		keyPlaybackPrevPattern, keyPlaybackNextPattern,
		keyControlsPrevious,	keyControlsNext,
		keySongLength,			keySongNew,
		keyMouseCursor,			keySaveCurrent,
		keyRenderToSample,		keyCleanup,
		keyToggleChannel1,		keyToggleChannel2,
		keyToggleChannel3,		keyToggleChannel4
	);

	TVideoInfo = record
		Renderer:		PSDL_Renderer;
		Window:			PSDL_Window;
		Texture:		PSDL_Texture;

		IsFullScreen: 	Boolean;
		HaveVSync:		Boolean;
		SyncRate:		Word;
		NextFrameTime:	UInt64;

		NewSDL: 		Boolean;
		RendererName,
		LibraryVersion:	AnsiString;
	end;

	TWindow = class
	const
		TimerInterval = 10;
	private
		{$IFDEF LIMIT_KEYBOARD_EVENTS}
		PrevKeyTimeStamp: Uint32;
		{$ENDIF}
		Screens:	TObjectList<TCWEScreen>;

		procedure 	ModuleSpeedChanged;
		procedure	ModuleOrderChanged;
		procedure 	TimerTick;

		function 	GetMaxScaling(MaxScale: Byte = 0): Byte;
		function 	SetupVideo: Boolean;
		procedure	SetFullScreen(B: Boolean);
		procedure 	InitConfiguration;

		procedure 	HandleInput;
		procedure 	ProcessMouseMovement;
		procedure	SyncTo60Hz;
		procedure	FlipFrame;
		procedure	UpdateVUMeter(Len: DWord);
		procedure 	UpdatePatternView;
	public
		Video:				TVideoInfo;
		MessageTextTimer,
		PlayTimeCounter:	Integer;

		constructor Create;
		destructor 	Destroy; override;
		procedure	Close;

		procedure	ProcessFrame;

		procedure	SetTitle(const Title: AnsiString);
		procedure 	DoLoadModule(const Filename: String);
		procedure 	PlayModeChanged;

		procedure 	DialogCallback(ID: Word; Button: TDialogButton;
					ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
		procedure	OnKeyDown(var Key: Integer; Shift: TShiftState);
		function	OnContextMenu(AddGlobal: Boolean): Boolean;
	end;


	procedure	GetModifierKey(keymod: SDL_Keymods; var Shift: TShiftState;
				keymodconst: Integer; shiftconst: TShiftStateEnum); inline;
	function 	GetShiftState: TShiftState;
	function 	TimerTickCallback(interval: Uint32; param: Pointer): UInt32; cdecl;

var
	SDL:			TSDL2Library;
	Window: 		TWindow;
	GlobalKeys: 	TKeyBindings;
	QuitFlag:		Boolean;
	Initialized:	Boolean;

implementation

uses
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	{$IFDEF BASS_DYNAMIC}
	lazdynamic_bass,
	{$ELSE}
	BASS,
	{$ENDIF}
    BuildInfo, Math, soxr,
	Screen.Editor, Screen.Samples, Screen.FileReq, Screen.FileReqSample,
	Screen.Log, Screen.Help, Screen.Config, Screen.Splash,
	Dialog.Cleanup, Dialog.ModuleInfo, Dialog.NewModule, Dialog.RenderAudio,
	CWE.MainMenu;

procedure ClearMessageQueue;
var
	InputEvent: SDL_Event;
begin
	SDL.Timer.SDL_Delay(50);
	while SDL.Events.SDL_PollEvent(@InputEvent) = SDL_TRUE do;
end;

procedure TWindow.DialogCallback(ID: Word; Button: TDialogButton;
	ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
begin
	// bail out if the originating modal dialog is still displaying
	// as some of these actions might want to display other dialogs
	if Dlg <> nil then Exit;

	if Button in [btnYes, btnOK] then
	case ID of

		ACTION_QUIT:
			QuitFlag := True;

		ACTION_LOADMODULE:
			DoLoadModule(Data);

		ACTION_NEWMODULE:
			with Editor do
			begin
				NewModule(False);
				SetSample(1);
				lblSongTitle.SetCaption(Module.Info.Title);
				lblFilename.SetCaption('');
				UpdatePatternView;
				Module.SetModified(False, True);
				if CurrentScreen = Editor then
					Paint;
			end;

		ACTION_SAVEFILE:
			FileScreen.SaveFile(False);

		ACTION_DELETEFILE:
			FileScreen.DeleteFile(False);

		ACTION_DELETEDIR:
			FileScreen.DeleteDir(False);

	end;
end;

// ==========================================================================
// Module events
// ==========================================================================

procedure PatternViewChanged;
begin
	if Options.Tracker.ITCommands then
	begin
		CmdChars := CmdCharsIT;
		Module.GetAllNoteTexts;
	end
	else
		CmdChars := CmdCharsPT;
end;

procedure ApplyAudioSettings;
begin
	Module.ApplyAudioSettings;
end;

procedure PixelScalingChanged;
begin
	ClearMessageQueue;
	MouseCursor.Erase;
	Window.SetFullScreen(Window.Video.IsFullScreen);
end;

procedure ChangeMousePointer;
begin
	MouseCursor.Show := False;

	case Options.Display.MousePointer of

		CURSOR_SYSTEM:
			SDL.Mouse.SDL_ShowCursor(SDL_Cursor_Show);

		CURSOR_CUSTOM:
			begin
				SDL.Mouse.SDL_ShowCursor(SDL_Cursor_Hide);
				MouseCursor.Show := True;
			end;

		else
			SDL.Mouse.SDL_ShowCursor(SDL_Cursor_Hide);
	end;
end;

procedure ApplyPointer;
var
	Fn: String;
begin
	if (Console.Font.Width >= 14) or (Console.Font.Height >= 14) then
		Fn := 'mouse2'
	else
		Fn := 'mouse';

	if MouseCursor <> nil then
	begin
		MouseCursor.Erase;
		MouseCursor.SetImage(Fn);
	end
	else
		MouseCursor := TMouseCursor.Create(Fn);

	ChangeMousePointer;
end;

procedure ApplyFont;
begin
	ClearMessageQueue;
	Window.SetupVideo;
	Window.SetFullScreen(Window.Video.IsFullScreen);
end;

procedure TWindow.UpdateVUMeter(Len: DWord);
var
	InModal: Boolean;
//	VUDrawn: Boolean;
begin
	// this hack will update the background screen (vumeters etc.) if a module
	// is currently playing underneath a modal dialog
	InModal := (ModalDialog.Dialog <> nil) and (Module.PlayMode <> PLAY_STOPPED);
	if InModal then
		CurrentScreen := ModalDialog.PreviousScreen;

	if CurrentScreen = Editor then
		Editor.UpdateVUMeter(Len)
	else
	if CurrentScreen = SampleScreen then
		SampleScreen.UpdateVUMeter
	else
	if CurrentScreen = SampleRequester then
		SampleRequester.Waveform.Paint;
	{else
	if (InModal) and (CurrentScreen = SplashScreen) then
		SplashScreen.Update;}

	if InModal then
	begin
		CurrentScreen := ModalDialog.Dialog;
		Console.Bitmap.FillRect(Console.GetPixelRect(CurrentScreen.Rect),
			Console.Palette[TConsole.COLOR_PANEL]);
		CurrentScreen.Paint;
	end;
end;

procedure TWindow.UpdatePatternView;
begin
	if CurrentScreen = Editor then
	with Editor do
	begin
		if FollowPlayback then
		begin
			PatternEditor.ScrollPos := Max(Module.PlayPos.Row - 16, 0);
			CurrentPattern := Module.PlayPos.Pattern;
		end;
		UpdateInfoLabels;
		PatternEditor.Paint;
	end;
end;

procedure TWindow.ModuleSpeedChanged;
begin
	Editor.UpdateInfoLabels;
end;

procedure TWindow.ModuleOrderChanged;
begin
	if CurrentScreen = Editor then
	begin
		OrderList.Paint;
		UpdatePatternView;
	end;
end;

procedure TWindow.PlayModeChanged;
var
	S: string;
begin
	if CurrentScreen <> Editor then Exit;

	case Module.PlayMode of

		PLAY_PATTERN:
			begin
				S := #16 + ' Pattern';
				//C := 11;
			end;

		PLAY_SONG:
			begin
				S := #16 + ' Song';
				//C := 0;
			end;

		else
			// PLAY_STOPPED:
			S := ''; // #219;//+ ' Stopped';
			//C := 1;
		end;

	Editor.lblPlayMode.ColorFore := 3; // C;
	Editor.lblPlayMode.SetCaption(S);

	Editor.Paint;
	UpdatePatternView;
end;

procedure TWindow.DoLoadModule(const Filename: String);

	procedure ResetModule;
	begin
		if Assigned(Module) then
			Module.Free;

		Module := TPTModule.Create(False);

		Module.OnSpeedChange := ModuleSpeedChanged;
		Module.OnPlayModeChange := PlayModeChanged;
		Module.OnModified := PatternEditor.SetModified;

		PatternViewChanged;
	end;

var
	OK: Boolean;
begin
	ResetModule;

	if Filename <> '' then
		OK := Module.LoadFromFile(Filename)
	else
		OK := True;

	CurrentOrder := 0;
	CurrentPattern := Module.OrderList[0];
	CurrentSample := 1;
	Editor.Reset;
	Module.SetModified(False, True);

	if not OK then
	begin
		Log('');
		ChangeScreen(TCWEScreen(LogScreen));
		ResetModule;
	end
	else
	begin
		ChangeScreen(TCWEScreen(Editor));

		Editor.SetSample(1);
		Editor.lblSongTitle.SetCaption(Module.Info.Title, True);
		Editor.lblFilename.SetCaption(ExtractFilename(Filename), True);

		Editor.Paint;

		if Module.Warnings then
			ChangeScreen(TCWEScreen(LogScreen))
	end;
end;

function TWindow.SetupVideo: Boolean;

	function SetHint(const Hint: AnsiString; Val: Boolean): Boolean;
	var
		bs: AnsiString;
	begin
		if Val then bs := '1' else bs := '0';
		Result := (SDL.Hints.SDL_SetHint(PAnsiChar(Hint), PAnsiChar(bs)) = SDL_TRUE);
		if not Result then
			LogIfDebug('Failed to set SDL hint "' + Hint + '"!');
	end;

	function GetFontFile(const Fn: String): String;
	begin
		Result := 'font/' + Fn + '.pcx';
	end;

var
	dm: SDL_DisplayMode;
	windowFlags: SDL_WindowFlags;
	rendererFlags: SDL_RendererFlags;
	screenW, screenH, sx, sy: Word;
	Icon: PSDL_Surface;
	Fn: String;
	rinfo: SDL_RendererInfo;
	sdlVersion: SDL_Version;
	OK: Boolean;
begin
    Result := False;
	Locked := True;

	Fn := GetDataFile(GetFontFile(Options.Display.Font));
	if Fn = '' then
	begin
		Options.Display.Font := FILENAME_DEFAULTFONT;
		Fn := GetDataFile(GetFontFile(FILENAME_DEFAULTFONT));
	end;

	if not Initialized then
	begin
		OK := (Fn <> '');
		if OK then
			Console := TConsole.Create(80, 45, GetFontFile(Options.Display.Font),
				GetDataFile('palette/Propulse.ini'), OK);
		if not OK then
		begin
			LogFatal('Error initializing console emulation!');
			LogFatal('Probably the file "' + Options.Display.Font + '" couldn''t be found.');
			Exit;
		end;
	end
	else
	begin
		sx := Console.Font.Width;
		sy := Console.Font.Height;
		Console.LoadFont('font/' + Options.Display.Font);
		if (sx <> Console.Font.Width) or (sy <> Console.Font.Height) then
		begin
			ApplyPointer;
			SplashScreen.Init;
		end;
	end;

	screenW := Console.Bitmap.Width;
	screenH := Console.Bitmap.Height;
	sx := screenW * Options.Display.Scaling;
	sy := screenH * Options.Display.Scaling;

	if not Initialized then
	begin
		SDL.Version.SDL_GetVersion(sdlVersion);
		Video.NewSDL := sdlVersion.patch >= 5; // we want SDL 2.0.5 or newer
		Video.LibraryVersion := Format('%d.%d.%d',
			[sdlVersion.major, sdlVersion.minor, sdlVersion.patch]);
		LogIfDebug('Loaded SDL ' + Video.LibraryVersion);
	end;

	windowFlags.Value := 0;
	rendererFlags.Value := UInt32(SDL_RENDERER_ACCELERATED or SDL_RENDERER_TARGETTEXTURE);

	if Video.NewSDL then
		SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, True);
	SetHint(SDL_HINT_TIMER_RESOLUTION, True);
	SetHint(SDL_HINT_VIDEO_HIGHDPI_DISABLED, True);
	SetHint(SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4, True);

	{$IFDEF UNIX}
	SetHint('SDL_VIDEO_X11_XRANDR',   False);
	SetHint('SDL_VIDEO_X11_XVIDMODE', True);
	{$ENDIF}

	if not Initialized then
	begin
		if SDL.SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER) <> 0 then
		begin
			LogFatal('Error initializing SDL: ' + SDL.Error.SDL_GetError);
			Exit;
		end;

		SDL.Threads.SDL_SetThreadPriority(SDL_THREAD_PRIORITY_HIGH);
		SDL.Hints.SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'nearest');
	end
	else
	begin
		SDL.Render.SDL_DestroyRenderer(Video.Renderer);
		SDL.Render.SDL_DestroyTexture(Video.Texture);
		SDL.Video.SDL_DestroyWindow(Video.Window);
	end;

    Video.HaveVSync := False;
	Video.SyncRate := 0;

	if Options.Display.VSyncMode <> VSYNC_OFF then
	begin
		if SDL.Video.SDL_GetDesktopDisplayMode(0, dm) = 0 then
			Video.SyncRate := dm.refresh_rate
		else
			Log('GetDesktopDisplayMode failed: ' + SDL.Error.SDL_GetError);
		if (Options.Display.VSyncMode = VSYNC_FORCE) or
			(Video.SyncRate in [50..61]) then
		begin
			rendererFlags.Value := rendererFlags.Value or UInt32(SDL_RENDERER_PRESENTVSYNC);
			Video.HaveVSync := True;
		end;
	end;

	windowFlags.Value := UInt32(SDL_WINDOW_SHOWN);

	Video.Window := SDL.Video.SDL_CreateWindow('Propulse Tracker',
		SDL.Video.SDL_WINDOWPOS_CENTERED, SDL.Video.SDL_WINDOWPOS_CENTERED,
		sx, sy, windowFlags);
	if Video.Window = nil then
	begin
		LogFatal('Error setting up window: ' + SDL.Error.SDL_GetError);
		Exit;
	end;

	// make sure not to exceed display bounds
	sx := GetMaxScaling;
	if sx <> Options.Display.Scaling then
	begin
		sy := screenH * sx;
		sx := screenW * sx;
		SDL.Video.SDL_SetWindowSize(Video.Window, sx, sy);
		SDL.Video.SDL_SetWindowPosition(Video.Window,
			SDL.Video.SDL_WINDOWPOS_CENTERED, SDL.Video.SDL_WINDOWPOS_CENTERED);
	end;

	Video.Renderer := SDL.Render.SDL_CreateRenderer(Video.Window, -1, rendererFlags);
	if (Video.Renderer = nil) and (Video.HaveVSync) then
	begin
		// try again without vsync flag
        Video.HaveVSync := False;
		rendererFlags.Value := rendererFlags.Value and not UInt32(SDL_RENDERER_PRESENTVSYNC);
		Video.Renderer := SDL.Render.SDL_CreateRenderer(Video.Window, -1, rendererFlags);
		if Video.Renderer = nil then
		begin
			LogFatal('Error creating renderer: ' + SDL.Error.SDL_GetError);
			Exit;
		end;
	end;
	SDL.Render.SDL_SetRenderDrawBlendMode(Video.Renderer, SDL_BLENDMODE_NONE);

	SDL.Render.SDL_GetRendererInfo(Video.Renderer, @rinfo);
	Video.RendererName := rinfo.name;

	if SDL.Render.SDL_RenderSetLogicalSize(Video.Renderer, screenW, screenH) <> 0 then
	begin
		LogFatal('Error setting renderer size: ' + SDL.Error.SDL_GetError);
		Exit;
	end;
	{$IFNDEF DISABLE_SDL2_2_0_5}
    if Video.NewSDL then
		SDL.Render.SDL_RenderSetIntegerScale(Video.Renderer, SDL_TRUE);
	{$ENDIF}

	Video.Texture := SDL.Render.SDL_CreateTexture(Video.Renderer,
		SDL_UInt32(SDL_PIXELFORMAT_ARGB8888), SDL_SInt32(SDL_TEXTUREACCESS_STREAMING), screenW, screenH);
	if Video.Texture = nil then
	begin
		LogFatal('Error initializing streaming texture: ' + SDL.Error.SDL_GetError);
		Exit;
	end;
	SDL.Render.SDL_SetTextureBlendMode(Video.Texture, SDL_BLENDMODE_NONE);

	Fn := GetDataFile('icon.bmp');
	if Fn <> '' then
	begin
		Icon := SDL.Surface.SDL_LoadBMP(PAnsiChar(Fn));
		SDL.Video.SDL_SetWindowIcon(Video.Window, Icon);
		SDL.Surface.SDL_FreeSurface(Icon);
	end;

	if Initialized then
	begin
		Console.Refresh;
		if CurrentScreen <> nil then
		begin
			CurrentScreen.Show;
			CurrentScreen.Paint;
		end;
	end;

	Video.NextFrameTime := Trunc(SDL.Timer.SDL_GetPerformanceCounter +
		((SDL.Timer.SDL_GetPerformanceFrequency / 60.0) + 0.5));

	Result := True;
	Locked := False;
end;

procedure TWindow.FlipFrame;
begin
	if Locked then Exit;

	if CurrentScreen = SplashScreen then
		SplashScreen.Update;

	MouseCursor.Draw;

	with SDL.Render do
	begin
		SDL_UpdateTexture(Video.Texture, nil, @Console.Bitmap.Bits[0], Console.Bitmap.Width*4);
		SDL_RenderClear(Video.Renderer);
		SDL_RenderCopy(Video.Renderer, Video.Texture, nil, nil);
		SDL_RenderPresent(Video.Renderer);
	end;

	MouseCursor.Erase;
end;

function TWindow.GetMaxScaling(MaxScale: Byte = 0): Byte;
var
	w, h: Integer;
	R: SDL_Rect;
begin
	if MaxScale = 0 then MaxScale := Max(Options.Display.Scaling, 1);
	with SDL.Video do
	begin
		{$IFNDEF DISABLE_SDL2_2_0_5}
		if Video.NewSDL then
			SDL_GetDisplayUsableBounds(SDL_GetWindowDisplayIndex(Video.Window), R)
		else
		{$ENDIF}
			SDL_GetDisplayBounds(SDL_GetWindowDisplayIndex(Video.Window), R);
	end;
	repeat
		w := Console.Bitmap.Width  * MaxScale;
		h := Console.Bitmap.Height * MaxScale;
		if (w <= R.w) and (h <= R.h) then Break;
		Dec(MaxScale);
	until MaxScale <= 1;
	Result := Max(MaxScale, 1);
end;

procedure TWindow.SetFullScreen(B: Boolean);
var
	w, h: Integer;
	X, Y: SDL_Float;
	{$IFDEF DISABLE_FULLSCREEN}
	R: SDL_Rect;
    {$ENDIF}
begin
	Locked := True;
    Video.IsFullScreen := B;

	with SDL.Video do
	if B then
	begin
    	{$IFNDEF DISABLE_FULLSCREEN}
		SDL.Video.SDL_SetWindowFullscreen(Video.Window, SDL_WINDOW_FULLSCREEN_DESKTOP);
		SDL.Video.SDL_SetWindowGrab(Video.Window, SDL_TRUE);
    	{$ELSE}
		if Video.NewSDL then
	        SDL_GetDisplayUsableBounds(SDL_GetWindowDisplayIndex(Video.Window), R)
		else
			SDL_GetDisplayBounds(SDL_GetWindowDisplayIndex(Video.Window), R);
        SDL_SetWindowSize(Video.Window, R.w, R.h);
		SDL_SetWindowPosition(Video.Window,
			SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
        {$ENDIF}
	end
	else
	begin
		h := GetMaxScaling;
		w := Console.Bitmap.Width  * h;
		h := Console.Bitmap.Height * h;

		SDL_SetWindowFullscreen(Video.Window, SDL_WINDOW_WINDOWED);
		SDL_SetWindowSize(Video.Window, w, h);
		SDL_SetWindowPosition(Video.Window,
			SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
		SDL_SetWindowGrab(Video.Window, SDL_FALSE);
	end;

	SDL.Render.SDL_RenderGetScale(Video.Renderer, X, Y);
	w := Max(Trunc(x), 1); h := Max(Trunc(y), 1);
	MouseCursor.Scaling := Types.Point(w, h);

	{$IFNDEF DISABLE_SDL2_2_0_5}
	if Video.NewSDL then
		SDL.Video.SDL_SetWindowInputFocus(Video.Window);
	{$ENDIF}

    ClearMessageQueue;
    Locked := False;
end;

procedure TWindow.SetTitle(const Title: AnsiString);
begin
	if Initialized then
		SDL.Video.SDL_SetWindowTitle(Video.Window, PAnsiChar(Title));
end;

procedure TWindow.OnKeyDown(var Key: Integer; Shift: TShiftState);
var
	S: AnsiString;
	InModalDialog: Boolean;
begin
	if (CurrentScreen = nil) then Exit;

	if (CurrentScreen.KeyDown(Key, Shift)) then
	begin
		Key := 0;
		Exit;
	end;

	InModalDialog := (ModalDialog.Dialog <> nil);

	case GlobalKeyNames(Shortcuts.Find(GlobalKeys, Key, Shift))
	of
		keyNONE:
			Exit;

		keyMainMenu:
			ContextMenu.Show;

		// exit program
		keyProgramQuit:
			Close;

		keyScreenConfig:
			ChangeScreen(TCWEScreen(ConfigScreen));

		keyScreenAbout:
			ChangeScreen(TCWEScreen(SplashScreen));

		keyScreenHelp:
			if not InModalDialog then
			with PatternEditor.Cursor do
				if Column >= COL_COMMAND then
				begin
					if (Note.Command = 0) and (Note.Parameter = 0) then
						S := 'No effect'
					else
						S := EffectHints[Note.Command];
					if Note.Command = $E then
						S := S + ExtEffectHints[Note.Parameter shr 4];
					Editor.MessageText(Format('%x%.2x %s', [Note.Command, Note.Parameter, S]));
				end
				else
					Help.Show(CurrentScreen.ID);

		keyScreenPatternEditor:
			if not InModalDialog then
			begin
				FollowPlayback := False;
				Editor.ActiveControl := PatternEditor;
				ChangeScreen(TCWEScreen(Editor));
			end;

		keyScreenOrderList:
			if not InModalDialog then
			begin
				Editor.ActiveControl := OrderList;
				ChangeScreen(TCWEScreen(Editor));
			end;

		keyScreenSamples:
			if not InModalDialog then
			begin
				SampleScreen.Waveform.Sample := Module.Samples[CurrentSample-1];
				ChangeScreen(TCWEScreen(SampleScreen));
				SampleScreen.UpdateSampleInfo;
			end;

		keyPlaybackSong:
			if not InModalDialog then
			begin
				if Module.PlayMode = PLAY_STOPPED then
					Module.Play;
				ChangeScreen(TCWEScreen(Editor));
				FollowPlayback := True;
				PlayTimeCounter := 0;
			end;

		keyPlaybackPattern:
			if not InModalDialog then
			begin
				FollowPlayback := False;
				Module.PlayPattern(CurrentPattern);
				PlayTimeCounter := 0;
			end;

		keyPlaybackPlayFrom:
			if not InModalDialog then
			begin
				Key := Integer(SDLK_F7); // dumb hack
				CurrentScreen.KeyDown(Key, Shift);
				PlayTimeCounter := 0;
			end;

		keyPlaybackStop:
			if not InModalDialog then
			begin
				Module.Stop;
				PlayTimeCounter := 0;
			end;

		keyScreenLoad:
			if not InModalDialog then
				FileRequester.Show(False, Options.Dirs.Modules);

		keyScreenSave:
			FileRequester.Show(True, Options.Dirs.Modules);

		keySaveCurrent:
			if not InModalDialog then
				PatternEditor.SaveModule;

		// toggle fullscreen with alt-enter
		keyProgramFullscreen:
				SetFullScreen(not Video.IsFullScreen);

		keyScreenLog:
			ChangeScreen(TCWEScreen(LogScreen));

		keyPlaybackPrevPattern:
			if not InModalDialog then
				Editor.SelectPrevPattern;

		keyPlaybackNextPattern:
			if not InModalDialog then
				Editor.SelectNextPattern;

		keySongNew:
			if not InModalDialog then
				NewModule(True);

		keyCleanup:
			if not InModalDialog then
				Dialog_Cleanup;

		keySongLength:
			if not InModalDialog then
				Dialog_ModuleInfo;

		keyRenderToSample:
			if not InModalDialog then
				Dialog_Render(True);

		keyToggleChannel1:	Editor.ToggleChannel(0);
		keyToggleChannel2:	Editor.ToggleChannel(1);
		keyToggleChannel3:	Editor.ToggleChannel(2);
		keyToggleChannel4:	Editor.ToggleChannel(3);

		keyMouseCursor:
			begin
				Inc(Options.Display.MousePointer);
				if Options.Display.MousePointer > CURSOR_NONE then
					Options.Display.MousePointer := 0;
				case Options.Display.MousePointer of
					CURSOR_SYSTEM:
						Editor.MessageText('Hardware mouse cursor enabled');
					CURSOR_CUSTOM:
						Editor.MessageText('Software mouse cursor enabled');
					CURSOR_NONE:
						Editor.MessageText('Mouse disabled');
				end;
				ChangeMousePointer;
			end;

	end;

	Key := 0; // fix F10
end;

procedure TWindow.ProcessMouseMovement;
var
	P: TPoint;
	X, Y: Integer;
begin
	if Locked then Exit;

	SDL.Events.SDL_PumpEvents;
	SDL.Mouse.SDL_GetMouseState(X, Y);

	X := X div MouseCursor.Scaling.X;
	Y := Y div MouseCursor.Scaling.Y;
	MouseCursor.Pos := Types.Point(X, Y);
	P := Types.Point(X div Console.Font.Width, Y div Console.Font.Height);

	if (CurrentScreen <> nil) and
		((X <> MouseCursor.OldPos.X) or (Y <> MouseCursor.OldPos.Y)) then
	begin
		MouseCursor.OldPos := MouseCursor.Pos;
		CurrentScreen.MouseMove(X, Y, P);
	end;
end;

procedure GetModifierKey(keymod: SDL_Keymods; var Shift: TShiftState;
	keymodconst: Integer; shiftconst: TShiftStateEnum);
begin
	if (Integer(keymod.Value) and keymodconst) <> 0 then
		Include(Shift, shiftconst);
end;

function GetShiftState: TShiftState;
var
	M: SDL_Keymods;
begin
	Result := [];
	M := SDL.Keyboard.SDL_GetModState;
	GetModifierKey(M, Result, KMOD_SHIFT,	ssShift);	// Shift
	GetModifierKey(M, Result, KMOD_CTRL,	ssCtrl);	// Ctrl
	GetModifierKey(M, Result, KMOD_ALT,		ssAlt);		// Alt
	GetModifierKey(M, Result, Integer(KMOD_MODE),	ssAltGr);	// AltGr
	GetModifierKey(M, Result, KMOD_GUI,		ssMeta);	// Windows
	//GetModifierKey(M, Result, KMOD_NUM,	ssNum);		// Num Lock
	GetModifierKey(M, Result, Integer(KMOD_CAPS),	ssCaps);	// Caps Lock
end;

function RemoveDiacritics(const S: String): String;
var
	F: Boolean;
	I: SizeInt;
	PS, PD: PChar;
begin
	SetLength(Result, Length(S));
	PS := PChar(S);
	PD := PChar(Result);
	I := 0;
	while PS^ <> #0 do
	begin
		F := PS^ = #195;
		if F then
		case PS[1] of
			#128..#134:			PD^ := 'A';
			#135:				PD^ := 'C';
			#136..#139:			PD^ := 'E';
			#140..#143:			PD^ := 'I';
			#144:				PD^ := 'D';
			#145:				PD^ := 'N';
			#146..#150, #152:	PD^ := 'O';
			#151:				PD^ := 'x';
			#153..#156:			PD^ := 'U';
			#157:				PD^ := 'Y';
			#158:				PD^ := 'P';
			#159:				PD^ := 's';
			#160..#166:			PD^ := 'a';
			#167:				PD^ := 'c';
			#168..#171:			PD^ := 'e';
			#172..#175:			PD^ := 'i';
			#176:				PD^ := 'd';
			#177:				PD^ := 'n';
			#178..#182, #184:	PD^ := 'o';
			#183:				PD^ := '-';
			#185..#188:			PD^ := 'u';
			#190:				PD^ := 'p';
			#189, #191:			PD^ := 'y';
		else
			F := False;
		end;
		if F then
			Inc(PS)
		else
			PD^ := PS^;
		Inc(I); Inc(PD); Inc(PS);
	end;
	SetLength(Result, I);
end;

procedure TWindow.HandleInput;
var
	InputEvent: SDL_Event;
	c: SDL_SInt32;
	X, Y: Integer;
	B: Boolean;
	Btn: TMouseButton;
	Key: SDL_KeyCode;
	km: SDL_KeyMods;
	Shift: TShiftState;
	AnsiInput: AnsiString;

	function GetXY: TPoint;
	begin
		Result := Types.Point(
			MouseCursor.Pos.X div Console.Font.Width,
			MouseCursor.Pos.Y div Console.Font.Height);
	end;

begin
	if Locked then Exit;

	X := MouseCursor.Pos.X;
	Y := MouseCursor.Pos.Y;

	while SDL.Events.SDL_PollEvent(@InputEvent) = SDL_TRUE do
	case {%H-}InputEvent._type of

		SDL_USEREVENT_EV:		// messages from playroutine
		begin
			c := InputEvent.user.code;
			if c = MSG_VUMETER then
				UpdateVUMeter(PtrInt(InputEvent.user.data1))
			else
			if c = MSG_ROWCHANGE then
				UpdatePatternView
			else
			if c = MSG_ORDERCHANGE then
				ModuleOrderChanged
			else
			if c = MSG_TIMERTICK then
				TimerTick;
		end;

		SDL_KEYDOWN:
		begin
			{$IFDEF LIMIT_KEYBOARD_EVENTS}
			if (InputEvent.key.timestamp - PrevKeyTimeStamp) > 4 then
			{$ENDIF}
			begin
				Key := InputEvent.key.keysym.sym;
				case Key of
					SDLK_UNKNOWN,
					SDLK_LSHIFT, SDLK_RSHIFT,
					SDLK_LCTRL,  SDLK_RCTRL,
					SDLK_LALT,   SDLK_RALT:
					;
				else
					Shift := [];
					if InputEvent.key.keysym.amod <> KMOD_NONE then
					begin
						km := InputEvent.key.keysym.amod;
						GetModifierKey(km, Shift, KMOD_SHIFT,ssShift);	// Shift
						GetModifierKey(km, Shift, KMOD_CTRL,	ssCtrl);	// Ctrl
						GetModifierKey(km, Shift, KMOD_ALT,	ssAlt);		// Alt
						GetModifierKey(km, Shift, KMOD_GUI,	ssMeta);	// Windows
						GetModifierKey(km, Shift, Integer(KMOD_CAPS),	ssCaps);	// Caps Lock
						GetModifierKey(km, Shift, Integer(KMOD_MODE),	ssAltGr);	// AltGr
					end;
					OnKeyDown(Integer(Key), Shift);
				end;
			end;
			{$IFDEF LIMIT_KEYBOARD_EVENTS}
			PrevKeyTimeStamp := InputEvent.key.timestamp;
			{$ENDIF}
		end;

        SDL_TEXTINPUT:
			if InputEvent.text.text[0] <> #0 then
			begin
				AnsiInput := RemoveDiacritics(InputEvent.text.text);
				if Length(AnsiInput) > 0 then
					CurrentScreen.TextInput(AnsiInput[1]);
			end;

		SDL_MOUSEBUTTONDOWN,
		SDL_MOUSEBUTTONUP:
			if CurrentScreen <> nil then
			begin
				case InputEvent.button.button of
					SDL_BUTTON_LEFT:	Btn := mbLeft;
					SDL_BUTTON_MIDDLE:	Btn := mbMiddle;
					SDL_BUTTON_RIGHT:	Btn := mbRight;
				else
					Btn := mbLeft;
				end;

				if InputEvent._type = SDL_MOUSEBUTTONDOWN then
				begin
					if PtInRect(CurrentScreen.Rect, GetXY) then
					begin
						SDL.Video.SDL_SetWindowGrab(Video.Window, SDL_TRUE);
						B := CurrentScreen.MouseDown(Btn, X, Y, GetXY);
						// right button for context menu if the button wasn't otherwise handled
						if (Btn = mbRight) and (not B) and (ModalDialog.Dialog = nil) then
							ContextMenu.Show;
					end
					else
					// close context menu by clicking outside it
					if (ModalDialog.Dialog <> nil) and (ModalDialog.ID = DIALOG_CONTEXTMENU) and
						(CurrentScreen = ModalDialog.Dialog) then
							ModalDialog.Close;
				end
				else
				if InputEvent._type = SDL_MOUSEBUTTONUP then
				begin
					SDL.Video.SDL_SetWindowGrab(Video.Window, SDL_FALSE);
					CurrentScreen.MouseUp(Btn, X, Y, GetXY);
				end;
			end;

		{SDL_MOUSEMOTION:
			//if (not DisableInput) then
			if (CurrentScreen <> nil) and (Initialized) then
				CurrentScreen.MouseMove(X, Y, GetXY);}

		SDL_MOUSEWHEEL:
			if CurrentScreen <> nil then
				CurrentScreen.MouseWheel([], InputEvent.wheel.y, GetXY);

		SDL_WINDOWEVENT_EV:
			case InputEvent.window.event of
		        SDL_WINDOWEVENT_ENTER:	MouseCursor.InWindow := True;
		        SDL_WINDOWEVENT_LEAVE:	MouseCursor.InWindow := False;
			end;

		SDL_DROPFILE:
			DoLoadModule(InputEvent.drop._file);

		SDL_QUITEV:
			Close;

	end;
end;

procedure TWindow.SyncTo60Hz; 				// from PT clone
var
	delayMs, perfFreq, timeNow_64bit: UInt64;
begin
	if (Video.HaveVSync) or (Locked) then Exit;

	perfFreq := SDL.Timer.SDL_GetPerformanceFrequency; // should be safe for double
	if perfFreq = 0 then Exit; // panic!

	timeNow_64bit := SDL.Timer.SDL_GetPerformanceCounter;
	if Video.NextFrameTime > timeNow_64bit then
	begin
		delayMs := Trunc((Video.NextFrameTime - timeNow_64bit)
			* (1000.0 / perfFreq) + 0.5);
		SDL.Timer.SDL_Delay(delayMs);
	end;
	Inc(Video.NextFrameTime, Trunc(perfFreq / 60 + 0.5));
end;

function TimerTickCallback(interval: Uint32; param: Pointer): UInt32; cdecl;
var
	event: SDL_Event;
begin
	if (Initialized) and (not Locked) then
	begin
		event._type := SDL_USEREVENT_EV;
		event.user.code := MSG_TIMERTICK;
	    SDL.Events.SDL_PushEvent(@event);
	end;
	Result := interval;
end;

procedure TWindow.TimerTick;
begin
	if Locked then Exit;

	if MessageTextTimer >= 0 then
	begin
		Dec(MessageTextTimer);
		if MessageTextTimer < 0 then
			Editor.MessageText('');
	end;

	Dec(PlayTimeCounter);
	if PlayTimeCounter <= 0 then
	begin
		PlayTimeCounter := 25;
		if CurrentScreen = Editor then
			Editor.UpdateTimeDisplay;
	end;

	if TimerCallback.Enabled then
	begin
		Inc(TimerCallback.Counter);
		if TimerCallback.Counter >= TimerCallback.Interval then
		begin
			TimerCallback.Counter := 0;
			if Assigned(TimerCallback.Callback) then
				TimerCallback.Callback(TimerCallback.Control, TimerCallback.ID);
		end;
	end;
end;

procedure TWindow.InitConfiguration;
var
	Cfg: TConfigurationManager;
	Sect: AnsiString;
	i: Integer;
	AudioDeviceList: array[1..10] of AnsiString;
	device: BASS_DEVICEINFO;
begin
	// Init list of audio devices
	//
	BASS_SetConfig(BASS_CONFIG_DEV_DEFAULT, 1);
	for i := Low(AudioDeviceList) to High(AudioDeviceList) do
	begin
		if BASS_GetDeviceInfo(i, device) then
			AudioDeviceList[i] := device.name
		else
			AudioDeviceList[i] := Format('%d: None', [i]);
	end;

	// Init configuration
	//
	ConfigManager := TConfigurationManager.Create;
	Cfg := ConfigManager;
	Cfg.Filename := ConfigPath + FILENAME_CONFIG;

	with Options do
	begin
		Sect := 'Editor';
		Cfg.AddBoolean(Sect, 'UseITCommands', @Tracker.ITCommands, False)
		.SetInfo('Effect commands', 0, 1,
			['ProTracker', 'Impulse Tracker'], PatternViewChanged);
		Cfg.AddBoolean(Sect, 'AltHomeEndBehavior', @Tracker.AltHomeEndBehavior, False)
		.SetInfo('Home and End keys behavior', 0, 1, ['Impulse Tracker', 'Propulse']);
		Cfg.AddBoolean(Sect, 'ShowEmptyParamZeroes', @Tracker.ShowEmptyParamZeroes, True)
		.SetInfo('Show empty command parameters as', 0, 1, ['...', '.00']);
		Cfg.AddBoolean(Sect, 'NoteB3AsInvalid', @Tracker.NoteB3Warning, False)
		.SetInfo('Consider note B-3 as invalid', 0, 1, ['No', 'Yes']);

		Sect := 'Program';
		Cfg.AddBoolean(Sect, 'HighPriority', @HighPriority, True)
		{$IFDEF WINDOWS}
		.SetInfo('Task priority', 0, 1, ['Normal', 'High'])
		{$ENDIF};

		Sect := 'Display';
		Cfg.AddByte(Sect, 'Scaling', @Display.Scaling, 2)
		.SetInfo('Maximum scale factor', 1, 9, [], PixelScalingChanged);
		Cfg.AddByte(Sect, 'Vsync', @Display.VSyncMode, VSYNC_AUTO)
		.SetInfo('Vertical sync', VSYNC_AUTO, VSYNC_OFF, ['Auto', 'Force on', 'Off']);
		Cfg.AddString(Sect, 'Font', @Display.Font, FILENAME_DEFAULTFONT)
		.SetInfoFromDir('Font', DataPath + 'font/', '*.pcx', ApplyFont);
		{Cfg.AddString(Sect, 'Palette', @Display.Palette, 'Propulse').
		SetInfoFromDir('Palette', ConfigPath + 'palette/', '*.ini', ApplyPalette);}
		Cfg.AddByte(Sect, 'Mouse', @Display.MousePointer, CURSOR_CUSTOM)
		.SetInfo('Mouse pointer', CURSOR_SYSTEM, CURSOR_NONE,
		['System', 'Software', 'Hidden'], ChangeMousePointer);
		Cfg.AddBoolean(Sect, 'SampleAsBytes', @Display.SampleAsBytes, True)
		.SetInfo('Show sample sizes/offsets in', 0, 1, ['Words', 'Bytes']);
		Cfg.AddBoolean(Sect, 'SampleAsDecimal', @Display.SizesAsDecimal, True)
		.SetInfo('Show sizes/offsets as', 0, 1, ['Hexadecimal', 'Decimal']);
		Cfg.AddBoolean(Sect, 'RawFileSizes', @Dirs.RawFileSizes, False)
		.SetInfo('Show filesizes as', 0, 1, ['Kilobytes', 'Bytes']);
		{ Cfg.AddBoolean(Sect, 'ShowVolumeColumn',
		  @Display.ShowVolumeColumn, True);
		  Cfg.SetInfo('Show volume column', 0, 1, ['Yes' ,'No']); }
		Cfg.AddBoolean(Sect, 'ShowSplashScreen', @Display.ShowSplashScreen, True)
		.SetInfo('Splash screen', 0, 1, ['Disabled', 'Enabled']);

		Sect := 'Audio';
		Cfg.AddByte(Sect, 'Device', @Audio.Device, 1)
		.SetInfo('Audio device', 1, 10, AudioDeviceList);
		Cfg.AddByte(Sect, 'Frequency', @Audio.Frequency, 2)
		.SetInfo('Sampling rate (Hz)', 0, 3, ['11025', '22050', '44100', '48000'], nil);
		Cfg.AddInteger(Sect, 'Buffer', @Audio.Buffer, 0)
		.SetInfo('Audio buffer (ms)', 0, 500, ['Automatic']);
		Cfg.AddFloat(Sect, 'Amplification', @Audio.Amplification, 4.00)
		.SetInfo('Amplification', 0, 10, [], ApplyAudioSettings, '', -1);
		Cfg.AddByte(Sect, 'StereoSeparation', @Audio.StereoSeparation, 15)
		.SetInfo('Stereo separation', 0, 100, ['Mono', 'Full stereo'], ApplyAudioSettings, '', 5);
		Cfg.AddBoolean(Sect, 'FilterLowPass',  @Audio.FilterLowPass, False)
		.SetInfo('Lowpass filter',  0, 1, ['Disabled', 'Enabled'], ApplyAudioSettings);
		Cfg.AddBoolean(Sect, 'FilterHighPass', @Audio.FilterHighPass, False)
		.SetInfo('Highpass filter', 0, 1, ['Disabled', 'Enabled'], ApplyAudioSettings);
		Cfg.AddBoolean(Sect, 'FilterLed',      @Audio.FilterLed, True)
		.SetInfo('LED filter',      0, 1, ['Disabled', 'Enabled'], ApplyAudioSettings);
		Cfg.AddBoolean(Sect, 'CIAMode', @Audio.CIAmode, False)
		.SetInfo('Timing mode', 0, 1, ['CIA', 'VBlank'], ApplyAudioSettings);
		Cfg.AddBoolean(Sect, 'EditorInvertLoop', @Audio.EditorInvertLoop, True)
		.SetInfo('Play EFx (Invert Loop) like', 0, 1, ['PT playroutine', 'PT editor']);

		Sect := 'Directory';
		Cfg.AddString	(Sect, 'Modules', 		@Dirs.Modules, 			AppPath);
		Cfg.AddString	(Sect, 'Samples', 		@Dirs.Samples, 			AppPath);
		Cfg.AddByte		(Sect, 'SortMode',		@Dirs.FileSortMode,   	FILESORT_NAME);
		Cfg.AddByte		(Sect, 'SortModeS',		@Dirs.SampleSortMode, 	FILESORT_NAME);

		Sect := 'Resampling';
		Cfg.AddBoolean(Sect, 'Resample.Automatic', @Import.Resampling.Enable, True)
		.SetInfo('Automatic resampling on import', 0, 1, ['Disabled', 'Enabled']);
		Cfg.AddInteger(Sect, 'Resample.From', @Import.Resampling.ResampleFrom, 29556)
		.SetInfo('Resample if sample rate exceeds', 0, 44100, []);
		Cfg.AddByte(Sect, 'Resample.To', @Import.Resampling.ResampleTo, 24)
		.SetInfo('Resample to note', 0, 35, NoteNames);
		Cfg.AddByte(Sect, 'Resample.Quality', @Import.Resampling.Quality, 4)
		.SetInfo('Resampling quality', 0, 4,
		['Quick cubic', 'Low', 'Medium', 'High', 'Very high']);
		Cfg.AddBoolean(Sect, 'Resample.Normalize', @Import.Resampling.Normalize, True)
		.SetInfo('Normalize audio levels', 0, 1, ['No', 'Yes']);
		Cfg.AddBoolean(Sect, 'Resample.HighBoost', @Import.Resampling.HighBoost, True)
		.SetInfo('Boost highs', 0, 1, ['No', 'Yes']);
	end;

	LogIfDebug('Loading configuration...');

	Cfg.Load;
end;

constructor TWindow.Create;
var
	Dir: String;
	i: Integer;
	Warnings: Boolean = False;
begin
	Initialized := False;
	QuitFlag := False;
	Locked := True;

	// Init application directories
	//
	AppPath := ExtractFilePath(ParamStr(0));
	DataPath := AppPath + 'data/';
	ConfigPath := GetAppConfigDir(False);
	if ConfigPath = '' then
		ConfigPath := DataPath;
	ConfigPath := IncludeTrailingPathDelimiter(ConfigPath);
	ForceDirectories(ConfigPath);
	DefaultFormatSettings.DecimalSeparator := '.';

	// Setup logging
	//
	LogIfDebug('============================================================');
	LogIfDebug('Propulse Tracker ' + ProTracker.Util.VERSION + ' starting...');

	// Init config
	//
	InitConfiguration;

	if Options.Dirs.Modules = '' then
		Options.Dirs.Modules := AppPath;
	if Options.Dirs.Samples = '' then
		Options.Dirs.Samples := Options.Dirs.Modules;

	// Create fake text mode console and init SDL
	//
	LogIfDebug('Setting up video...');

	SDL := TSDL2Library.Create('');
	SDL.Init;
	LogIfDebug('SDL loaded from ' + SDL.FileName);

	if not SDL.Valid then
	begin
		LogFatal('Could not initialize SDL2!');
		QuitFlag := True;
		Exit;
	end;

	if not SetupVideo then
	begin
		LogFatal('Could not initialize video!');
		QuitFlag := True;
		Exit;
	end;

	// Create screens
	//
	Screens := TObjectList<TCWEScreen>.Create(True);

	// Init config screen first to load user palettes
	//
	ConfigScreen := TConfigScreen.Create(Console, 'Propulse Configuration', 'Config');
	Screens.Add(ConfigScreen);

	// Set up logging
	//
	LogScreen := TLogScreen.Create(Console, 'Messages', 'Log');
	Screens.Add(LogScreen);

//	Progress := TProgress.Create;

	// Log startup messages
	//
	Log('');
	Log(TEXT_HEAD + 'Propulse Tracker v' + ProTracker.Util.VERSION + ' (built on ' +
		Build.CompileDate + ' ' + Build.CompileTime + ')');
	Log('');
	Log(TEXT_LIGHT + '(C) 2016-2017 hukka (Joel Toivonen)');
	Log(TEXT_LIGHT + URL);
	Log(TEXT_LIGHT + 'Contains code based on work by 8bitbubsy (Olav Sorensen)');
	Log('');

	if not Video.NewSDL then
	begin
		Log(TEXT_WARNING + 'Using an older version of SDL. (< 2.0.5)');
		Warnings := True;
		Log('');
	end;

	if Video.SyncRate = 0 then
		Dir := 'unknown'
	else
		Dir := IntToStr(Video.SyncRate);
	Dir := Format('Video: SDL %s (%s), %s renderer at %s Hz',
		[Video.LibraryVersion, ExtractFilename(SDL.FileName), Video.RendererName, Dir]);
	if Video.HaveVSync then	Dir := Dir + ' VSync';
	Log(TEXT_INIT + Dir);

	case Options.Audio.Frequency of
		0: i := 11025;
		1: i := 22050;
		2: i := 44100;
		3: i := 48000;
	else
		i := 44100;
	end;

	LogIfDebug('Initializing audio...');
	if not AudioInit(i) then
	begin
	    LogFatal('Could not initialize audio; quitting!');
		QuitFlag := True;
		Exit;
	end;

	Options.Features.SOXR := (soxr_version <> '');
	if not Options.Features.SOXR then
	begin
		{$IFDEF UNIX}
	    Log(TEXT_WARNING + 'SOXR support not compiled in!');
		{$ENDIF}
	    Log(TEXT_WARNING + 'Resampling features disabled.');
		Warnings := True;
	end
	else
		Log(TEXT_INIT + 'Other: Using ' + soxr_version + ' for resampling');

	Log('');

	// Init keyboard commands now so we can log any possible errors with
	// the initialization of subsequent screens
	//
	with Shortcuts do
	begin
		GlobalKeys := SetContext('Global');

		Bind(keyMainMenu,				'Program.Menu',				'Escape');
		Bind(keyProgramQuit, 			'Program.Quit', 			'Ctrl+Q');
		Bind(keyProgramFullscreen, 		'Program.Fullscreen', 		'Alt+Return');
		Bind(keyScreenHelp, 			'Screen.Help', 				'F1');
		Bind(keyScreenPatternEditor, 	'Screen.PatternEditor', 	'F2');
		Bind(keyScreenSamples, 			'Screen.Samples', 			'F3');
		Bind(keyScreenLoad, 			'Screen.Load', 				['F9', 'Ctrl+L', 'Ctrl+O']);
		Bind(keyScreenSave, 			'Screen.Save', 				['F10', 'Ctrl+W']);
		Bind(keyCleanup, 				'Song.Cleanup',				'Ctrl+Shift+C');
		Bind(keyScreenOrderList, 		'Screen.OrderList', 		'F11');
		Bind(keyScreenLog, 				'Screen.Log', 				['F4', 'Ctrl+F11']);
		Bind(keyScreenAbout, 			'Screen.About', 			'Ctrl+F1');
		Bind(keyScreenConfig, 			'Screen.Config', 			'F12');
		Bind(keyPlaybackSong, 			'Playback.Song', 			'F5');
		Bind(keyPlaybackPattern, 		'Playback.Pattern', 		'F6');
		Bind(keyPlaybackPlayFrom, 		'Playback.PlayFrom', 		'F7');
		Bind(keyPlaybackStop, 			'Playback.Stop', 			'F8');
		Bind(keyPlaybackPrevPattern, 	'Playback.PrevPattern', 	'Ctrl+Left');
		Bind(keyPlaybackNextPattern, 	'Playback.NextPattern', 	'Ctrl+Right');
		Bind(keySongLength, 			'Song.Length', 				'Ctrl+P');
		Bind(keySongNew, 				'Song.New', 				'Ctrl+N');
		Bind(keyRenderToSample, 		'Song.RenderToSample',		'Shift+F10');
		Bind(keySaveCurrent, 			'Song.SaveCurrent', 		'Ctrl+S');
		Bind(keyMouseCursor, 			'Program.MouseCursor', 		'Ctrl+M');
		Bind(keyToggleChannel1, 		'Playback.ToggleChannel.1',	'Ctrl+1');
		Bind(keyToggleChannel2, 		'Playback.ToggleChannel.2',	'Ctrl+2');
		Bind(keyToggleChannel3, 		'Playback.ToggleChannel.3',	'Ctrl+3');
		Bind(keyToggleChannel4, 		'Playback.ToggleChannel.4',	'Ctrl+4');

		FileOpKeys := SetContext('FileOperations');

		Bind(filekeyRename,				'File.Rename',				'Shift+F2');
		Bind(filekeyCopy,				'File.Copy',				'Shift+F5');
		Bind(filekeyMove,				'File.Move',				'Shift+F6');
		Bind(filekeyDelete,				'File.Delete',				['Shift+F8', 'Delete']);
		Bind(filekeyCreate,				'File.CreateDir',			'Shift+F7');
	end;

	LogIfDebug('Initializing GUI...');
	InitCWE;

	// Create the rest of the screens
	//
	Editor := TEditorScreen.Create(Console,	'Pattern Editor', 'Editor');
	Screens.Add(Editor);

	FileRequester := TModFileScreen.Create(Console,
		'File Requester', 'File Requester');
	Screens.Add(FileRequester);
	FileScreen := FileRequester;

	SampleRequester := TSampleFileScreen.Create(Console,
		'File Requester', 'Sample File Requester');
	Screens.Add(SampleRequester);

	SampleScreen := TSampleScreen.Create(Console, 'Sample List', 'Samples');
	Screens.Add(SampleScreen);

	Help := THelpScreen.Create(Console, 'Help Viewer', 'Help');
	Screens.Add(Help);

	SplashScreen := TSplashScreen.Create(Console, '', 'Splash');
	Screens.Add(SplashScreen);

	// Init context menu
	ContextMenu := TCWEMainMenu.Create;

	// Load any user-defined shortcuts
	Shortcuts.Load(GetDataFile(FILENAME_KEYBOARD));

	Log('');

	ApplyPointer;

	ConfigScreen.Init(ConfigManager);

	DoLoadModule('');

	{$IFDEF LIMIT_KEYBOARD_EVENTS}
	PrevKeyTimeStamp := 0;
	{$ENDIF}
	MessageTextTimer := -1;
	Initialized := True;
	Module.SetModified(False);

	{$IFDEF WINDOWS}
	if Options.HighPriority then
		SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
	{$ENDIF}

	SetFullScreen(Video.IsFullScreen);

	SDL.Timer.SDL_AddTimer(TimerInterval, TimerTickCallback, nil);

	LogIfDebug('OK.');
	Log(TEXT_SUCCESS + 'Program started at ' + DateTimeToStr(Now) + '.');
	Log('-');

    if Warnings then
		ChangeScreen(TCWEScreen(LogScreen))
	else
	if Options.Display.ShowSplashScreen then
		ChangeScreen(TCWEScreen(SplashScreen))
	else
		ChangeScreen(TCWEScreen(Editor));

	Console.Paint;

	MouseCursor.InWindow := True;
end;

destructor TWindow.Destroy;
begin
	LogIfDebug('Closing down...');

	if Initialized then
	begin
		Initialized := False;

		// Save configuration
		if Shortcuts <> nil then
			Shortcuts.Save(ConfigPath + FILENAME_KEYBOARD);
		if ConfigScreen <> nil then
			ConfigScreen.SavePalette;

		if ConfigManager <> nil then
		begin
			ConfigManager.Save;
			ConfigManager.Free;
		end;

		ContextMenu.Free;
		Console.Free;
		Screens.Free;
		MouseCursor.Free;

		Module.Free;
		AudioClose;

		if Video.Renderer <> nil then
			SDL.Render.SDL_DestroyRenderer(Video.Renderer);
		if Video.Texture <> nil then
			SDL.Render.SDL_DestroyTexture(Video.Texture);
		if Video.Window <> nil then
			SDL.Video.SDL_DestroyWindow(Video.Window);
		SDL.SDL_Quit;
	end;
	SDL.Free;
end;

procedure TWindow.Close;
begin
	if Module.Modified then
	begin
		if ModalDialog.Dialog <> nil then Exit;
		ModalDialog.MessageDialog(ACTION_QUIT,
			'Quit Propulse Tracker',
			'There are unsaved changes. Discard and quit?',
			[btnOK, btnCancel], btnCancel, DialogCallback, 0)
	end
	else
		QuitFlag := True;
end;

procedure TWindow.ProcessFrame;
begin
	HandleInput;
	SyncTo60Hz;
	ProcessMouseMovement;
	FlipFrame;
end;

function TWindow.OnContextMenu(AddGlobal: Boolean): Boolean;
begin
	with ContextMenu do
	begin
		SetSection(GlobalKeys);

		if AddGlobal then
		begin
			AddSection('Module');
			AddCmd(Ord(keySongNew),					'New module');
			AddCmd(Ord(keyScreenLoad), 				'Load module');
			AddCmd(Ord(keyScreenSave), 				'Save module');
			AddCmd(Ord(keySongLength), 				'Show length/size');
			AddCmd(Ord(keyCleanup), 				'Cleanup');
			AddCmd(Ord(keyRenderToSample),			'Selection to sample');

			AddSection('Screens');
			if CurrentScreen <> Help then
				AddCmd(Ord(keyScreenHelp), 			'Help');
			if CurrentScreen <> Editor then
				AddCmd(Ord(keyScreenPatternEditor),	'Pattern editor');
			if CurrentScreen <> SampleScreen then
				AddCmd(Ord(keyScreenSamples), 		'Samples');
			if CurrentScreen <> LogScreen then
				AddCmd(Ord(keyScreenLog), 			'Message log');
			if CurrentScreen <> ConfigScreen then
				AddCmd(Ord(keyScreenConfig), 		'Configuration');
		end;

		AddSection('Program');
		AddCmd(Ord(keyProgramFullscreen), 		'Toggle fullscreen');
		AddCmd(Ord(keyScreenAbout), 			'About...');
		AddCmd(Ord(keyProgramQuit), 			'Quit');
	end;
	Result := True;
end;

end.

