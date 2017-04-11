unit MainWindow;

{$mode delphi}
{$IFNDEF FPC}
type
	PtrInt  = Integer;
	PtrUInt = Cardinal;
{$ENDIF}

interface

uses
	Classes, Types, SysUtils, Generics.Collections,
	SDL2,
	ConfigurationManager, ShortcutManager,
	TextMode, CWE.Core, CWE.MouseCursor, CWE.Dialogs,
	CWE.Widgets.Text,
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

	{ TCWEMainMenu }

	TCWEMainMenu = class(TCWETwoColumnList)
		procedure MainMenuCommand(Sender: TCWEControl);
		function  KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
	end;

	{ TWindow }
	TWindow = class
	const
		TimerInterval = 10;
	private
		next60HzTime_64bit:	UInt64;

		Texture:	PSDL_Texture;
		Screens:	TObjectList<TCWEScreen>;

		procedure 	ModuleSpeedChanged;
		//procedure	ModuleRowChanged;
		procedure	ModuleOrderChanged;
		procedure 	TimerTick;

		procedure	UpdateVUMeter(Len: DWord);
		procedure 	UpdatePatternView;

		function 	SetupVideo(screenW, screenH: Word; Scale: Byte): Boolean;
	public
		Renderer:			PSDL_Renderer;
		Window:				PSDL_Window;

		IsFullScreen: 		Boolean;
		VSyncRate:			Word;
		sdlVersion: 		TSDL_version;
		MessageTextTimer,
		PlayTimeCounter:	Integer;

		constructor Create;
		destructor 	Destroy; override;
		procedure	Close;

		procedure 	HandleInput;
		procedure 	ProcessMouseMovement;
		procedure	SyncTo60Hz;
		procedure	FlipFrame;

		procedure 	EscMenu;
		procedure	SetFullScreen(B: Boolean);
		procedure	SetTitle(const Title: AnsiString);
		procedure 	DoLoadModule(const Filename: String);
		procedure 	PlayModeChanged;

		procedure 	DialogCallback(ID: Word; Button: TDialogButton;
					ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
		procedure	OnKeyDown(var Key: Integer; Shift: TShiftState);
	end;


	procedure	GetModifierKey(keymod: Integer; var Shift: TShiftState;
				keymodconst: Word; shiftconst: TShiftStateEnum); inline;
	function 	GetShiftState: TShiftState;
	function 	TimerTickCallback(interval: Uint32; param: Pointer): UInt32; cdecl;

var
	Window: 	TWindow;
	GlobalKeys: TKeyBindings;
	QuitFlag:	Boolean;
	NewSDL: 	Boolean;
	Initialized:Boolean;


implementation

uses
	{$IFDEF WINDOWS}
	Windows,
	{$ENDIF}
	BASS, BuildInfo, Math,
	Screen.Editor, Screen.Samples, Screen.FileReq, Screen.FileReqSample,
	Screen.Log, Screen.Help, Screen.Config, Screen.Splash,
	Dialog.Cleanup, Dialog.ModuleInfo, Dialog.NewModule, Dialog.RenderAudio,
	soxr;


procedure TWindow.DialogCallback(ID: Word; Button: TDialogButton;
	ModalResult: Integer; Data: Variant; Dlg: TCWEDialog);
begin
	// bail out if the originating modal dialog is still displaying
	// as some of these actions might want to display other dialogs
	if Dlg <> nil then Exit;

	if Button in [btnYes, btnOK] then
	case ID of

		ACTION_QUIT:
		begin
			Module.Modified := False;
			Close;
		end;

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
	MouseCursor.Erase;
	Window.SetFullScreen(Window.IsFullScreen);
end;

procedure ChangeMousePointer;
begin
	MouseCursor.Show := False;

	case Options.Display.MousePointer of

		CURSOR_SYSTEM:
			SDL_ShowCursor(SDL_ENABLE);

		CURSOR_CUSTOM:
			begin
				SDL_ShowCursor(SDL_DISABLE);
				MouseCursor.Show := True;
			end;

		else
			SDL_ShowCursor(SDL_DISABLE);
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
		MouseCursor.SetImage(DataPath + Fn);
	end
	else
		MouseCursor := TMouseCursor.Create(DataPath + Fn);

	ChangeMousePointer;
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

function TWindow.SetupVideo(screenW, screenH: Word; Scale: Byte): Boolean;

	function GetFontFile(const Fn: String): String;
	begin
		Result := AppPath + 'data/font/' + Fn + '.pcx';
	end;

var
	dm: TSDL_DisplayMode;
	windowFlags, rendererFlags: UInt32;
	sx, sy: Word;
	Icon: PSDL_Surface;
	Fn: String;
begin
  	Result := False;

	Fn := GetFontFile(Options.Display.Font);
	if not FileExists(Fn) then
	begin
		Options.Display.Font := FILENAME_DEFAULTFONT;
		Fn := GetFontFile(Options.Display.Font);
		if not FileExists(Fn) then Exit;
	end;

	Console := TConsole.Create(screenW, screenH, Fn, AppPath + 'palette/Propulse.ini');

	screenW := Console.Bitmap.Width;
	screenH := Console.Bitmap.Height;
	if Scale < 1 then Scale := 1;
	sx := screenW * Scale;
	sy := screenH * Scale;

	SDL_GetVersion(@sdlVersion);
	NewSDL := sdlVersion.patch >= 5; // we want SDL 2.0.5 or newer

	windowFlags   := 0;
	rendererFlags := SDL_RENDERER_ACCELERATED or SDL_RENDERER_TARGETTEXTURE;

	SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, '1'); // this was fun to figure out, not
	SDL_SetHint(SDL_HINT_TIMER_RESOLUTION, '1');
	SDL_SetHint(SDL_HINT_VIDEO_HIGHDPI_DISABLED, '1');
	SDL_SetHint(SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4, '1');

	{$IFDEF UNIX}
	SDL_SetHint('SDL_VIDEO_X11_XRANDR',   '0');
	SDL_SetHint('SDL_VIDEO_X11_XVIDMODE', '1');
	{$ENDIF}

	if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER) < 0 then Exit;

	VSyncRate := 0;
	if SDL_GetDesktopDisplayMode(0, @dm) = 0 then
	if dm.refresh_rate in [50..61] then // 59Hz is a wrong NTSC legacy value from EDID. It's 60Hz!
	begin
		VSyncRate := dm.refresh_rate;
		rendererFlags := rendererFlags or SDL_RENDERER_PRESENTVSYNC;
	end;

	SDL_SetThreadPriority(SDL_THREAD_PRIORITY_HIGH);
	SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'nearest');

	Window := SDL_CreateWindow('Propulse Tracker',
		SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, sx, sy, windowFlags);
	if Window = nil then Exit;

	Renderer := SDL_CreateRenderer(Window, -1, rendererFlags);
	if (Renderer = nil) and (VSyncRate > 0) then
	begin
		// try again without vsync flag
		VSyncRate := 0;
		rendererFlags := rendererFlags and not SDL_RENDERER_PRESENTVSYNC;
		Renderer := SDL_CreateRenderer(Window, -1, rendererFlags);
		if Renderer = nil then Exit;
	end;

	SDL_RenderSetLogicalSize(Renderer, screenW, screenH);
	SDL_SetRenderDrawBlendMode(Renderer, SDL_BLENDMODE_NONE);

    if NewSDL then
		SDL_RenderSetIntegerScale(Renderer, SDL_TRUE);

	texture := SDL_CreateTexture(Renderer,
		SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, screenW, screenH);
	if Texture = nil then Exit;

	SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_NONE);

	next60HzTime_64bit := Trunc(SDL_GetPerformanceCounter +
		((SDL_GetPerformanceFrequency / 60.0) + 0.5));

	Icon := SDL_LoadBMP(PAnsiChar(DataPath + 'icon.bmp'));
	SDL_SetWindowIcon(Window, Icon);
	SDL_FreeSurface(Icon);

	Result := True;
end;

procedure TWindow.FlipFrame;
begin
	if CurrentScreen = SplashScreen then
		SplashScreen.Update;

	ProcessMouseMovement;
	MouseCursor.Draw;

	SDL_UpdateTexture(Texture, nil, @Console.Bitmap.Bits[0], Console.Bitmap.Width*4);
	SDL_RenderClear(Renderer);
	SDL_RenderCopy(Renderer, Texture, nil, nil);
	SDL_RenderPresent(Renderer);

	MouseCursor.Erase;
end;

procedure TWindow.SetFullScreen(B: Boolean);
var
	z, w, h: Integer;
	X, Y: Single;
begin
	IsFullScreen := B;

	if B then
	begin
		{$IFDEF WINDOWS}
		SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
		{$ELSE}
		SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN);
		{$ENDIF}
		SDL_SetWindowGrab(window, SDL_TRUE);
	end
	else
	begin
		z := Options.Display.Scaling;
		{repeat
			w := Console.Bitmap.Width  * z;
			h := Console.Bitmap.Height * z;
			if (w >= Screen.
		until ;}
		if z < 1 then z := 1;
		Options.Display.Scaling := z;

		w := Console.Bitmap.Width  * z;
		h := Console.Bitmap.Height * z;

		SDL_SetWindowFullscreen(window, 0);
		SDL_SetWindowSize(window, w, h);
		SDL_SetWindowPosition(window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
		SDL_SetWindowGrab(window, SDL_FALSE);
	end;

{    SDL_GetWindowSize(window, @w, @h);
    SDL_WarpMouseInWindow(window, w div 2, h div 2);}

	SDL_RenderGetScale(Renderer, SDL2.PFloat(@X), SDL2.PFloat(@Y));
	w := Max(Trunc(x), 1); h := Max(Trunc(y), 1);
	MouseCursor.Scaling := Types.Point(w, h);
end;

procedure TWindow.SetTitle(const Title: AnsiString);
begin
	if Initialized then
		SDL_SetWindowTitle(Window, PAnsiChar(Title));
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
			EscMenu;

		// exit program
		keyProgramQuit:
			if not InModalDialog then
				QuitFlag := True;

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
				Key := SDLK_F7; // dumb hack
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
				SetFullScreen(not IsFullScreen);

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
	SDL_PumpEvents;
	SDL_GetMouseState(@X, @Y);

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

procedure GetModifierKey(keymod: Integer; var Shift: TShiftState;
	keymodconst: Word; shiftconst: TShiftStateEnum);
begin
	if keymod and keymodconst <> 0 then
		Include(Shift, shiftconst);
end;

function GetShiftState: TShiftState;
var
	M: Word;
begin
	Result := [];
	M := SDL_GetModState;
	GetModifierKey(M, Result, KMOD_SHIFT,	ssShift);	// Shift
	GetModifierKey(M, Result, KMOD_CTRL,	ssCtrl);	// Ctrl
	GetModifierKey(M, Result, KMOD_ALT,		ssAlt);		// Alt
	GetModifierKey(M, Result, KMOD_MODE,	ssAltGr);	// AltGr
	GetModifierKey(M, Result, KMOD_GUI,		ssMeta);	// Windows
	//GetModifierKey(M, Result, KMOD_NUM,	ssNum);		// Num Lock
	GetModifierKey(M, Result, KMOD_CAPS,	ssCaps);	// Caps Lock
end;

(*function ScancodeToChar(Scancode: TSDL_Scancode): Char;
var
	Key: TSDL_Keycode;
begin
    case Scancode of
        SDL_SCANCODE_MINUS:          Result := '-';  //SDLK_MINUS;
        SDL_SCANCODE_EQUALS:         Result := '=';  //SDLK_EQUALS;
        SDL_SCANCODE_LEFTBRACKET:    Result := '{';  //SDLK_LEFTBRACKET;
        SDL_SCANCODE_RIGHTBRACKET:   Result := '}';  //SDLK_RIGHTBRACKET;
        SDL_SCANCODE_BACKSLASH:      Result := '\';  //SDLK_BACKSLASH;
        SDL_SCANCODE_SEMICOLON:      Result := ';';  //SDLK_SEMICOLON;
        SDL_SCANCODE_APOSTROPHE:     Result := ''''; //SDLK_QUOTE;
        SDL_SCANCODE_GRAVE:          Result := '~';  //SDLK_BACKQUOTE;
        SDL_SCANCODE_COMMA:          Result := ',';  //SDLK_COMMA;
        SDL_SCANCODE_PERIOD:         Result := '.';  //SDLK_PERIOD;
        SDL_SCANCODE_SLASH:          Result := '/';  //SDLK_SLASH;
        SDL_SCANCODE_NONUSBACKSLASH: Result := '\';  //SDLK_LESS;
	else
		Key := SDL_GetKeyFromScancode(Scancode);
	    if (Key < -128) or (Key > 127) then
			Result := #0
		else
			Result := Chr(Key);
	end;
end;*)

procedure TWindow.HandleInput;
var
	InputEvent: TSDL_Event;
	c: SInt32;
	X, Y: Integer;
	B: Boolean;
	Btn: TMouseButton;
	Key: Integer;
	Shift: TShiftState;

	function GetXY: TPoint;
	begin
		Result := Types.Point(
			MouseCursor.Pos.X div Console.Font.Width,
			MouseCursor.Pos.Y div Console.Font.Height);
	end;

begin
	X := MouseCursor.Pos.X;
	Y := MouseCursor.Pos.Y;

	while SDL_PollEvent(@InputEvent) <> 0 do
	case {%H-}InputEvent.type_ of

		SDL_USEREVENT:		// messages from playroutine
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
			Key := InputEvent.key.keysym.sym;
			case Key of
				SDLK_LSHIFT, SDLK_RSHIFT,
				SDLK_LCTRL,  SDLK_RCTRL,
				SDLK_LALT,   SDLK_RALT,
				0: ;
			else
				Shift := [];
				{if (Key <> 0) and (InputEvent.key._repeat = 0) then
				begin
					writeln('Key code: ', InputEvent.key.keysym.sym,
						'  "', SDL_GetKeyName(InputEvent.key.keysym.sym), '"');
					writeln('Scancode: ', InputEvent.key.keysym.scancode,
						'  "', SDL_GetScancodeName(InputEvent.key.keysym.scancode ), '"');
					writeln('Modifiers: ', InputEvent.key.keysym._mod);
					writeln;
				end;}
				if InputEvent.key.keysym._mod <> 0 then
				begin
					c := InputEvent.key.keysym._mod;
					GetModifierKey(c, Shift, KMOD_SHIFT,ssShift);	// Shift
					GetModifierKey(c, Shift, KMOD_CTRL,	ssCtrl);	// Ctrl
					GetModifierKey(c, Shift, KMOD_ALT,	ssAlt);		// Alt
					GetModifierKey(c, Shift, KMOD_GUI,	ssMeta);	// Windows
					GetModifierKey(c, Shift, KMOD_CAPS,	ssCaps);	// Caps Lock
					GetModifierKey(c, Shift, KMOD_MODE,	ssAltGr);	// AltGr
				end;
				OnKeyDown(Key, Shift);
			end;
		end;

        SDL_TEXTINPUT:
			if InputEvent.text.text[0] <> #0 then
			begin
				//writeln( 'Text input: "', InputEvent.text.text, '"' );
				CurrentScreen.TextInput(InputEvent.text.text[0]);
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

				if InputEvent.type_ = SDL_MOUSEBUTTONDOWN then
				begin
					if PtInRect(CurrentScreen.Rect, GetXY) then
					begin
						SDL_CaptureMouse(SDL_TRUE);
						B := CurrentScreen.MouseDown(Btn, X, Y, GetXY);
						// right button for context menu if the button wasn't otherwise handled
						if (Btn = mbRight) and (not B) and (ModalDialog.Dialog = nil) then
							EscMenu;
					end
					else
					// close context menu by clicking outside it
					if (ModalDialog.Dialog <> nil) and (ModalDialog.ID = DIALOG_CONTEXTMENU) and
						(CurrentScreen = ModalDialog.Dialog) then
							ModalDialog.Close;
				end
				else
				if InputEvent.type_ = SDL_MOUSEBUTTONUP then
				begin
					SDL_CaptureMouse(SDL_FALSE);
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

		SDL_WINDOWEVENT:
			case InputEvent.window.event of
		        SDL_WINDOWEVENT_ENTER:	MouseCursor.InWindow := True;
		        SDL_WINDOWEVENT_LEAVE:	MouseCursor.InWindow := False;
			end;

		SDL_DROPFILE:
			DoLoadModule(InputEvent.drop._file);

		SDL_QUITEV:
			QuitFlag := True;

	end;
end;

procedure TWindow.SyncTo60Hz; 				// from PT clone
var
	delayMs, perfFreq, timeNow_64bit: UInt64;
begin
	if VSyncRate > 0 then Exit;

	perfFreq := SDL_GetPerformanceFrequency; // should be safe for double
	if perfFreq = 0 then Exit; // panic!

	timeNow_64bit := SDL_GetPerformanceCounter;
	if next60HzTime_64bit > timeNow_64bit then
	begin
		delayMs := Trunc((next60HzTime_64bit - timeNow_64bit) * (1000.0 / perfFreq) + 0.5);
		SDL_Delay(delayMs);
	end;
	Inc(next60HzTime_64bit, Trunc(perfFreq / 60 + 0.5));
end;

procedure TWindow.EscMenu;
var
	Dlg: TCWEScreen;
	List: TCWEMainMenu;
	Section: TKeyBindings;

	procedure AddSection(const Caption: AnsiString);
	begin
		List.Items.Add(TCWEListItem.Create(Caption, LISTITEM_HEADER, nil, 3, 2));
	end;

	procedure AddCmd(Key: Cardinal; const Caption: AnsiString);
	begin
		List.Items.Add(TCWEListItem.Create(
			Caption + COLUMNSEPARATOR + ShortCuts.GetShortcut(Section, Key),
			Ord(Key), Pointer(Section)));
	end;

	procedure AddCmd2(Key: Cardinal; const Caption: AnsiString);
	begin
		List.Items.Add(TCWEListItem.Create(Caption, $80000000 + Key, Pointer(Section)));
	end;

var
	i, W, H: Integer;
begin
	W := 34+6;
	H := 32;

	if ModalDialog.Dialog <> nil then Exit;

	Dlg := ModalDialog.CreateDialog(DIALOG_CONTEXTMENU, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'Menu');

	List := TCWEMainMenu.Create(Dlg, '', 'Menu',
		Types.Rect(1, 2, W-1, H-1), True);

	List.ColorBack := TConsole.COLOR_PANEL;
	List.ColorFore := TConsole.COLOR_TEXT;
	List.ColumnColor[0] := List.ColorFore;
	List.ColumnColor[1] := TConsole.COLOR_3DDARK;
	List.ColumnWidth[1] := 13;
	List.ColumnWidth[0] := List.Width - List.ColumnWidth[1];
	List.OnActivate  := List.MainMenuCommand;
	List.Selection3D := True;

	List.Data[0].Value := TConsole.COLOR_LIGHT; // bright white
	for i := 1 to 3 do
		List.Data[i].Value := 8; // hover bg + border

	with ModalDialog do
	begin
		if CurrentScreen = Editor then
		begin
			Section := EditorKeys;

			AddSection('Block operations');

			AddCmd(Ord(keyBlockCut),				'Cut');
			AddCmd(Ord(keyBlockCopy),				'Copy');
			AddCmd(Ord(keyBlockPaste),				'Paste');
			AddCmd(Ord(keyBlockOverwrite),			'Overwrite');
			AddCmd(Ord(keyBlockMix),				'Mix');
			AddCmd(Ord(keyBlockSwap),				'Swap');
			AddCmd(Ord(keyBlockDouble),				'Double size');
			AddCmd(Ord(keyBlockHalve),				'Halve size');
			AddCmd(Ord(keyBlockSlideWipeEffect),	'Slide effect values');
			AddCmd(Ord(keyBlockSetSample),			'Replace sample');
			AddCmd(Ord(keyTransposeSemitoneUp),		'Transpose semitone up');
			AddCmd(Ord(keyTransposeSemitoneDown),	'Transpose semitone down');
			AddCmd(Ord(keyTransposeOctaveUp),		'Transpose octave up');
			AddCmd(Ord(keyTransposeOctaveDown),		'Transpose octave down');
			{AddSection('Pattern operations'); 		// handled in Screen.Editor
			AddCmd2(CMD_PATTERN_INSERT,		 		'Insert pattern');
			AddCmd2(CMD_PATTERN_DELETE,		 		'Delete pattern');
			AddCmd2(CMD_PATTERN_CLONE,		 		'Duplicate pattern');}
		end
		else
		if CurrentScreen = SampleScreen then
		begin
			Section := SampleListKeys;
			AddSection('Sample');

			AddCmd(Ord(keySampleClear),			'Clear name');
			AddCmd(Ord(keySampleDelete),		'Delete');
			AddCmd(Ord(keySampleCopy),			'Copy from...');
			AddCmd(Ord(keySampleReplace),		'Replace with...');
			AddCmd(Ord(keySampleSwap),			'Swap with...');
			AddCmd(Ord(keySampleCutLeft),		'Cut pre-loop');
			AddCmd(Ord(keySampleCutRight),		'Cut post-loop');
			AddCmd(Ord(keySampleReverse),		'Reverse');
			AddCmd(Ord(keySampleInvert),		'Invert');
			if SOXRLoaded then
			begin
				AddCmd(Ord(keySampleResample),		'Resample...');
			end;
			AddCmd(Ord(keySampleAmplify),		'Amplify...');
			AddCmd(Ord(keySampleAmplifyAll),	'Amplify all...');
			AddCmd(Ord(keySampleSave), 			'Save to file...');
			AddCmd(Ord(keySampleInsertSlot),	'Insert slot');
			AddCmd(Ord(keySampleRemoveSlot),	'Remove slot');
		end
		else
		if (CurrentScreen = FileRequester)   or
		   (CurrentScreen = SampleRequester) then
		begin
			AddSection('File listing');

			AddCmd2(FILESORT_NAME,				'Sort by name');
			AddCmd2(FILESORT_SIZE,				'Sort by size');
			AddCmd2(FILESORT_DATE,				'Sort by date');
			{$IFDEF WINDOWS}
			AddCmd2(FILE_EXPLORE,				'Show file in Explorer');
			{$ENDIF}
		end;

		Section := GlobalKeys;

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

		AddSection('Program');
		AddCmd(Ord(keyProgramFullscreen), 		'Toggle fullscreen');
		AddCmd(Ord(keyScreenAbout), 			'About...');
		AddCmd(Ord(keyProgramQuit), 			'Quit');

		H := List.Items.Count + 3;

		ModalDialog.SetBounds(Bounds(
			(Console.Width  div 2) - (W div 2),
			(Console.Height div 2) - (H div 2), W, H));
		List.SetBounds(
			Types.Rect(1, 2, W-1, H-1));
		List.ItemIndex := 1;
		List.Scrollbar.Visible := False;

		Show;
	end;
end;

function TimerTickCallback(interval: Uint32; param: Pointer): UInt32; cdecl;
var
	event: TSDL_Event;
begin
	if Initialized then
	begin
		event.type_ := SDL_USEREVENT;
		event.user.code := MSG_TIMERTICK;
	    SDL_PushEvent(@event);
	end;
	Result := interval;
end;

procedure TWindow.TimerTick;
begin
//	if DisableInput then Exit;

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

constructor TWindow.Create;
var
	Cfg: TConfigurationManager;
	Dir: String;
	Sect: AnsiString;
	i: Integer;
	AudioDeviceList: array[1..10] of AnsiString;
	device: BASS_DEVICEINFO;
begin
	Initialized := False;
	QuitFlag := False;

	// Init application directories
	//
	AppPath := ExtractFilePath(ParamStr(0));
	DataPath := AppPath + 'data/';
	ConfigPath := GetAppConfigDir(False);
	if ConfigPath = '' then
		ConfigPath := DataPath;
	ForceDirectories(ConfigPath);
	DefaultFormatSettings.DecimalSeparator := '.';

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
		.SetInfo('Pixel scaling', 1, 8, [], PixelScalingChanged);
		Cfg.AddString(Sect, 'Font', @Display.Font, FILENAME_DEFAULTFONT).
		SetInfoFromDir('Font', DataPath + 'font/', '*.pcx', nil{ApplyFont});
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

	Cfg.Load;

	if Options.Dirs.Modules = '' then
		Options.Dirs.Modules := AppPath;
	if Options.Dirs.Samples = '' then
		Options.Dirs.Samples := Options.Dirs.Modules;


	// Create fake text mode console and init SDL
	//
	if not SetupVideo(80, 45, Options.Display.Scaling) then
	begin
		{$IFDEF UNIX}
		writeln('Could not initialize video; quitting!');
		{$ENDIF}
		HALT;
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
	Log('2016-2017 hukka (Joel Toivonen)');
	Log('http://hukka.yiff.fi/porotracker/');
	Log('Contains code based on work by 8bitbubsy (Olav Sorensen)');
	Log('');

	if not NewSDL then
	begin
		Log(TEXT_WARNING + 'Using an older version of SDL. (< 2.0.5)');
		Log(TEXT_WARNING + 'Visuals may appear worse than intended!');
		Log('');
	end;

	Dir := Format('Using SDL %d.%d.%d',[sdlVersion.major, sdlVersion.minor, sdlVersion.patch]);
	if VSyncRate > 0 then
		Dir := Dir + Format(' with %dHz vsync', [VSyncRate]);
	Log(Dir);

	Options.Features.SOXR := (soxr_version <> '');
	if not Options.Features.SOXR then
	begin
		{$IFDEF UNIX}
	    Log(TEXT_WARNING + 'SOXR support not implemented on non-Windows platforms.');
		{$ELSE}
	    Log(TEXT_WARNING + 'SOXR support not compiled in!');
		{$ENDIF}
	    Log(TEXT_WARNING + 'Resampling features disabled.');
	end
	else
		Log('Using SOXR ' + soxr_version);

	case Options.Audio.Frequency of
		0: i := 11025;
		1: i := 22050;
		2: i := 44100;
		3: i := 48000;
	else
		i := 44100;
	end;

	if not AudioInit(i) then
	begin
		{$IFDEF UNIX}
	    writeln('Could not initialize audio; quitting!');
		{$ENDIF}
		HALT;
	end;


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

	// Load any user-defined shortcuts
	//
	Shortcuts.Load(ConfigPath + FILENAME_KEYBOARD);

	Log('');

	ApplyPointer;

	ConfigScreen.Init(ConfigManager);

	DoLoadModule('');

	ChangeScreen(TCWEScreen(Editor));
	Console.Paint;

	Initialized := True;
	Module.SetModified(False);

	MessageTextTimer := -1;

	{$IFDEF WINDOWS}
	if Options.HighPriority then
		SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
	{$ENDIF}

	SetFullScreen(IsFullScreen);

	{SDL_TimerID := }
	SDL_AddTimer(TimerInterval, TimerTickCallback, nil);
end;

destructor TWindow.Destroy;
begin
	Initialized := False;

	// Save configuration
	Shortcuts.Save(ConfigPath + FILENAME_KEYBOARD);
	ConfigScreen.SavePalette;

	ConfigManager.Save;
	ConfigManager.Free;

	Console.Free;
	Screens.Free;
	MouseCursor.Free;

	Module.Free;
	AudioClose;

	SDL_DestroyRenderer(Renderer);
	SDL_DestroyTexture(Texture);
	SDL_DestroyWindow(Window);
	SDL_Quit;
end;

procedure TWindow.Close;
begin
	QuitFlag := True;
end;

{ TCWEMainMenu }

procedure TCWEMainMenu.MainMenuCommand(Sender: TCWEControl);
var
	Item: TCWEListItem;
	Sect: TKeyBindings;
	Binding: TKeyBinding;
	i: Integer;
	Cmd: Cardinal;
	Key: Integer;
	Shift: TShiftState;
begin
	// get the Key and Shift codes for a keybinding const (e.g. keyScreenHelp -> F1, [])

	Item := Items[ItemIndex];
	if Item.ObjData = nil then Exit;
	Cmd := Item.Data;

	if (Cmd and $80000000) = 0 then
	begin
		i := Shortcuts.Sections.IndexOf(TKeyBindings(Item.ObjData));
		if i < 0 then
		begin
			Log(TEXT_WARNING+'Section not found!');
			Exit;
		end;

		Sect := Shortcuts.Sections[i];
		Binding := Sect.FindKey(Cmd);

		if Binding = nil then
		begin
			Log(TEXT_WARNING+'Binding not found!');
			Exit;
		end
		else
		begin
			Key := Binding.Shortcut.Key;
			Shift := Binding.Shortcut.Shift;
			if Key <> 0 then
			begin
				ModalDialog.Close;
				Window.OnKeyDown(Key, Shift);
			end
			else
				Log(TEXT_WARNING+'Unhandled command!');
		end;
	end
	else
	begin
		ModalDialog.Close;
		CurrentScreen.HandleCommand(Cmd and $7FFFFFFF);
	end;
end;

function TCWEMainMenu.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
begin
	if Key = SDLK_ESCAPE then
	begin
		Result := True;
		ModalDialog.Close;
	end
	else
		Result := inherited;
end;


end.

