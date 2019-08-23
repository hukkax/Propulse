{$IFDEF UNIX}
	{.$DEFINE DISABLE_FULLSCREEN}		// disable broken SDL fullscreen mode
	{.$DEFINE LIMIT_KEYBOARD_EVENTS}	// fix duplicate keyboard events on Linux with FCITX
{$ENDIF}

{$IFDEF WINDOWS}
	{.$DEFINE MIDI}						// enable experimental MIDI input support
	{.$DEFINE MIDI_DISPLAY}				// test code to show fx on an APC Key25 led matrix
{$ENDIF}

{$DEFINE BASS}
{$DEFINE BASS_DYNAMIC}

