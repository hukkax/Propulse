{$IFDEF UNIX}
	{$DEFINE DISABLE_FULLSCREEN}		// disable broken SDL fullscreen mode
	{.$DEFINE LIMIT_KEYBOARD_EVENTS}	// fix duplicate keyboard events on Linux with FCITX
{$ENDIF}

{$IFDEF WINDOWS}
	{.$DEFINE MIDI} // enable MIDI input support
{$ENDIF}

