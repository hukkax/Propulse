program Propulse;

{$APPTYPE GUI}
{$R Propulse.res}

uses
	{$IFDEF FPC}
		{$ifdef unix}
		cthreads, //cmem,
		Classes,
		{$endif}
		{$IFDEF BASS_DYNAMIC}
		lazdynamic_bass,
		{$ENDIF}
	{$ENDIF}
	MainWindow, CWE.Core;


	{$IFDEF FPC}
	{$ifdef unix}
	// on Unix we need to initialize the threading system before
	// using custom callbacks with BASS or we crash!
	type
		TDummyThread = class(TThread)
			procedure Execute; override;
		end;

		procedure TDummyThread.Execute;
		begin
		end;
	{$ENDIF}
	{$ENDIF}


begin
	{$IFDEF FPC}
	{$ifdef unix}
		with TDummyThread.Create(False) do
		begin
			WaitFor;
			Free;
		end;
	{$ENDIF}
	{$ENDIF}

	{$IFDEF BASS_DYNAMIC}
	// load the BASS library dynamically at runtime
		{$IFDEF WINDOWS}
		if not Load_BASSDLL('bass.dll') then
		{$ELSE}
		if not Load_BASSDLL('./libbass.so') then
		{$ENDIF}
		begin
			writeln('Could not init BASS library!');
			HALT;
		end;
	{$ENDIF}

	Window := TWindow.Create;

    with Window do
	while not QuitFlag do
	begin
		SyncTo60Hz;
        HandleInput;

		Console.Paint;

		FlipFrame;
	end;

	Window.Free;

	{$IFDEF BASS_DYNAMIC}
	Unload_BASSDLL;
	{$ENDIF}
end.

