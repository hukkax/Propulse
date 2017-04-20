program Propulse;

(*	Propulse - a ProTracker clone with an Impulse Tracker-style interface

	Copyright 2016, 2017 Joel Toivonen (hukka)
	Portions of code adapted from pt2play.c Copyright Olav Sørensen (8bitbubsy)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

{$R propulse.res}

{$IFDEF RELEASE}
	{$IFDEF WINDOWS}
		{$APPTYPE GUI}
	{$ENDIF}
{$ENDIF}

uses
	{$IFDEF UNIX}
	cthreads, //cmem,
	Classes,
	{$ENDIF}
	{$IFDEF BASS_DYNAMIC}
	lazdynamic_bass,
	{$ENDIF}
	MainWindow;


	{$IFDEF UNIX}
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


begin
	{$IFNDEF WINDOWS}
	with TDummyThread.Create(False) do
	begin
		WaitFor;
		Free;
	end;
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

		while not QuitFlag do
			Window.ProcessFrame;

	Window.Free;

	{$IFDEF BASS_DYNAMIC}
	Unload_BASSDLL;
	{$ENDIF}
end.

