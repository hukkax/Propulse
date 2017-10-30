//
// interface CWE with SDL2
//
unit CWE.ExternalAPI;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	SDL.Api.Types,
	CWE.Widgets.Text;

const
	KEY_DOWN		= SDLK_DOWN;
	KEY_UP			= SDLK_UP;
	KEY_RETURN		= SDLK_RETURN;
	KEY_BACKSPACE	= SDLK_BACKSPACE;


	procedure ClipboardCopy(Sender: TCWEEdit);
	procedure ClipboardPaste(Sender: TCWEEdit);


implementation

uses
	Math,
	SDL.Api.libSDL2,
	MainWindow;

{ TClipboardHandler }

procedure ClipboardCopy(Sender: TCWEEdit);
begin
	MainWindow.SDL.Clipboard.SDL_SetClipboardText(PChar(Sender.Caption));
end;

procedure ClipboardPaste(Sender: TCWEEdit);
var
	S: SDL_String;
	Text, Dest: AnsiString;
	X: Integer;
begin
	if MainWindow.SDL.Clipboard.SDL_HasClipboardText = SDL_TRUE then
	begin
		if Sender.AllowedChars <> '' then Exit;
		S := MainWindow.SDL.Clipboard.SDL_GetClipboardText;
		Text := StrPas(S);
		Dest := Sender.Caption;
		X := Sender.Cursor.X;
		Dest.Insert(X, Text);
		Sender.SetCaption(Dest);
		Sender.Cursor.X := Min(X + Length(Text), Length(Dest));
		Sender.Paint;
		//MainWindow.SDL.StdInc.SDL_Free(S); // crashes
	end;
end;

end.

