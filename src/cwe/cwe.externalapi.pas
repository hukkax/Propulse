//
// interface CWE with SDL2
//
unit CWE.ExternalAPI;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	SDL2,
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
	MainWindow;

procedure ClipboardCopy(Sender: TCWEEdit);
begin
	SDL_SetClipboardText(PChar(Sender.Caption));
end;

procedure ClipboardPaste(Sender: TCWEEdit);
var
	S: PAnsiChar;
	Text, Dest: AnsiString;
	X: Integer;
begin
	if SDL_HasClipboardText = SDL_TRUE then
	begin
		if Sender.AllowedChars <> '' then Exit;
		S := SDL_GetClipboardText;
		Text := StrPas(S);
		if Text <> '' then
		begin
			Dest := Sender.Caption;
			X := Sender.Cursor.X;
			Dest.Insert(X, Text);
			Sender.SetCaption(Dest);
			Sender.Cursor.X := Min(X + Length(Text), Length(Dest));
			Sender.Paint;
			Sender.Change(Sender.ReportAnyChange);
		end;
		//SDL_Free(S); // crashes
	end;
end;

end.

