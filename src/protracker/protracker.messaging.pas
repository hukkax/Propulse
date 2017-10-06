unit ProTracker.Messaging;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	SDL.Api.libSDL2, SDL.Api.Types, SDL.API.Events;

	function RegisterMessages(Message, Count: Integer): Integer;
	function GetMessageValue(const InputEvent: SDL_Event): Integer; inline;
	procedure PostMessagePtr(EventType: Byte; Data: Pointer);
	procedure PostMessageValue(EventType: Byte; Data: Integer);


implementation

uses
	MainWindow;


function RegisterMessages(Message, Count: Integer): Integer;
var
	i: Integer;
begin
	Result := Integer(MainWindow.SDL.Events.SDL_RegisterEvents(Count));
end;

function GetMessageValue(const InputEvent: SDL_Event): Integer; inline;
begin
	Result := PtrInt(InputEvent.user.data1);
end;

procedure PostMessagePtr(EventType: Byte; Data: Pointer);
var
	event: SDL_Event;
begin
	event._type := SDL_USEREVENT_EV;
	event.user.code := EventType;
	event.user.data1 := Data;
	MainWindow.SDL.Events.SDL_PushEvent(@event);
end;

procedure PostMessageValue(EventType: Byte; Data: Integer);
var
	event: SDL_Event;
begin
	event._type := SDL_USEREVENT_EV;
	event.user.code := EventType;
	event.user.data1 := Pointer(Data);
	MainWindow.SDL.Events.SDL_PushEvent(@event);
end;

end.

