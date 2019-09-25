unit ProTracker.Messaging;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	SDL2;

	function RegisterMessages(Message, Count: Integer): Integer;
	function GetMessageValue(const InputEvent: TSDL_Event): Integer; inline;
	procedure PostMessagePtr(EventType: Byte; Data: Pointer);
	procedure PostMessageValue(EventType: Byte; Data: Integer);


implementation

uses
	MainWindow;


function RegisterMessages(Message, Count: Integer): Integer;
var
	i: Integer;
begin
	Result := Integer(SDL_RegisterEvents(Count));
end;

function GetMessageValue(const InputEvent: TSDL_Event): Integer; inline;
begin
	Result := PtrInt(InputEvent.user.data1);
end;

procedure PostMessagePtr(EventType: Byte; Data: Pointer);
var
	event: TSDL_Event;
begin
	event.type_ := SDL_USEREVENT;
	event.user.code := EventType;
	event.user.data1 := Data;
	SDL_PushEvent(@event);
end;

procedure PostMessageValue(EventType: Byte; Data: Integer);
var
	event: TSDL_Event;
begin
	event.type_ := SDL_USEREVENT;
	event.user.code := EventType;
	event.user.data1 := Pointer(Data);
	SDL_PushEvent(@event);
end;

end.

