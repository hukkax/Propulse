(**
 *  \file SDL_events.h
 *
 *  Include file for SDL event handling.
 *)
unit SDL.Api.Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLEventsApi }

  TSDLEventsApi = class(TSubLibrary)
  public type
    PSDL_EventFilter = ^TSDL_EventFilter;
    TSDL_EventFilter = function( userdata: Pointer; event: PSDL_Event ): SDL_SInt32; cdecl;
  private type
    TSDL_PumpEvents = procedure cdecl;
    TSDL_PeepEvents = function(events: PSDL_Event; numevents: SDL_SInt32; action: SDL_EventAction; minType: SDL_EventType; maxType: SDL_EventType): SDL_SInt32 cdecl;
    TSDL_HasEvent = function(type_: SDL_EventType): SDL_Bool; cdecl;
    TSDL_HasEvents = function(minType: SDL_EventType; maxType: SDL_EventType): SDL_Bool cdecl;
    TSDL_FlushEvent = procedure(type_: SDL_EventType) cdecl;
    TSDL_FlushEvents = procedure(minType: SDL_EventType; maxType: SDL_EventType) cdecl;
    TSDL_PollEvent = function(event: PSDL_Event): SDL_Bool cdecl;
    TSDL_WaitEvent = function(event: PSDL_Event): SDL_SInt32 cdecl;
    TSDL_WaitEventTimeout = function(event: PSDL_Event; timeout: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_PushEvent = function(event: PSDL_Event): SDL_ResultCode cdecl;
    TSDL_SetEventFilter = procedure(filter: TSDL_EventFilter; userdata: SDL_Data) cdecl;
    TSDL_GetEventFilter = function(out filter: PSDL_EventFilter; out userdata: SDL_Data): SDL_Bool cdecl;
    TSDL_AddEventWatch = procedure(filter: TSDL_EventFilter; userdata: SDL_Data) cdecl;
    TSDL_DelEventWatch = procedure(filter: TSDL_EventFilter; userdata: SDL_Data) cdecl;
    TSDL_FilterEvents = procedure(filter: TSDL_EventFilter; userdata: SDL_Data) cdecl;
    TSDL_EventState = function(type_: SDL_EventType; state: SDL_EventState): SDL_UInt8 cdecl;
    TSDL_RegisterEvents = function(numevents: SDL_SInt32): SDL_EventType cdecl;
  public
    (**
     *  Pumps the event loop, gathering events from the input devices.
     *
     *  This function updates the event queue and internal input device state.
     *
     *  This should only be run in the thread that sets the video mode.
     *)
    SDL_PumpEvents: TSDL_PumpEvents;
    (**
     *  Checks the event queue for messages and optionally returns them.
     *
     *  If \c action is ::SDL_ADDEVENT, up to \c numevents events will be added to
     *  the back of the event queue.
     *
     *  If \c action is ::SDL_PEEKEVENT, up to \c numevents events at the front
     *  of the event queue, within the specified minimum and maximum type,
     *  will be returned and will not be removed from the queue.
     *
     *  If \c action is ::SDL_GETEVENT, up to \c numevents events at the front
     *  of the event queue, within the specified minimum and maximum type,
     *  will be returned and will be removed from the queue.
     *
     *  \return The number of events actually stored, or -1 if there was an error.
     *
     *  This function is thread-safe.
     *)
    SDL_PeepEvents: TSDL_PeepEvents;
    (**
     *  Checks to see if certain event types are in the event queue.
     *)
    (* @{ *)
    SDL_HasEvent: TSDL_HasEvent;
    SDL_HasEvents: TSDL_HasEvents;
    (* @} *)
    (**
     *  This function clears events from the event queue
     *  This function only affects currently queued events. If you want to make
     *  sure that all pending OS events are flushed, you can call SDL_PumpEvents()
     *  on the main thread immediately before the flush call.
     *)
    (* @{ *)
    SDL_FlushEvent: TSDL_FlushEvent;
    SDL_FlushEvents: TSDL_FlushEvents;
    (* @} *)
    (**
     *  \brief Polls for currently pending events.
     *
     *  \return 1 if there are any pending events, or 0 if there are none available.
     *
     *  \param event If not NULL, the next event is removed from the queue and
     *               stored in that area.
     *)
    SDL_PollEvent: TSDL_PollEvent;
    (**
     *  \brief Waits indefinitely for the next available event.
     *
     *  \return 1, or 0 if there was an error while waiting for events.
     *
     *  \param event If not NULL, the next event is removed from the queue and
     *               stored in that area.
     *)
    SDL_WaitEvent: TSDL_WaitEvent;
    (**
     *  \brief Waits until the specified timeout (in milliseconds) for the next
     *         available event.
     *
     *  \return 1, or 0 if there was an error while waiting for events.
     *
     *  \param event If not NULL, the next event is removed from the queue and
     *               stored in that area.
     *  \param timeout The timeout (in milliseconds) to wait for next event.
     *)
    SDL_WaitEventTimeout: TSDL_WaitEventTimeout;
    (**
     *  \brief Add an event to the event queue.
     *
     *  \return 1 on success, 0 if the event was filtered, or -1 if the event queue
     *          was full or there was some other error.
     *)
    SDL_PushEvent: TSDL_PushEvent;
    (**
     *  Sets up a filter to process all events before they change internal state and
     *  are posted to the internal event queue.
     *
     *  The filter is prototyped as:
     *  \code
     *      int SDL_EventFilter(void *userdata, SDL_Event * event);
     *  \endcode
     *
     *  If the filter returns 1, then the event will be added to the internal queue.
     *  If it returns 0, then the event will be dropped from the queue, but the
     *  internal state will still be updated.  This allows selective filtering of
     *  dynamically arriving events.
     *
     *  \warning  Be very careful of what you do in the event filter function, as
     *            it may run in a different thread!
     *
     *  There is one caveat when dealing with the ::SDL_QuitEvent event type.  The
     *  event filter is only called when the window manager desires to close the
     *  application window.  If the event filter returns 1, then the window will
     *  be closed, otherwise the window will remain open if possible.
     *
     *  If the quit event is generated by an interrupt signal, it will bypass the
     *  internal queue and be delivered to the application at the next event poll.
     *)
    SDL_SetEventFilter: TSDL_SetEventFilter;
    (**
     *  Return the current event filter - can be used to "chain" filters.
     *  If there is no event filter set, this function returns SDL_FALSE.
     *)
    SDL_GetEventFilter: TSDL_GetEventFilter;
    (**
     *  Add a function which is called when an event is added to the queue.
     *)
    SDL_AddEventWatch: TSDL_AddEventWatch;
    (**
     *  Remove an event watch function added with SDL_AddEventWatch()
     *)
    SDL_DelEventWatch: TSDL_DelEventWatch;
    (**
     *  Run the filter function on the current event queue, removing any
     *  events for which the filter returns 0.
     *)
    SDL_FilterEvents: TSDL_FilterEvents;
    (**
     *  This function allows you to set the state of processing certain events.
     *   - If \c state is set to ::SDL_IGNORE, that event will be automatically
     *     dropped from the event queue and will not event be filtered.
     *   - If \c state is set to ::SDL_ENABLE, that event will be processed
     *     normally.
     *   - If \c state is set to ::SDL_QUERY, SDL_EventState() will return the
     *     current processing state of the specified event.
     *)
    SDL_EventState: TSDL_EventState;
    (**
     *  This function allocates a set of user-defined events, and returns
     *  the beginning event number for that set of events.
     *
     *  If there aren't enough user-defined events left, this function
     *  returns (Uint32)-1
     *)
    SDL_RegisterEvents: TSDL_RegisterEvents;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    function SDL_GetEventState(EventType: SDL_EventType): SDL_UInt8;
  end;

implementation

{ TSDLEventsApi }

procedure TSDLEventsApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_PumpEvents', @SDL_PumpEvents);
  List.Add('SDL_PeepEvents', @SDL_PeepEvents);
  List.Add('SDL_HasEvent', @SDL_HasEvent);
  List.Add('SDL_HasEvents', @SDL_HasEvents);
  List.Add('SDL_FlushEvent', @SDL_FlushEvent);
  List.Add('SDL_FlushEvents', @SDL_FlushEvents);
  List.Add('SDL_PollEvent', @SDL_PollEvent);
  List.Add('SDL_WaitEvent', @SDL_WaitEvent);
  List.Add('SDL_WaitEventTimeout', @SDL_WaitEventTimeout);
  List.Add('SDL_PushEvent', @SDL_PushEvent);
  List.Add('SDL_SetEventFilter', @SDL_SetEventFilter);
  List.Add('SDL_GetEventFilter', @SDL_GetEventFilter);
  List.Add('SDL_AddEventWatch', @SDL_AddEventWatch);
  List.Add('SDL_DelEventWatch', @SDL_DelEventWatch);
  List.Add('SDL_FilterEvents', @SDL_FilterEvents);
  List.Add('SDL_EventState', @SDL_EventState);
  List.Add('SDL_RegisterEvents', @SDL_RegisterEvents);
end;

function TSDLEventsApi.SDL_GetEventState(EventType: SDL_EventType): SDL_UInt8;
begin
  Result := SDL_EventState(EventType, SDL_QUERY)
end;

end.

