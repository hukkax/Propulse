unit SDL.Api.Assert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLAssertApi }

  TSDLAssertApi = class(TSubLibrary)
  public type
    TSDL_AssertionHandler = function(const data: PSDL_AssertData; userdata: SDL_Data): SDL_AssertState;
  private type
    TSDL_SetAssertionHandler = procedure(handler: TSDL_AssertionHandler; userdata: SDL_Data) cdecl;
    TSDL_GetDefaultAssertionHandler = function(): TSDL_AssertionHandler cdecl;
    TSDL_GetAssertionHandler = function(out usserdata: SDL_Data): TSDL_AssertionHandler cdecl;
    TSDL_GetAssertionReport = function(): PSDL_AssertData cdecl;
    TSDL_ResetAssertionReport = procedure() cdecl;
  public
    (**
     *  \brief Set an application-defined assertion handler.
     *
     *  This allows an app to show its own assertion UI and/or force the
     *  response to an assertion failure. If the app doesn't provide this, SDL
     *  will try to do the right thing, popping up a system-specific GUI dialog,
     *  and probably minimizing any fullscreen windows.
     *
     *  This callback may fire from any thread, but it runs wrapped in a mutex, so
     *  it will only fire from one thread at a time.
     *
     *  Setting the callback to NULL restores SDL's original internal handler.
     *
     *  This callback is NOT reset to SDL's internal handler upon SDL_Quit()!
     *
     *  \return SDL_assert_state value of how to handle the assertion failure.
     *
     *  \param handler Callback function, called when an assertion fails.
     *  \param userdata A pointer passed to the callback as-is.
     *)
    SDL_SetAssertionHandler: TSDL_SetAssertionHandler;
    (**
     *  \brief Get the default assertion handler.
     *
     *  This returns the function pointer that is called by default when an
     *   assertion is triggered. This is an internal function provided by SDL,
     *   that is used for assertions when SDL_SetAssertionHandler() hasn't been
     *   used to provide a different function.
     *
     *  \return The default SDL_AssertionHandler that is called when an assert triggers.
     *)
    SDL_GetDefaultAssertionHandler: TSDL_GetDefaultAssertionHandler;
    (**
     *  \brief Get the current assertion handler.
     *
     *  This returns the function pointer that is called when an assertion is
     *   triggered. This is either the value last passed to
     *   SDL_SetAssertionHandler(), or if no application-specified function is
     *   set, is equivalent to calling SDL_GetDefaultAssertionHandler().
     *
     *   \param puserdata Pointer to a void*, which will store the "userdata"
     *                    pointer that was passed to SDL_SetAssertionHandler().
     *                    This value will always be NULL for the default handler.
     *                    If you don't care about this data, it is safe to pass
     *                    a NULL pointer to this function to ignore it.
     *  \return The SDL_AssertionHandler that is called when an assert triggers.
     *)
    SDL_GetAssertionHandler: TSDL_GetAssertionHandler;
    (**
     *  \brief Get a list of all assertion failures.
     *
     *  Get all assertions triggered since last call to SDL_ResetAssertionReport(),
     *  or the start of the program.
     *
     *  The proper way to examine this data looks something like this:
     *
     *  <code>
     *  const SDL_assert_data *item = SDL_GetAssertionReport();
     *  while (item) {
     *      printf("'%s', %s (%s:%d), triggered %u times, always ignore: %s.\n",
     *             item->condition, item->function, item->filename,
     *             item->linenum, item->trigger_count,
     *             item->always_ignore ? "yes" : "no");
     *      item = item->next;
     *  }
     *  </code>
     *
     *  \return List of all assertions.
     *  \sa SDL_ResetAssertionReport
     *)
    SDL_GetAssertionReport: TSDL_GetAssertionReport;
    (**
     *  \brief Reset the list of all assertion failures.
     *
     *  Reset list of all assertions triggered.
     *
     *  \sa SDL_GetAssertionReport
     *)
    SDL_ResetAssertionReport: TSDL_ResetAssertionReport;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLAssertApi }

procedure TSDLAssertApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_SetAssertionHandler', @SDL_SetAssertionHandler);
  List.Add('SDL_GetDefaultAssertionHandler', @SDL_GetDefaultAssertionHandler);
  List.Add('SDL_GetAssertionHandler', @SDL_GetAssertionHandler);
  List.Add('SDL_GetAssertionReport', @SDL_GetAssertionReport);
  List.Add('SDL_ResetAssertionReport', @SDL_ResetAssertionReport);
end;

end.

