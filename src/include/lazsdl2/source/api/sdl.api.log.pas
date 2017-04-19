unit SDL.Api.Log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLLogApi }

  TSDLLogApi = class(TSubLibrary)
  public type
    TSDL_LogOutputFunction = procedure(userdata: SDL_Data; category: SDL_LogCategory; priority: SDL_LogPriority; const msg: SDL_String);
  private type
    TSDL_LogSetAllPriority = procedure(priority: SDL_LogPriority) cdecl;
    TSDL_LogSetPriority = procedure(category: SDL_LogCategory; priority: SDL_LogPriority) cdecl;
    TSDL_LogGetPriority = function(category: SDL_LogCategory): SDL_LogPriority cdecl;
    TSDL_LogResetPriorities = procedure() cdecl;
    TSDL_Log = procedure(const fmt: PAnsiChar) cdecl; varargs;
    TSDL_LogVerbose = procedure(category: SDL_LogCategory; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogDebug = procedure(category: SDL_LogCategory; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogInfo = procedure(category: SDL_LogCategory; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogWarn = procedure(category: SDL_LogCategory; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogError = procedure(category: SDL_LogCategory; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogCritical = procedure(category: SDL_LogCategory; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogMessage = procedure(category: SDL_LogCategory; priority: SDL_LogPriority; const fmt: SDL_String) cdecl; varargs;
    TSDL_LogGetOutputFunction = procedure(out callback: TSDL_LogOutputFunction; out userdata: SDL_Data) cdecl;
    TSDL_LogSetOutputFunction = procedure(callback: TSDL_LogOutputFunction; userdata: SDL_Data) cdecl;
  public
    (**
     *  \brief Set the priority of all log categories
     *)
    SDL_LogSetAllPriority: TSDL_LogSetAllPriority;
    (**
     *  \brief Set the priority of a particular log category
     *)
    SDL_LogSetPriority: TSDL_LogSetPriority;
    (**
     *  \brief Get the priority of a particular log category
     *)
    SDL_LogGetPriority: TSDL_LogGetPriority;
    (**
     *  \brief Reset all priorities to default.
     *
     *  \note This is called in SDL_Quit().
     *)
    SDL_LogResetPriorities: TSDL_LogResetPriorities;
    (**
     *  \brief Log a message with SDL_LOG_CATEGORY_APPLICATION and SDL_LOG_PRIORITY_INFO
     *)
    SDL_Log: TSDL_Log;
    (**
     *  \brief Log a message with SDL_LOG_PRIORITY_VERBOSE
     *)
    SDL_LogVerbose: TSDL_LogVerbose;
    (**
     *  \brief Log a message with SDL_LOG_PRIORITY_DEBUG
     *)
    SDL_LogDebug: TSDL_LogDebug;
    (**
     *  \brief Log a message with SDL_LOG_PRIORITY_INFO
     *)
    SDL_LogInfo: TSDL_LogInfo;
    (**
     *  \brief Log a message with SDL_LOG_PRIORITY_WARN
     *)
    SDL_LogWarn: TSDL_LogWarn;
    (**
     *  \brief Log a message with SDL_LOG_PRIORITY_ERROR
     *)
    SDL_LogError: TSDL_LogError;
    (**
     *  \brief Log a message with SDL_LOG_PRIORITY_CRITICAL
     *)
    SDL_LogCritical: TSDL_LogCritical;
    (**
     *  \brief Log a message with the specified category and priority.
     *)
    SDL_LogMessage: TSDL_LogMessage;
    (**
     *  \brief Get the current log output function.
     *)
    SDL_LogGetOutputFunction: TSDL_LogGetOutputFunction;
    (**
     *  \brief This function allows you to replace the default log output
     *         function with one of your own.
     *)
    SDL_LogSetOutputFunction: TSDL_LogSetOutputFunction;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLLogApi }

procedure TSDLLogApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_LogSetAllPriority', @SDL_LogSetAllPriority);
  List.Add('SDL_LogSetPriority', @SDL_LogSetPriority);
  List.Add('SDL_LogGetPriority', @SDL_LogGetPriority);
  List.Add('SDL_LogResetPriorities', @SDL_LogResetPriorities);
  List.Add('SDL_Log', @SDL_Log);
  List.Add('SDL_LogVerbose', @SDL_LogVerbose);
  List.Add('SDL_LogDebug', @SDL_LogDebug);
  List.Add('SDL_LogInfo', @SDL_LogInfo);
  List.Add('SDL_LogWarn', @SDL_LogWarn);
  List.Add('SDL_LogError', @SDL_LogError);
  List.Add('SDL_LogCritical', @SDL_LogCritical);
  List.Add('SDL_LogMessage', @SDL_LogMessage);
  List.Add('SDL_LogGetOutputFunction', @SDL_LogGetOutputFunction);
  List.Add('SDL_LogSetOutputFunction', @SDL_LogSetOutputFunction);
end;

end.

