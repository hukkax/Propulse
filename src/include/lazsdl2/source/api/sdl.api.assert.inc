
type

  PSDL_AssertData = ^SDL_AssertData;

  SDL_AssertState = (
      SDL_ASSERTION_RETRY,  (** Retry the assert immediately. *)
      SDL_ASSERTION_BREAK,  (** Make the debugger trigger a breakpoint. *)
      SDL_ASSERTION_ABORT,  (** Terminate the program. *)
      SDL_ASSERTION_IGNORE,  (** Ignore the assert. *)
      SDL_ASSERTION_ALWAYS_IGNORE  (** Ignore the assert from now on. *)
  );

  SDL_AssertData = record
      always_ignore: SDL_SInt32;
      trigger_count: SDL_UInt32;
      condition: SDL_String;
      filename: SDL_String;
      linenum: SDL_SInt32;
      _function: SDL_String;
      next: PSDL_AssertData;
  end;

