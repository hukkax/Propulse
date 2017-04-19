
type

  SDL_errorcode = (
      SDL_ENOMEM,
      SDL_EFREAD,
      SDL_EFWRITE,
      SDL_EFSEEK,
      SDL_UNSUPPORTED,
      SDL_LASTERROR
  );

  SDL_Error = record
    error: SDL_SInt32;
    key: SDL_ErrorString;
    argc: SDL_SInt32;
    args: array [0..SDLOptions.ERR_MAX_ARGS - 1] of record
      case SDL_SInt32 of
        1: (value_ptr: SDL_Data;);
        0: (value_c: SDL_UInt8;);
        2: (value_i: SDL_SInt32;);
        3: (value_f: SDL_Double;);
        4: (buf: SDL_ErrorString;);
    end;
  end;
