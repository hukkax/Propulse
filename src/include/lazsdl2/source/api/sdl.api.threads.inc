
type

  PSDL_Thread = ^TSDL_Thread;

  SDL_ThreadID = SDL_UInt32;
  SDL_TLSID = SDL_UInt32;

  SDL_ThreadPriority = (
    SDL_THREAD_PRIORITY_LOW,
    SDL_THREAD_PRIORITY_NORMAL,
    SDL_THREAD_PRIORITY_HIGH
  );

  TSDL_Thread = record
    threadid: SDL_ThreadID;
    handle: THandle;
    status: SDL_SInt32;
    errbuf: SDL_Error;
    name: PChar;
    data: SDL_Data;
  end;
