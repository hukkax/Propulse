
type

  PSDL_mutex = ^SDL_mutex;
  PSDL_semaphore = ^SDL_semaphore;
  PSDL_cond = ^SDL_cond;

  SDL_cond = record
    lock: PSDL_mutex;
    waiting: SDL_SInt32;
    signals: SDL_SInt32;
    wait_sem: PSDL_semaphore;
    wait_done: PSDL_semaphore;
  end;

  SDL_semaphore = record
    count: SDL_UInt32;
    waiters_count: SDL_UInt32;
    count_lock: PSDL_mutex;
    count_nonzero: PSDL_cond;
  end;

  SDL_mutex = record
    recursive: SDL_SInt32;
    owner: SDL_threadID;
    sem: PSDL_semaphore;
  end;
