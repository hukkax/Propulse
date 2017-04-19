
type

  PSDL_SpinLock = ^SDL_SpinLock;
  PPSDL_atomic_t = ^PSDL_atomic_t;
  PSDL_atomic_t = ^SDL_atomic_t;

  (**
   * \name SDL AtomicLock
   *
   * The atomic locks are efficient spinlocks using CPU instructions,
   * but are vulnerable to starvation and can spin forever if a thread
   * holding a lock has been terminated.  For this reason you should
   * minimize the code executed inside an atomic lock and never do
   * expensive things like API or system calls while holding them.
   *
   * The atomic locks are not safe to lock recursively.
   *
   * Porting Note:
   * The spin lock functions and type are required and can not be
   * emulated because they are used in the atomic emulation code.
   *)
  SDL_SpinLock = SDL_SInt32;

  (**
   * \brief A type representing an atomic integer value.  It is a struct
   *        so people don't accidentally use numeric operations on it.
   *)
  SDL_atomic_t = record
    Value: SDL_SInt32;
  end;

