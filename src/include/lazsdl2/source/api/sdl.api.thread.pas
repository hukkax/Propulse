unit SDL.Api.Thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLThreadApi }

  TSDLThreadApi = class(TSubLibrary)
  public type
    TSDL_TLS_Destructor = procedure(Data: SDL_Data); cdecl;
    PSDL_ThreadFunction = ^TSDL_ThreadFunction;
    TSDL_ThreadFunction = function(data: SDL_Data): SDL_SInt32; cdecl;
    TpfnSDL_CurrentBeginThread = function(SecurityAttributes: Pointer; StackSize: LongWord; ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord; var ThreadId: TThreadID): Integer;
    TpfnSDL_CurrentEndThread = procedure(ExitCode: Integer);
  protected type
    {$IFDEF WINDOWS}
    TSDL_CreateThread = function(fn: TSDL_ThreadFunction; name: SDL_String; data: Pointer; pfnBeginThread: TpfnSDL_CurrentBeginThread; pfnEndThread: TpfnSDL_CurrentEndThread): PSDL_Thread; cdecl;
    {$ELSE}
    TSDL_CreateThread = function(fn: TSDL_ThreadFunction; name: SDL_String; data: Pointer): PSDL_Thread; cdecl;
    {$ENDIF}
    TSDL_GetThreadName = function(thread: PSDL_Thread): SDL_String cdecl;
    TSDL_ThreadID = function: SDL_ThreadID cdecl;
    TSDL_GetThreadID = function(thread: PSDL_Thread): SDL_ThreadID cdecl;
    TSDL_SetThreadPriority = function(priority: SDL_ThreadPriority): SDL_SInt32 cdecl;
    TSDL_WaitThread = procedure(thread: PSDL_Thread; status: PSDL_SInt32) cdecl;
    TSDL_WaitThreadEx = procedure(thread: PSDL_Thread; out status: SDL_SInt32) cdecl;
    TSDL_DetachThread = procedure(thread: PSDL_Thread); cdecl;
    TSDL_TLSCreate = function: SDL_TLSID cdecl;
    TSDL_TLSGet = function(id: SDL_TLSID): SDL_Data cdecl;
    TSDL_TLSSet = function(id: SDL_TLSID; value: SDL_Data; destructor_: TSDL_TLS_Destructor): SDL_SInt32 cdecl;
  public
    (**
     *  Create a thread.
     *
     *   Thread naming is a little complicated: Most systems have very small
     *    limits for the string length (Haiku has 32 bytes, Linux currently has 16,
     *    Visual C++ 6.0 has nine!), and possibly other arbitrary rules. You'll
     *    have to see what happens with your system's debugger. The name should be
     *    UTF-8 (but using the naming limits of C identifiers is a better bet).
     *   There are no requirements for thread naming conventions, so long as the
     *    string is null-terminated UTF-8, but these guidelines are helpful in
     *    choosing a name:
     *
     *    http://stackoverflow.com/questions/149932/naming-conventions-for-threads
     *
     *   If a system imposes requirements, SDL will try to munge the string for
     *    it (truncate, etc), but the original string contents will be available
     *    from SDL_GetThreadName().
     *)
    SDL_CreateThread: TSDL_CreateThread;
    (**
     * Get the thread name, as it was specified in SDL_CreateThread().
     *  This function returns a pointer to a UTF-8 string that names the
     *  specified thread, or NULL if it doesn't have a name. This is internal
     *  memory, not to be free()'d by the caller, and remains valid until the
     *  specified thread is cleaned up by SDL_WaitThread().
     *)
    SDL_GetThreadName: TSDL_GetThreadName;
    (**
     *  Get the thread identifier for the current thread.
     *)
    SDL_ThreadID: TSDL_ThreadID;
    (**
     *  Get the thread identifier for the specified thread.
     *
     *  Equivalent to SDL_ThreadID() if the specified thread is NULL.
     *)
    SDL_GetThreadID: TSDL_GetThreadID;
    (**
     *  Set the priority for the current thread
     *)
    SDL_SetThreadPriority: TSDL_SetThreadPriority;
    (**
     *  Wait for a thread to finish. Threads that haven't been detached will
     *  remain (as a "zombie") until this function cleans them up. Not doing so
     *  is a resource leak.
     *
     *  Once a thread has been cleaned up through this function, the SDL_Thread
     *  that references it becomes invalid and should not be referenced again.
     *  As such, only one thread may call SDL_WaitThread() on another.
     *
     *  The return code for the thread function is placed in the area
     *  pointed to by \c status, if \c status is not NULL.
     *
     *  You may not wait on a thread that has been used in a call to
     *  SDL_DetachThread(). Use either that function or this one, but not
     *  both, or behavior is undefined.
     *
     *  It is safe to pass NULL to this function; it is a no-op.
     *)
     (* @{ *)
    SDL_WaitThread: TSDL_WaitThread;
    SDL_WaitThreadEx: TSDL_WaitThreadEx;
     (* @} *)
    (**
     *  A thread may be "detached" to signify that it should not remain until
     *  another thread has called SDL_WaitThread() on it. Detaching a thread
     *  is useful for long-running threads that nothing needs to synchronize
     *  with or further manage. When a detached thread is done, it simply
     *  goes away.
     *
     *  There is no way to recover the return code of a detached thread. If you
     *  need this, don't detach the thread and instead use SDL_WaitThread().
     *
     *  Once a thread is detached, you should usually assume the SDL_Thread isn't
     *  safe to reference again, as it will become invalid immediately upon
     *  the detached thread's exit, instead of remaining until someone has called
     *  SDL_WaitThread() to finally clean it up. As such, don't detach the same
     *  thread more than once.
     *
     *  If a thread has already exited when passed to SDL_DetachThread(), it will
     *  stop waiting for a call to SDL_WaitThread() and clean up immediately.
     *  It is not safe to detach a thread that might be used with SDL_WaitThread().
     *
     *  You may not call SDL_WaitThread() on a thread that has been detached.
     *  Use either that function or this one, but not both, or behavior is
     *  undefined.
     *
     *  It is safe to pass NULL to this function; it is a no-op.
     *)
    SDL_DetachThread: TSDL_DetachThread;
    (**
     *  \brief Create an identifier that is globally visible to all threads but refers to data that is thread-specific.
     *
     *  \return The newly created thread local storage identifier, or 0 on error
     *
     *  \code
     *  static SDL_SpinLock tls_lock;
     *  static SDL_TLSID thread_local_storage;
     *
     *  void SetMyThreadData(void *value)
     *  {
     *      if (!thread_local_storage) {
     *          SDL_AtomicLock(&tls_lock);
     *          if (!thread_local_storage) {
     *              thread_local_storage = SDL_TLSCreate();
     *          }
     *          SDL_AtomicUnlock(&tls_lock);
     *      }
     *      SDL_TLSSet(thread_local_storage, value, 0);
     *  }
     *
     *  void *GetMyThreadData(void)
     *  {
     *      return SDL_TLSGet(thread_local_storage);
     *  }
     *  \endcode
     *
     *  \sa SDL_TLSGet()
     *  \sa SDL_TLSSet()
     *)
    SDL_TLSCreate: TSDL_TLSCreate;
    (**
     *  \brief Get the value associated with a thread local storage ID for the current thread.
     *
     *  \param id The thread local storage ID
     *
     *  \return The value associated with the ID for the current thread, or NULL if no value has been set.
     *
     *  \sa SDL_TLSCreate()
     *  \sa SDL_TLSSet()
     *)
    SDL_TLSGet: TSDL_TLSGet;
    (**
     *  \brief Set the value associated with a thread local storage ID for the current thread.
     *
     *  \param id The thread local storage ID
     *  \param value The value to associate with the ID for the current thread
     *  \param destructor A function called when the thread exits, to free the value.
     *
     *  \return 0 on success, -1 on error
     *
     *  \sa SDL_TLSCreate()
     *  \sa SDL_TLSGet()
     *)
    SDL_TLSSet: TSDL_TLSSet;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    function SDL_CreateThreadEx(fn: TSDL_ThreadFunction; name: SDL_String; data: Pointer): PSDL_Thread;
  end;

implementation

{ TSDLThreadApi }

procedure TSDLThreadApi.DoInit;
begin

end;

procedure TSDLThreadApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_CreateThread', @SDL_CreateThread);
  List.Add('SDL_GetThreadName', @SDL_GetThreadName);
  List.Add('SDL_ThreadID', @SDL_ThreadID);
  List.Add('SDL_GetThreadID', @SDL_GetThreadID);
  List.Add('SDL_SetThreadPriority', @SDL_SetThreadPriority);
  List.Add('SDL_WaitThread', @SDL_WaitThread);
  List.Add('SDL_WaitThread', @SDL_WaitThreadEx);
  List.Add('SDL_DetachThread', @SDL_DetachThread);
  List.Add('SDL_TLSCreate', @SDL_TLSCreate);
  List.Add('SDL_TLSGet', @SDL_TLSGet);
  List.Add('SDL_TLSSet', @SDL_TLSSet);
end;

function TSDLThreadApi.SDL_CreateThreadEx(fn: TSDL_ThreadFunction;
  name: SDL_String; data: Pointer): PSDL_Thread;
begin
  {$IFDEF WINDOWS}
  Result := SDL_CreateThread(fn, name, data, nil , nil);
  {$ELSE}
  Result := SDL_CreateThread(fn, name, data);
  {$ENDIF}
end;

end.

