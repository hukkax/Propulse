(**
 *  \file SDL_mutex.h
 *
 *  Functions to provide thread synchronization primitives.
 *)
unit SDL.Api.Mutex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLMutexApi }

  TSDLMutexApi = class(TSubLibrary)
  private type
    TSDL_CreateMutex = function: PSDL_Mutex cdecl;
    TSDL_LockMutex = function(mutex: PSDL_Mutex): SDL_ResultCode cdecl;
    TSDL_TryLockMutex = function(mutex: PSDL_Mutex): SDL_ResultCode cdecl;
    TSDL_UnlockMutex = function(mutex: PSDL_Mutex): SDL_ResultCode cdecl;
    TSDL_DestroyMutex = procedure(mutex: PSDL_Mutex) cdecl;
    TSDL_CreateSemaphore = function(initial_value: SDL_UInt32): PSDL_semaphore cdecl;
    TSDL_DestroySemaphore = procedure(sem: PSDL_semaphore) cdecl;
    TSDL_SemWait = function(sem: PSDL_semaphore): SDL_ResultCode cdecl;
    TSDL_SemTryWait = function(sem: PSDL_semaphore): SDL_ResultCode cdecl;
    TSDL_SemWaitTimeout = function(sem: PSDL_semaphore; ms: SDL_UInt32): SDL_ResultCode cdecl;
    TSDL_SemPost = function(sem: PSDL_semaphore): SDL_ResultCode cdecl;
    TSDL_SemValue = function(sem: PSDL_semaphore): SDL_UInt32 cdecl;
    TSDL_CreateCond = function: PSDL_Cond cdecl;
    TSDL_DestroyCond = procedure(cond: PSDL_Cond) cdecl;
    TSDL_CondSignal = function(cond: PSDL_Cond): SDL_ResultCode cdecl;
    TSDL_CondBroadcast = function(cond: PSDL_Cond): SDL_ResultCode cdecl;
    TSDL_CondWait = function(cond: PSDL_Cond; mutex: PSDL_Mutex): SDL_ResultCode cdecl;
    TSDL_CondWaitTimeout = function(cond: PSDL_Cond; mutex: PSDL_Mutex; ms: SDL_UInt32): SDL_ResultCode cdecl;
  public
    (**
      *  Create a mutex, initialized unlocked.
      *
      * \sa SDL_LockMutex
      *)
    SDL_CreateMutex: TSDL_CreateMutex;
    (**
      * Lock the mutex.
      *
      * \return 0, or -1 on error.
      *
      * \sa SDL_LockMutex
      *)
    SDL_LockMutex: TSDL_LockMutex;
    (**
      *  Try to lock the mutex
      *
      *  \return 0, SDL_MUTEX_TIMEDOUT, or -1 on error
      *)
    SDL_TryLockMutex: TSDL_TryLockMutex;
    (**
      *  Unlock the mutex.
      *
      *  \return 0, or -1 on error.
      *
      *  \warning It is an error to unlock a mutex that has not been locked by
      *           the current thread, and doing so results in undefined behavior.
      *)
    SDL_UnlockMutex: TSDL_UnlockMutex;
    (**
      *  Destroy a mutex.
      *)
    SDL_DestroyMutex: TSDL_DestroyMutex;
    (**
      *  Create a semaphore, initialized with value, returns NULL on failure.
      *)
    SDL_CreateSemaphore: TSDL_CreateSemaphore;
    (**
      *  Destroy a semaphore.
      *)
    SDL_DestroySemaphore: TSDL_DestroySemaphore;
    (**
      *  This function suspends the calling thread until the semaphore pointed
      *  to by \c sem has a positive count. It then atomically decreases the
      *  semaphore count.
      *)
    SDL_SemWait: TSDL_SemWait;
    (**
      *  Non-blocking variant of SDL_SemWait().
      *
      *  \return 0 if the wait succeeds, ::SDL_MUTEX_TIMEDOUT if the wait would
      *          block, and -1 on error.
      *)
    SDL_SemTryWait: TSDL_SemTryWait;
    (**
      *  Variant of SDL_SemWait() with a timeout in milliseconds.
      *
      *  \return 0 if the wait succeeds, ::SDL_MUTEX_TIMEDOUT if the wait does not
      *          succeed in the allotted time, and -1 on error.
      *
      *  \warning On some platforms this function is implemented by looping with a
      *           delay of 1 ms, and so should be avoided if possible.
      *)
    SDL_SemWaitTimeout: TSDL_SemWaitTimeout;
    (**
      *  Atomically increases the semaphore's count (not blocking).
      *
      *  \return 0, or -1 on error.
      *)
    SDL_SemPost: TSDL_SemPost;
    (**
      *  Returns the current count of the semaphore.
      *)
    SDL_SemValue: TSDL_SemValue;
    (**
      *  Create a condition variable.
      *
      *  Typical use of condition variables:
      *
      *  Thread A:
      *    SDL_LockMutex(lock);
      *    while ( ! condition ) {
      *        SDL_CondWait(cond, lock);
      *    }
      *    SDL_UnlockMutex(lock);
      *
      *  Thread B:
      *    SDL_LockMutex(lock);
      *    ...
      *    condition = true;
      *    ...
      *    SDL_CondSignal(cond);
      *    SDL_UnlockMutex(lock);
      *
      *  There is some discussion whether to signal the condition variable
      *  with the mutex locked or not.  There is some potential performance
      *  benefit to unlocking first on some platforms, but there are some
      *  potential race conditions depending on how your code is structured.
      *
      *  In general it's safer to signal the condition variable while the
      *  mutex is locked.
      *)
    SDL_CreateCond: TSDL_CreateCond;
    (**
      *  Destroy a condition variable.
      *)
    SDL_DestroyCond: TSDL_DestroyCond;
    (**
      *  Restart one of the threads that are waiting on the condition variable.
      *
      *  \return 0 or -1 on error.
      *)
    SDL_CondSignal: TSDL_CondSignal;
    (**
      *  Restart all threads that are waiting on the condition variable.
      *
      *  \return 0 or -1 on error.
      *)
    SDL_CondBroadcast: TSDL_CondBroadcast;
    (**
      *  Wait on the condition variable, unlocking the provided mutex.
      *
      *  \warning The mutex must be locked before entering this function!
      *
      *  The mutex is re-locked once the condition variable is signaled.
      *
      *  \return 0 when it is signaled, or -1 on error.
      *)
    SDL_CondWait: TSDL_CondWait;
    (**
      *  Waits for at most \c ms milliseconds, and returns 0 if the condition
      *  variable is signaled, ::SDL_MUTEX_TIMEDOUT if the condition is not
      *  signaled in the allotted time, and -1 on error.
      *
      *  \warning On some platforms this function is implemented by looping with a
      *           delay of 1 ms, and so should be avoided if possible.
      *)
    SDL_CondWaitTimeout: TSDL_CondWaitTimeout;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  end;

implementation

{ TSDLMutexApi }

procedure TSDLMutexApi.DoInit;
begin

end;

procedure TSDLMutexApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_CreateMutex', @SDL_CreateMutex);
  List.Add('SDL_LockMutex', @SDL_LockMutex);
  List.Add('SDL_TryLockMutex', @SDL_TryLockMutex);
  List.Add('SDL_UnlockMutex', @SDL_UnlockMutex);
  List.Add('SDL_DestroyMutex', @SDL_DestroyMutex);
  List.Add('SDL_CreateSemaphore', @SDL_CreateSemaphore);
  List.Add('SDL_DestroySemaphore', @SDL_DestroySemaphore);
  List.Add('SDL_SemWait', @SDL_SemWait);
  List.Add('SDL_SemTryWait', @SDL_SemTryWait);
  List.Add('SDL_SemWaitTimeout', @SDL_SemWaitTimeout);
  List.Add('SDL_SemPost', @SDL_SemPost);
  List.Add('SDL_SemValue', @SDL_SemValue);
  List.Add('SDL_CreateCond', @SDL_CreateCond);
  List.Add('SDL_DestroyCond', @SDL_DestroyCond);
  List.Add('SDL_CondSignal', @SDL_CondSignal);
  List.Add('SDL_CondBroadcast', @SDL_CondBroadcast);
  List.Add('SDL_CondWait', @SDL_CondWait);
  List.Add('SDL_CondWaitTimeout', @SDL_CondWaitTimeout);
end;

end.

