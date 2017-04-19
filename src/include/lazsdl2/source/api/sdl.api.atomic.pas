(**
 * \file SDL_atomic.h
 *
 * Atomic operations.
 *
 * IMPORTANT:
 * If you are not an expert in concurrent lockless programming, you should
 * only be using the atomic lock and reference counting functions in this
 * file.  In all other cases you should be protecting your data structures
 * with full mutexes.
 *
 * The list of "safe" functions to use are:
 *  SDL_AtomicLock()
 *  SDL_AtomicUnlock()
 *  SDL_AtomicIncRef()
 *  SDL_AtomicDecRef()
 *
 * Seriously, here be dragons!
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *
 * You can find out a little more about lockless programming and the
 * subtle issues that can arise here:
 * http://msdn.microsoft.com/en-us/library/ee418650%28v=vs.85%29.aspx
 *
 * There's also lots of good information here:
 * http://www.1024cores.net/home/lock-free-algorithms
 * http://preshing.com/
 *
 * These operations may or may not actually be implemented using
 * processor specific atomic operations. When possible they are
 * implemented as true processor specific atomic operations. When that
 * is not possible the are implemented using locks that *do* use the
 * available atomic operations.
 *
 * All of the atomic operations that modify memory are full memory barriers.
 *)
unit SDL.Api.Atomic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLAtomicApi }

  TSDLAtomicApi = class(TSubLibrary)
  protected type
    TSDL_AtomicTryLock = function(lock: PSDL_SpinLock): SDL_Bool cdecl;
    TSDL_AtomicLock = procedure(lock: PSDL_SpinLock) cdecl;
    TSDL_AtomicUnlock = procedure(lock: PSDL_SpinLock) cdecl;
    TSDL_AtomicCAS = function(a: PSDL_atomic_t; oldval: SDL_SInt32; newval: SDL_SInt32): SDL_Bool cdecl;
    TSDL_AtomicSet = function(a: PSDL_atomic_t; v: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_AtomicGet = function(a: PSDL_atomic_t): SDL_SInt32 cdecl;
    TSDL_AtomicAdd = function(a: PSDL_atomic_t; v: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_AtomicCASPtr = function(a: PPSDL_atomic_t; oldval: PSDL_SInt32; newval: PSDL_SInt32): SDL_Bool cdecl;
    TSDL_AtomicSetPtr = function(a: PPSDL_atomic_t; oldval: PSDL_SInt32): SDL_Pointer cdecl;
    TSDL_AtomicGetPtr = function(a: PPSDL_atomic_t): SDL_Pointer cdecl;
  public
    (**
     * \brief Try to lock a spin lock by setting it to a non-zero value.
     *
     * \param lock Points to the lock.
     *
     * \return SDL_TRUE if the lock succeeded, SDL_FALSE if the lock is already held.
     *)
    SDL_AtomicTryLock: TSDL_AtomicTryLock;
    (**
     * \brief Lock a spin lock by setting it to a non-zero value.
     *
     * \param lock Points to the lock.
     *)
    SDL_AtomicLock: TSDL_AtomicLock;
    (**
     * \brief Unlock a spin lock by setting it to 0. Always returns immediately
     *
     * \param lock Points to the lock.
     *)
    SDL_AtomicUnlock: TSDL_AtomicUnlock;
    (**
     * \brief Set an atomic variable to a new value if it is currently an old value.
     *
     * \return SDL_TRUE if the atomic variable was set, SDL_FALSE otherwise.
     *
     * \note If you don't know what this function is for, you shouldn't use it!
    *)
    SDL_AtomicCAS: TSDL_AtomicCAS;
    (**
     * \brief Set an atomic variable to a value.
     *
     * \return The previous value of the atomic variable.
     *)
    SDL_AtomicSet: TSDL_AtomicSet;
    (**
     * \brief Get the value of an atomic variable
     *)
    SDL_AtomicGet: TSDL_AtomicGet;
    (**
     * \brief Add to an atomic variable.
     *
     * \return The previous value of the atomic variable.
     *
     * \note This same style can be used for any number operation
     *)
    SDL_AtomicAdd: TSDL_AtomicAdd;
    (**
     * \brief Set a pointer to a new value if it is currently an old value.
     *
     * \return SDL_TRUE if the pointer was set, SDL_FALSE otherwise.
     *
     * \note If you don't know what this function is for, you shouldn't use it!
    *)
    SDL_AtomicCASPtr: TSDL_AtomicCASPtr;
    (**
     * \brief Set a pointer to a value atomically.
     *
     * \return The previous value of the pointer.
     *)
    SDL_AtomicSetPtr: TSDL_AtomicSetPtr;
    (**
     * \brief Get the value of a pointer atomically.
     *)
    SDL_AtomicGetPtr: TSDL_AtomicGetPtr;
  protected
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     * \brief Increment an atomic variable used as a reference count.
     *)
    procedure SDL_AtomicIncRef(a: PSDL_atomic_t);
    (**
     * \brief Decrement an atomic variable used as a reference count.
     *
     * \return SDL_TRUE if the variable reached zero after decrementing,
     *         SDL_FALSE otherwise
     *)
    function SDL_AtomicDecRef(a: PSDL_atomic_t): SDL_Bool;
  end;

implementation

{ TSDLAtomicApi }

procedure TSDLAtomicApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_AtomicTryLock', @SDL_AtomicTryLock);
  List.Add('SDL_AtomicLock', @SDL_AtomicLock);
  List.Add('SDL_AtomicUnlock', @SDL_AtomicUnlock);
  List.Add('SDL_AtomicCAS', @SDL_AtomicCAS);
  List.Add('SDL_AtomicSet', @SDL_AtomicSet);
  List.Add('SDL_AtomicGet', @SDL_AtomicGet);
  List.Add('SDL_AtomicAdd', @SDL_AtomicAdd);
  List.Add('SDL_AtomicCASPtr', @SDL_AtomicCASPtr);
  List.Add('SDL_AtomicSetPtr', @SDL_AtomicSetPtr);
  List.Add('SDL_AtomicGetPtr', @SDL_AtomicGetPtr);
end;

procedure TSDLAtomicApi.SDL_AtomicIncRef(a: PSDL_atomic_t);
begin
  SDL_AtomicAdd(a, 1)
end;

function TSDLAtomicApi.SDL_AtomicDecRef(a: PSDL_atomic_t): SDL_Bool;
begin
  if SDL_AtomicAdd(a, -1) = 0 then
    Result := SDL_TRUE
  else
    Result := SDL_FALSE
end;

end.

