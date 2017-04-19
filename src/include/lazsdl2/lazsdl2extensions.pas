{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazSDL2Extensions;

interface

uses
  SDL.Android, SDL.Init, Event.Logger, SDL.Extended.Application, 
  SDL.Extended.AtomicLock, SDL.Extended.Camera, 
  SDL.Extended.ConditionVariable, SDL.Extended.Context, 
  SDL.Extended.Interfaces, SDL.Extended.LibObject, 
  SDL.Extended.LibraryProvider, SDL.Extended.Loaders, SDL.Extended.Mutex, 
  SDL.Extended.OpenGLExtensions, SDL.Extended.Renderer, 
  SDL.Extended.Semaphore, SDL.Extended.Stream, SDL.Extended.Strings, 
  SDL.Extended.Thread, SDL.Extended.Types, SDL.Extended.Utils, 
  SDL.Extended.Window, SDL.Extended.WorkerControl, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazSDL2Extensions', @Register);
end.
