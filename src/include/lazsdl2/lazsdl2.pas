{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazSDL2;

interface

uses
  DLibrary, SDL.Api.Assert, SDL.Api.Atomic, SDL.Api.Audio, SDL.Api.Clipboard, 
  SDL.APi.Consts, SDL.Api.CPUInfo, SDL.Api.Error, SDL.Api.Events, 
  SDL.Api.FileSystem, SDL.Api.GameController, SDL.Api.Gesture, SDL.Api.Haptic, 
  SDL.Api.Helpers, SDL.Api.Hints, SDL.Api.Keyboard, SDL.Api.libSDL2, 
  SDL.Api.libSDL_Image, SDL.Api.LoadSO, SDL.Api.Log, SDL.Api.Main, 
  SDL.Api.MessageBox, SDL.Api.Mouse, SDL.Api.Mutex, SDL.Api.OpenGL, 
  SDL.Api.OpenGL.Helpers, SDL.Api.OpenGL.Types, SDL.Api.Options, 
  SDL.Api.Pixels, SDL.Api.Platform, SDL.Api.Power, SDL.Api.Rect, 
  SDL.Api.Render, SDL.Api.RWops, SDL.Api.Shape, SDL.Api.StdInc, 
  SDL.Api.Surface, SDL.Api.Thread, SDL.Api.Timer, SDL.Api.Touch, 
  SDL.Api.Types, SDL.Api.Version, SDL.Api.Video, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazSDL2', @Register);
end.
