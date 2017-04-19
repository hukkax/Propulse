unit SDL.Android;

{$mode objfpc}{$H+}

interface

{$IFDEF ANDROID}
uses
  {%H-}SDL.Android.AssetManager,
  {%H-}SDL.Android.Input,
  {%H-}SDL.Android.Logger,
  {%H-}SDL.Android.Looper,
  {%H-}SDL.Android.NativeActivity,
  {%H-}SDL.Android.NativeWindow,
  {%H-}SDL.Android.Rect;
{$ENDIF}

implementation

end.

