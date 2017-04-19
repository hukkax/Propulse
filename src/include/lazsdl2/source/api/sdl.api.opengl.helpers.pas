unit SDL.Api.OpenGL.Helpers;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, SDL.Api.OpenGL.Types;

type
  { TGL_ClearBufferTypeHelper }

  TGL_ClearBufferTypeHelper = record helper for GL_ClearBufferType
  const
    {$IFNDEF ANDROID}
    GL_ClearBufferTypeLength = 4;
    {$ELSE}
    GL_ClearBufferTypeLength = 3;
    {$ENDIF}
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TGL_ClearBufferTypesHelper }

  TGL_ClearBufferTypesHelper = record helper for GL_ClearBufferTypes
  public
    function Int: SDL_SInt32;
    function ToString: String;
  end;

  { TGL_ClipPlaneHelper }

  TGL_ClipPlaneHelper = record helper for GL_ClipPlane
  public
    function GL_CLIP_PLANE(const Num: Int32): GL_ClipPlane;
  end;

implementation

{ TGL_ClipPlaneHelper }

function TGL_ClipPlaneHelper.GL_CLIP_PLANE(const Num: Int32): GL_ClipPlane;
begin
  Result := GL_ClipPlane(Ord(GL_CLIP_PLANE0) + Num)
end;

{ TGL_ClearBufferTypesHelper }

function TGL_ClearBufferTypesHelper.Int: SDL_SInt32;
begin
  Result := Self.Value
end;

function TGL_ClearBufferTypesHelper.ToString: String;
const
  cArray: array [0..TGL_ClearBufferTypeHelper.GL_ClearBufferTypeLength - 1] of GL_ClearBufferType = (
    GL_DEPTH_BUFFER_BIT,
    {$IFNDEF ANDROID}
    GL_ACCUM_BUFFER_BIT,
    {$ENDIF}
    GL_STENCIL_BUFFER_BIT,
    GL_COLOR_BUFFER_BIT
  );
var
  Enum: GL_ClearBufferType;
begin
  Result := '[';
  for Enum in cArray do
  begin
    if Enum in Self then
    begin
      if Result = '[' then
        Result := Result + Enum.ToString
      else
        Result := Result + ',' + Enum.ToString
    end
  end;

  Result := Result + ']'
end;


{ TGL_ClearBufferTypeHelper }

function TGL_ClearBufferTypeHelper.Int: SDL_SInt32;
begin
  Result := Ord(Self)
end;

function TGL_ClearBufferTypeHelper.ToString: String;
begin
  case Self of
    GL_DEPTH_BUFFER_BIT: Result := 'GL_DEPTH_BUFFER_BIT';
    {$IFNDEF ANDROID}
    GL_ACCUM_BUFFER_BIT: Result := 'GL_ACCUM_BUFFER_BIT';
    {$ENDIF}
    GL_STENCIL_BUFFER_BIT: Result := 'GL_STENCIL_BUFFER_BIT';
    GL_COLOR_BUFFER_BIT: Result := 'GL_COLOR_BUFFER_BIT';
  else
    Result := 'GL_CLEAR_UNKNOWN'
  end;
end;

end.

