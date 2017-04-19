 
{$REGION 'SDL_InitFlags'}
operator in (const Left: SDL_InitFlag; const Right: SDL_InitFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left) and Right.Value) = UInt32(Ord(Left))
end;
 
operator or (const Left: SDL_InitFlag; const Right: SDL_InitFlag): SDL_InitFlags; inline;
begin
  Result.Value := Ord(Left) or Ord(Right) 
end;
 
operator or (const Left: SDL_InitFlag; const Right: SDL_InitFlags): SDL_InitFlags; inline;
begin
  Result.Value := UInt32(UInt32(Ord(Left)) or UInt32(Ord(Right.Value)))
end;
 
operator or (const Left: SDL_InitFlags; const Right: SDL_InitFlag): SDL_InitFlags; inline;
begin
  Result.Value := UInt32(Ord(Left.Value)) or UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_InitFlags; const Right: SDL_InitFlag): Boolean; inline;
begin
  Result := UInt32(Left.Value) = UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_InitFlag; const Right: SDL_InitFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) = UInt32(Right.Value) 
end;
 
operator = (const Left: SDL_InitFlags; const Right: SDL_InitFlags): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: SDL_InitFlags; const Right: SDL_InitFlag): Boolean; inline;
begin
  Result := UInt32(Left.Value) <> UInt32(Ord(Right)) 
end;
 
operator <> (const Left: SDL_InitFlag; const Right: SDL_InitFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) <> UInt32(Right.Value)
end;
 
operator <> (const Left: SDL_InitFlags; const Right: SDL_InitFlags): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: SDL_InitFlags; const Right: SDL_InitFlag): SDL_InitFlags; inline;
begin
  Result.Value := UInt32(Left.Value) or UInt32(Ord(Right)) 
end;
 
operator + (const Left: SDL_InitFlags; const Right: SDL_InitFlags): SDL_InitFlags; inline;
begin
  Result.Value := Left.Value or Right.Value 
end;
 
operator - (const Left: SDL_InitFlags; const Right: SDL_InitFlag): SDL_InitFlags; inline;
begin
  Result.Value := UInt32(Left.Value) xor UInt32(Ord(Right)) 
end;
 
operator := (const Value: SDL_InitFlag): SDL_InitFlags; inline;
begin
  Result.Value := Ord(Value) 
end;
 
operator := (const Value: SDL_InitFlag): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: SDL_InitFlags): String; inline;
begin
  Result := Value.ToString 
end;
 
{$ENDREGION}
 
{$REGION 'SDL_WindowFlags'}
operator in (const Left: SDL_WindowFlag; const Right: SDL_WindowFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) and UInt32(Right.Value) = UInt32(Ord(Left)) 
end;
 
operator or (const Left: SDL_WindowFlag; const Right: SDL_WindowFlag): SDL_WindowFlags; inline;
begin
  Result.Value := Ord(Left) or Ord(Right) 
end;
 
operator or (const Left: SDL_WindowFlag; const Right: SDL_WindowFlags): SDL_WindowFlags; inline;
begin
  Result.Value := UInt32(Ord(Left)) or UInt32(Ord(Right.Value)) 
end;
 
operator or (const Left: SDL_WindowFlags; const Right: SDL_WindowFlag): SDL_WindowFlags; inline;
begin
  Result.Value := UInt32(Ord(Left.Value)) or UInt32(Ord(Right))
end;
 
operator = (const Left: SDL_WindowFlags; const Right: SDL_WindowFlag): Boolean; inline;
begin
  Result := UInt32(Left.Value) = UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_WindowFlag; const Right: SDL_WindowFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) = UInt32(Right.Value) 
end;
 
operator = (const Left: SDL_WindowFlags; const Right: SDL_WindowFlags): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: SDL_WindowFlags; const Right: SDL_WindowFlag): Boolean; inline;
begin
  Result := UInt32(Left.Value) <> UInt32(Ord(Right)) 
end;
 
operator <> (const Left: SDL_WindowFlag; const Right: SDL_WindowFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) <> UInt32(Right.Value)
end;
 
operator <> (const Left: SDL_WindowFlags; const Right: SDL_WindowFlags): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: SDL_WindowFlags; const Right: SDL_WindowFlag): SDL_WindowFlags; inline;
begin
  Result.Value := UInt32(Left.Value) or UInt32(Ord(Right)) 
end;
 
operator + (const Left: SDL_WindowFlags; const Right: SDL_WindowFlags): SDL_WindowFlags; inline;
begin
  Result.Value := Left.Value or Right.Value 
end;
 
operator - (const Left: SDL_WindowFlags; const Right: SDL_WindowFlag): SDL_WindowFlags; inline;
begin
  Result.Value := UInt32(Left.Value) xor UInt32(Ord(Right))
end;
 
operator := (const Value: SDL_WindowFlag): SDL_WindowFlags; inline;
begin
  Result.Value := Ord(Value) 
end;
 
operator := (const Value: SDL_WindowFlag): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: SDL_WindowFlags): String; inline;
begin
  Result := Value.ToString 
end;
 
{$ENDREGION}
 
{$REGION 'SDL_Surface_Flags'}
operator in (const Left: SDL_Surface_Flag; const Right: SDL_Surface_Flags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) and UInt32(Right.Value) = UInt32(Ord(Left))
end;
 
operator or (const Left: SDL_Surface_Flag; const Right: SDL_Surface_Flag): SDL_Surface_Flags; inline;
begin
  Result.Value := Ord(Left) or Ord(Right) 
end;
 
operator or (const Left: SDL_Surface_Flag; const Right: SDL_Surface_Flags): SDL_Surface_Flags; inline;
begin
  Result.Value := UInt32(Ord(Left)) or UInt32(Ord(Right.Value)) 
end;
 
operator or (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flag): SDL_Surface_Flags; inline;
begin
  Result.Value := UInt32(Ord(Left.Value)) or UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flag): Boolean; inline;
begin
  Result := UInt32(Left.Value) = UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_Surface_Flag; const Right: SDL_Surface_Flags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) = UInt32(Right.Value) 
end;
 
operator = (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flags): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flag): Boolean; inline;
begin
  Result := UInt32(Left.Value) <> UInt32(Ord(Right)) 
end;
 
operator <> (const Left: SDL_Surface_Flag; const Right: SDL_Surface_Flags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) <> UInt32(Right.Value)
end;
 
operator <> (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flags): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flag): SDL_Surface_Flags; inline;
begin
  Result.Value := UInt32(Left.Value) or UInt32(Ord(Right)) 
end;
 
operator + (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flags): SDL_Surface_Flags; inline;
begin
  Result.Value := Left.Value or Right.Value 
end;
 
operator - (const Left: SDL_Surface_Flags; const Right: SDL_Surface_Flag): SDL_Surface_Flags; inline;
begin
  Result.Value := UInt32(Left.Value) xor UInt32(Ord(Right))
end;
 
operator := (const Value: SDL_Surface_Flag): SDL_Surface_Flags; inline;
begin
  Result.Value := Ord(Value) 
end;
 
operator := (const Value: SDL_Surface_Flag): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: SDL_Surface_Flags): String; inline;
begin
  Result := Value.ToString 
end;
 
{$ENDREGION}
 
{$REGION 'SDL_Keymods'}
operator in (const Left: SDL_Keymod; const Right: SDL_Keymods): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) and UInt32(Right.Value) = UInt32(Ord(Left)) 
end;
 
operator or (const Left: SDL_Keymod; const Right: SDL_Keymod): SDL_Keymods; inline;
begin
  Result.Value := Ord(Left) or Ord(Right) 
end;
 
operator or (const Left: SDL_Keymod; const Right: SDL_Keymods): SDL_Keymods; inline;
begin
  Result.Value := UInt32(Ord(Left)) or UInt32(Ord(Right.Value)) 
end;
 
operator or (const Left: SDL_Keymods; const Right: SDL_Keymod): SDL_Keymods; inline;
begin
  Result.Value := UInt32(Ord(Left.Value)) or UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_Keymods; const Right: SDL_Keymod): Boolean; inline;
begin
  Result := UInt32(Left.Value) = UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_Keymod; const Right: SDL_Keymods): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) = UInt32(Right.Value) 
end;
 
operator = (const Left: SDL_Keymods; const Right: SDL_Keymods): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: SDL_Keymods; const Right: SDL_Keymod): Boolean; inline;
begin
  Result := UInt32(Left.Value) <> UInt32(Ord(Right)) 
end;
 
operator <> (const Left: SDL_Keymod; const Right: SDL_Keymods): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) <> UInt32(Right.Value)
end;
 
operator <> (const Left: SDL_Keymods; const Right: SDL_Keymods): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: SDL_Keymods; const Right: SDL_Keymod): SDL_Keymods; inline;
begin
  Result.Value := UInt32(Left.Value) or UInt32(Ord(Right)) 
end;
 
operator + (const Left: SDL_Keymods; const Right: SDL_Keymods): SDL_Keymods; inline;
begin
  Result.Value := Left.Value or Right.Value 
end;
 
operator - (const Left: SDL_Keymods; const Right: SDL_Keymod): SDL_Keymods; inline;
begin
  Result.Value := UInt32(Left.Value) xor UInt32(Ord(Right)) 
end;
 
operator := (const Value: SDL_Keymod): SDL_Keymods; inline;
begin
  Result.Value := Ord(Value) 
end;
 
operator := (const Value: SDL_Keymod): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: SDL_Keymods): String; inline;
begin
  Result := Value.ToString 
end;
 
{$ENDREGION}
 
{$REGION 'SDL_EventTypes'}
operator in (const Left: SDL_EventType; const Right: SDL_EventTypes): Boolean; inline;
var
  i: Integer;
begin
  for i := Low(Right.Value) to High(Right.Value) do
    if Left = Right.Value[i] then
      Exit(True);

  Result := False
end;
 
operator or (const Left: SDL_EventType; const Right: SDL_EventType): SDL_EventTypes; inline;
begin
  SetLength(Result.Value, 2);
  Result.Value[0] := Left;
  Result.Value[1] := Right
end;
 
operator or (const Left: SDL_EventType; const Right: SDL_EventTypes): SDL_EventTypes; inline;
begin
  Result := Right + Left
end;

operator or (const Left: SDL_EventTypes; const Right: SDL_EventType): SDL_EventTypes; inline;
begin
  Result := Left + Right
end;
 
operator = (const Left: SDL_EventTypes; const Right: SDL_EventType): Boolean; inline;
begin
  if Length(Left.Value) = 1 then
    Result := Left.Value[0] = Right
  else
    Result := False
end;
 
operator = (const Left: SDL_EventType; const Right: SDL_EventTypes): Boolean; inline;
begin
  if Length(Right.Value) = 1 then
    Result := Right.Value[0] = Left
  else
    Result := False
end;
 
operator = (const Left: SDL_EventTypes; const Right: SDL_EventTypes): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: SDL_EventTypes; const Right: SDL_EventType): Boolean; inline;
begin
  Result := not (Left = Right)
end;
 
operator <> (const Left: SDL_EventType; const Right: SDL_EventTypes): Boolean; inline;
begin
  Result := not (Left = Right)
end;
 
operator <> (const Left: SDL_EventTypes; const Right: SDL_EventTypes): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: SDL_EventTypes; const Right: SDL_EventType): SDL_EventTypes; inline;
var
  i: Integer;
begin
  if not (Right in Left) then
  begin
    SetLength(Result.Value, Length(Left.Value) + 1);

    for i := Low(Result.Value) to High(Result.Value) - 1 do
      Result.Value[i] := Left.Value[i];
    Result.Value[High(Result.Value)] := Right;
  end;
end;
 
operator + (const Left: SDL_EventTypes; const Right: SDL_EventTypes): SDL_EventTypes; inline;
var
  i: Integer;
begin
  SetLength(Result.Value, Length(Left.Value));

  for i := Low(Left.Value) to High(Left.Value) do
    Result.Value[i] := Left.Value[i];

  for i := Low(Right.Value) to High(Right.Value) do
    Result := Result + Right.Value[i];
end;
 
operator - (const Left: SDL_EventTypes; const Right: SDL_EventType): SDL_EventTypes; inline;
var
  i: Integer;
begin
  SetLength(Result.Value, Length(Left.Value));

  for i := Low(Result.Value) to High(Result.Value) do
  begin
    if Left.Value[i] = Right then
      SetLength(Result.Value, Length(Result.Value) - 1)
    else
      Result.Value[i] := Left.Value[i];
  end;
end;
 
operator := (const Value: SDL_EventType): SDL_EventTypes; inline;
begin
  SetLength(Result.Value, 1);
  Result.Value[0] := Value
end;
 
operator := (const Value: SDL_EventType): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: SDL_EventTypes): String; inline;
begin
  Result := Value.ToString 
end;
 
{$ENDREGION}
 
{$REGION 'SDL_RendererFlags'}
operator in (const Left: SDL_RendererFlag; const Right: SDL_RendererFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) and UInt32(Right.Value) = UInt32(Ord(Left)) 
end;
 
operator or (const Left: SDL_RendererFlag; const Right: SDL_RendererFlag): SDL_RendererFlags; inline;
begin
  Result.Value := Ord(Left) or Ord(Right) 
end;
 
operator or (const Left: SDL_RendererFlag; const Right: SDL_RendererFlags): SDL_RendererFlags; inline;
begin
  Result.Value := UInt32(Ord(Left)) or UInt32(Ord(Right.Value))
end;
 
operator or (const Left: SDL_RendererFlags; const Right: SDL_RendererFlag): SDL_RendererFlags; inline;
begin
  Result.Value := UInt32(Ord(Left.Value)) or UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_RendererFlags; const Right: SDL_RendererFlag): Boolean; inline;
begin
  Result := UInt32(Left.Value) = UInt32(Ord(Right)) 
end;
 
operator = (const Left: SDL_RendererFlag; const Right: SDL_RendererFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) = UInt32(Right.Value) 
end;
 
operator = (const Left: SDL_RendererFlags; const Right: SDL_RendererFlags): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: SDL_RendererFlags; const Right: SDL_RendererFlag): Boolean; inline;
begin
  Result := UInt32(Left.Value) <> UInt32(Ord(Right)) 
end;
 
operator <> (const Left: SDL_RendererFlag; const Right: SDL_RendererFlags): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) <> UInt32(Right.Value)
end;
 
operator <> (const Left: SDL_RendererFlags; const Right: SDL_RendererFlags): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: SDL_RendererFlags; const Right: SDL_RendererFlag): SDL_RendererFlags; inline;
begin
  Result.Value := UInt32(Left.Value) or UInt32(Ord(Right)) 
end;
 
operator + (const Left: SDL_RendererFlags; const Right: SDL_RendererFlags): SDL_RendererFlags; inline;
begin
  Result.Value := Left.Value or Right.Value 
end;
 
operator - (const Left: SDL_RendererFlags; const Right: SDL_RendererFlag): SDL_RendererFlags; inline;
begin
  Result.Value := UInt32(Left.Value) xor UInt32(Ord(Right)) 
end;
 
operator := (const Value: SDL_RendererFlag): SDL_RendererFlags; inline;
begin
  Result.Value := Ord(Value) 
end;
 
operator := (const Value: SDL_RendererFlag): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: SDL_RendererFlags): String; inline;
begin
  Result := Value.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_ButtonEnum}
operator := (const Value: SDL_ButtonEnum): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_ButtonEnum; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_ButtonEnum): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_ButtonEnum; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_ButtonEnum): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_SwapInterval}
operator := (const Value: SDL_SwapInterval): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_SwapInterval; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_SwapInterval): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_SwapInterval; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_SwapInterval): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_Bool}
operator := (const Value: SDL_Bool): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_Bool; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_Bool): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_Bool; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_Bool): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_LogPriority}
operator := (const Value: SDL_LogPriority): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_LogPriority; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_LogPriority): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_LogPriority; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_LogPriority): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_LogCategory}
operator := (const Value: SDL_LogCategory): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_LogCategory; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_LogCategory): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_LogCategory; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_LogCategory): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_EventState}
operator := (const Value: SDL_EventState): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_EventState; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_EventState): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_EventState; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_EventState): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
{$REGION SDL_AudioFormat}
operator := (const Value: SDL_AudioFormat): String; inline;
begin
  Result := Value.ToString 
end;
 
operator + (const Left: SDL_AudioFormat; const Right: String): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: String; const Right: SDL_AudioFormat): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
operator + (const Left: SDL_AudioFormat; const Right: Char): String; inline;
begin
  Result := Left.ToString + Right 
end;
 
operator + (const Left: Char; const Right: SDL_AudioFormat): String; inline;
begin
  Result := Left + Right.ToString 
end;
 
{$ENDREGION}
