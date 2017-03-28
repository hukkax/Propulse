unit Graphics32_LowLevel;

{$mode delphi}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface


{$IFDEF COMPILERFPC}
  // Set up internal CPU target directives according to FPC directives
  {$IFDEF CPU386}
    {$IFDEF CPUI386}
      // target is an Intel 80386 or later.
      {$DEFINE TARGET_x86}
      {$ASMMODE INTEL}
    {$ENDIF}
    {$IFDEF CPUX86_64}
      // target is a 64-bit processor (AMD or INTEL).
      {$DEFINE TARGET_x64}
      {$ASMMODE INTEL}
    {$ENDIF}
  {$IFDEF CPUX86_64}
    // target is a 64-bit processor (AMD or INTEL).
    {$DEFINE TARGET_x64}
    {$ASMMODE INTEL}
  {$ENDIF}
{$ENDIF}
{$ELSE}
  // check for XE2 64-Bit compiler define
  {$IFDEF CPUX64}
    {$DEFINE TARGET_x64}
  {$ELSE}
    // define default (delphi etc) target (32 bit Intel 80386 or later)
    {$DEFINE TARGET_x86}
  {$ENDIF}
{$ENDIF}

{$IFDEF PUREPASCAL}
  {$DEFINE USENATIVECODE}
  {$DEFINE USEMOVE}
{$ENDIF}
{$IFDEF USEINLINING}
  {$DEFINE USENATIVECODE}
{$ENDIF}

{$IFDEF TARGET_x64}
  {$DEFINE USENATIVECODE}
  {$DEFINE USEINLINING}
  {$DEFINE USEMOVE}
{$ENDIF}

{uses
  Graphics, GR32, GR32_Math;}

{ Clamp function restricts value to [0..255] range }
function Clamp(const Value: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ An analogue of FillChar for 32 bit values }
procedure FillLongword(var X; Count: Integer; Value: Longword);

procedure FillWord(var X; Count: Cardinal; Value: Longword);

{ An analogue of Move for 32 bit values }
{$IFDEF USEMOVE}
procedure MoveLongword(const Source; var Dest; Count: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}
{$ELSE}
procedure MoveLongword(const Source; var Dest; Count: Integer);
{$ENDIF}
procedure MoveWord(const Source; var Dest; Count: Integer);


{ Returns value constrained to [Lo..Hi] range}
function Constrain(const Value, Lo, Hi: Integer): Integer; {$IFDEF USEINLINING} inline; {$ENDIF} overload;
function Constrain(const Value, Lo, Hi: Single): Single; {$IFDEF USEINLINING} inline; {$ENDIF} overload;

{ Returns min./max. value of A, B and C }
function Min(const A, B, C: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Max(const A, B, C: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Clamp integer value to [0..Max] range }
function Clamp(Value, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
{ Same but [Min..Max] range }
function Clamp(Value, Min, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Wrap integer value to [0..Max] range }
function Wrap(Value, Max: Integer): Integer; overload;
{ Same but [Min..Max] range }
function Wrap(Value, Min, Max: Integer): Integer; overload;

{ Wrap single value to [0..Max] range }
function Wrap(Value, Max: Single): Single; overload; {$IFDEF USEINLINING} inline; {$ENDIF} overload;

{ Fast Wrap alternatives for cases where range + 1 is a power of two }
function WrapPow2(Value, Max: Integer): Integer; {$IFDEF USEINLINING} inline; {$ENDIF} overload;
function WrapPow2(Value, Min, Max: Integer): Integer; {$IFDEF USEINLINING} inline; {$ENDIF} overload;

{ Mirror integer value in [0..Max] range }
function Mirror(Value, Max: Integer): Integer; overload;
{ Same but [Min..Max] range }
function Mirror(Value, Min, Max: Integer): Integer; overload;

{ Fast Mirror alternatives for cases where range + 1 is a power of two }
function MirrorPow2(Value, Max: Integer): Integer; {$IFDEF USEINLINING} inline; {$ENDIF} overload;
function MirrorPow2(Value, Min, Max: Integer): Integer; {$IFDEF USEINLINING} inline; {$ENDIF} overload;

{ Fast Value div 255, correct result with Value in [0..66298] range }
function Div255(Value: Cardinal): Cardinal; {$IFDEF USEINLINING} inline; {$ENDIF}


implementation

uses
	{$IFDEF FPC}SysUtils;{$ENDIF}

{$R-}{$Q-}  // switch off overflow and range checking

function Clamp(const Value: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
 if Value > 255 then
   Result := 255
 else
 if Value < 0 then
   Result := 0
 else
   Result := Value;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        // in x64 calling convention parameters are passed in ECX, EDX, R8 & R9
        MOV     EAX,ECX
{$ENDIF}
        TEST    EAX,$FFFFFF00
        JNZ     @1
        RET
@1:     JS      @2
        MOV     EAX,$FF
        RET
@2:     XOR     EAX,EAX
{$ENDIF}
end;

{$IFDEF USENATIVECODE}
procedure FillLongword(var X; Count: Integer; Value: Longword);
var
	I: Integer;
	P: PIntegerArray;
begin
	P := PIntegerArray(@X);
	for I := Count - 1 downto 0 do
		P[I] := Integer(Value);
end;
{$ENDIF}
{$IFNDEF USENATIVECODE}
procedure FillLongword(var X; Count: Integer; Value: Longword);
	{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
        TEST       EDX, EDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        PUSH       EDI             // push EDI on stack
        MOV        EDI, EAX        // Point EDI to destination

        CMP        EDX, 32
        JL         @SmallLoop

        AND        EAX, 3          // get aligned count
        TEST       EAX, EAX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        EAX, EDI
        SHR        EAX, 2          // bytes to count
        AND        EAX, 3          // get aligned count
        ADD        EAX,-4
        NEG        EAX             // get count to advance
        JZ         @SetupMain
        SUB        EDX, EAX        // subtract aligning start from total count

@AligningLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AligningLoop

@SetupMain:
        MOV        EAX, EDX        // EAX = remaining count
        SHR        EAX, 2
        SHL        EAX, 2
        SUB        EDX, EAX        // EDX = remaining count
        SHR        EAX, 2

        MOVD       XMM0, ECX
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [EDI], XMM0
        ADD        EDI, 16
        DEC        EAX
        JNZ        @SSE2Loop

@SmallLoop:
        MOV        EAX,ECX
        MOV        ECX,EDX

        REP        STOSD           // Fill count dwords

@ExitPOP:
        POP        EDI

@Exit:
{$ENDIF}
{$IFDEF TARGET_x64}
        // RCX = X;   RDX = Count;   R8 = Value

        TEST       RDX, RDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        MOV        R9, RCX         // Point R9 to destination

        CMP        RDX, 32
        JL         @SmallLoop

        AND        RCX, 3          // get aligned count
        TEST       RCX, RCX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        RCX, R9
        SHR        RCX, 2          // bytes to count
        AND        RCX, 3          // get aligned count
        ADD        RCX,-4
        NEG        RCX             // get count to advance
        JZ         @SetupMain
        SUB        RDX, RCX        // subtract aligning start from total count

@AligningLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RCX
        JNZ        @AligningLoop

@SetupMain:
        MOV        RCX, RDX        // RCX = remaining count
        SHR        RCX, 2
        SHL        RCX, 2
        SUB        RDX, RCX        // RDX = remaining count
        SHR        RCX, 2

        MOVD       XMM0, R8D
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [R9], XMM0
        ADD        R9, 16
        DEC        RCX
        JNZ        @SSE2Loop

        TEST       RDX, RDX
        JZ         @Exit
@SmallLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RDX
        JNZ        @SmallLoop
@Exit:
{$ENDIF}
end;
{$ENDIF}

procedure FillWord(var X; Count: Cardinal; Value: LongWord);
{$IFDEF USENATIVECODE}
var
  I: Integer;
  P: PWordArray;
begin
  P := PWordArray(@X);
  for I := Count - 1 downto 0 do
    P[I] := Value;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JZ      @exit

        REP     STOSW    // Fill count words
@exit:
        POP     EDI
{$ENDIF}

{$IFDEF TARGET_x64}
        // ECX = X;   EDX = Count;   R8D = Value
        PUSH    RDI

        MOV     RDI,RCX  // Point EDI to destination
        MOV     EAX,R8D
        MOV     ECX,EDX
        TEST    ECX,ECX
        JZ      @exit

        REP     STOSW    // Fill count words
@exit:
        POP     RDI
{$ENDIF}
{$ENDIF}
end;

procedure MoveLongword(const Source; var Dest; Count: Integer);
{$IFDEF USEMOVE}
begin
  Move(Source, Dest, Count shl 2);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = Source;   EDX = Dest;   ECX = Count
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX
        CMP     EDI,ESI
        JE      @exit

        REP     MOVSD
@exit:
        POP     EDI
        POP     ESI
{$ENDIF}
{$IFDEF TARGET_x64}
        // RCX = Source;   RDX = Dest;   R8 = Count
        PUSH    RSI
        PUSH    RDI

        MOV     RSI,RCX
        MOV     RDI,RDX
        MOV     RCX,R8
        CMP     RDI,RSI
        JE      @exit

        REP     MOVSD
@exit:
        POP     RDI
        POP     RSI
{$ENDIF}
{$ENDIF}
end;

procedure MoveWord(const Source; var Dest; Count: Integer);
{$IFDEF USEMOVE}
begin
  Move(Source, Dest, Count shl 1);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,ECX
        CMP     EDI,ESI
        JE      @exit

        REP     MOVSW
@exit:
        POP     EDI
        POP     ESI
{$ENDIF}

{$IFDEF TARGET_x64}
        // ECX = X;   EDX = Count;   R8 = Value
        PUSH    RSI
        PUSH    RDI

        MOV     RSI,RCX
        MOV     RDI,RDX
        MOV     RAX,R8
        CMP     RDI,RSI
        JE      @exit

        REP     MOVSW
@exit:
        POP     RDI
        POP     RSI
{$ENDIF}
{$ENDIF}
end;

function Constrain(const Value, Lo, Hi: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if Value < Lo then
    Result := Lo
  else if Value > Hi then
    Result := Hi
  else
    Result := Value;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
        MOV       ECX,R8D
{$ENDIF}
        CMP       EDX,EAX
        CMOVG     EAX,EDX
        CMP       ECX,EAX
        CMOVL     EAX,ECX
{$ENDIF}
end;

function Constrain(const Value, Lo, Hi: Single): Single; overload;
begin
  if Value < Lo then Result := Lo
  else if Value > Hi then Result := Hi
  else Result := Value;
end;

function Max(const A, B, C: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if A > B then
    Result := A
  else
    Result := B;

  if C > Result then
    Result := C;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       RAX,RCX
        MOV       RCX,R8
{$ENDIF}
        CMP       EDX,EAX
        CMOVG     EAX,EDX
        CMP       ECX,EAX
        CMOVG     EAX,ECX
{$ENDIF}
end;

function Min(const A, B, C: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if A < B then
    Result := A
  else
    Result := B;

  if C < Result then
    Result := C;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       RAX,RCX
        MOV       RCX,R8
{$ENDIF}
        CMP       EDX,EAX
        CMOVL     EAX,EDX
        CMP       ECX,EAX
        CMOVL     EAX,ECX
{$ENDIF}
end;

function Clamp(Value, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if Value > Max then
    Result := Max
  else if Value < 0 then
    Result := 0
  else
    Result := Value;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX,ECX
        MOV     ECX,R8D
{$ENDIF}
        CMP     EAX,EDX
        JG      @Above
        TEST    EAX,EAX
        JL      @Below
        RET
@Above:
        MOV     EAX,EDX
        RET
@Below:
        MOV     EAX,0
        RET
{$ENDIF}
end;

function Clamp(Value, Min, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if Value > Max then
    Result := Max
  else if Value < Min then
    Result := Min
  else
    Result := Value;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX,ECX
        MOV     ECX,R8D
{$ENDIF}
        CMP     EDX,EAX
        CMOVG   EAX,EDX
        CMP     ECX,EAX
        CMOVL   EAX,ECX
{$ENDIF}
end;

function Wrap(Value, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if Value < 0 then
    Result := Max + (Value - Max) mod (Max + 1)
  else
    Result := Value mod (Max + 1);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX,ECX
        MOV     ECX,R8D
        LEA     ECX,[RDX+1]
{$ELSE}
        LEA     ECX,[EDX+1]
{$ENDIF}
        CDQ
        IDIV    ECX
        MOV     EAX,EDX
        TEST    EAX,EAX
        JNL     @Exit
        ADD     EAX,ECX
@Exit:
{$ENDIF}
end;

function Wrap(Value, Min, Max: Integer): Integer;
begin
  if Value < Min then
    Result := Max + (Value - Max) mod (Max - Min + 1)
  else
    Result := Min + (Value - Min) mod (Max - Min + 1);
end;

function Wrap(Value, Max: Single): Single;
begin
{$IFDEF USEFLOATMOD}
  Result := FloatMod(Value, Max);
{$ELSE}
  Result := Value;
  while Result >= Max do Result := Result - Max;
  while Result < 0 do Result := Result + Max;
{$ENDIF}
end;

function DivMod(Dividend, Divisor: Integer; out Remainder: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  Remainder := Dividend mod Divisor;
  Result := Dividend div Divisor;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        PUSH      EBX
        MOV       EBX,EDX
        CDQ
        IDIV      EBX
        MOV       [ECX],EDX
        POP       EBX
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH      RBX
        MOV       EAX,ECX
        MOV       ECX,R8D
        MOV       EBX,EDX
        CDQ
        IDIV      EBX
        MOV       [RCX],EDX
        POP       RBX
{$ENDIF}
{$ENDIF}
end;

function Mirror(Value, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
var
  DivResult: Integer;
begin
  if Value < 0 then
  begin
    DivResult := DivMod(Value - Max, Max + 1, Result);
    Inc(Result, Max);
  end
  else
    DivResult := DivMod(Value, Max + 1, Result);

  if Odd(DivResult) then
    Result := Max - Result;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
        MOV       ECX,R8D
{$ENDIF}
        TEST      EAX,EAX
        JNL       @@1
        NEG       EAX
@@1:
        MOV       ECX,EDX
        CDQ
        IDIV      ECX
        TEST      EAX,1
        MOV       EAX,EDX
        JZ        @Exit
        NEG       EAX
        ADD       EAX,ECX
@Exit:
{$ENDIF}
end;

function Mirror(Value, Min, Max: Integer): Integer;
var
  DivResult: Integer;
begin
  if Value < Min then
  begin
    DivResult := DivMod(Value - Max, Max - Min + 1, Result);
    Inc(Result, Max);
  end
  else
  begin
    DivResult := DivMod(Value - Min, Max - Min + 1, Result);
    Inc(Result, Min);
  end;
  if Odd(DivResult) then Result := Max + Min - Result;
end;

function WrapPow2(Value, Max: Integer): Integer; overload;
begin
  Result := Value and Max;
end;

function WrapPow2(Value, Min, Max: Integer): Integer; overload;
begin
  Result := (Value - Min) and (Max - Min) + Min;
end;

function MirrorPow2(Value, Max: Integer): Integer; overload;
begin
  if Value and (Max + 1) = 0 then
    Result := Value and Max
  else
    Result := Max - Value and Max;
end;

function MirrorPow2(Value, Min, Max: Integer): Integer; overload;
begin
  Value := Value - Min;
  Result := Max - Min;

  if Value and (Result + 1) = 0 then
    Result := Min + Value and Result
  else
    Result := Max - Value and Result;
end;

function Div255(Value: Cardinal): Cardinal;
begin
  Result := (Value * $8081) shr 23;
end;

{ shift right with sign conservation }
function SAR_3(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 8;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,3
{$ENDIF}
end;

function SAR_4(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 16;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,4
{$ENDIF}
end;

function SAR_6(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 64;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,6
{$ENDIF}
end;

function SAR_8(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 256;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,8
{$ENDIF}
end;

function SAR_9(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 512;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,9
{$ENDIF}
end;

function SAR_11(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 2048;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,11
{$ENDIF}
end;

function SAR_12(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 4096;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,12
{$ENDIF}
end;

function SAR_13(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 8192;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,13
{$ENDIF}
end;

function SAR_14(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 16384;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,14
{$ENDIF}
end;

function SAR_15(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 32768;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,15
{$ENDIF}
end;

function SAR_16(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 65536;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,16
{$ENDIF}
end;


end.

