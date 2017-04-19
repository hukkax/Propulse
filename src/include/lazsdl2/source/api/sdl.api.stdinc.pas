(**
 *  \file SDL_stdinc.h
 *
 *  This is a general header that includes C language support.
 *)
unit SDL.Api.StdInc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLStrIncApi }

  TSDLStrIncApi = class(TSubLibrary)
  public type
    TSDL_Compare = function(const Left, Right: Pointer): SDL_SInt32;
  private type
    TSDL_malloc = function(size: SDL_Size_t): Pointer cdecl;
    TSDL_calloc = function(nmemb: SDL_Size_t; size: SDL_Size_t): Pointer cdecl;
    TSDL_realloc = function(mem: Pointer; size: SDL_Size_t): Pointer cdecl;
    TSDL_free = procedure(mem: Pointer) cdecl;
    TSDL_setenv = function(const name: SDL_String; const value: SDL_String; overwrite: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_getenv = function(const name: SDL_String): SDL_String cdecl;
    TSDL_qsort = procedure(base: Pointer; nmemb: SDL_Size_t; size: SDL_Size_t; compare: TSDL_Compare) cdecl;
    TSDL_abs = function(x: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_isdigit = function(x: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_isspace = function(x: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_toupper = function(x: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_tolower = function(x: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_memset = function(dst: Pointer; c: SDL_SInt32; len: SDL_Size_t): Pointer cdecl;
    TSDL_memcpy = function(dst: Pointer; src: Pointer; len: SDL_Size_t): Pointer cdecl;
    TSDL_memmove = function(dst: Pointer; src: Pointer; len: SDL_Size_t): Pointer cdecl;
    TSDL_memcmp = function(const dst: Pointer; src: Pointer; len: SDL_Size_t): SDL_SInt32 cdecl;
    TSDL_wcslen = function(const wstr: SDL_WideString): SDL_Size_t cdecl;
    TSDL_wcslcpy = function(dst: SDL_WideString; const src: SDL_WideString; maxlen: SDL_Size_t): SDL_Size_t cdecl;
    TSDL_wcslcat = function(dst: SDL_WideString; const src: SDL_WideString; maxlen: SDL_Size_t): SDL_Size_t cdecl;
    TSDL_strlen = function(const str: SDL_String): SDL_Size_t cdecl;
    TSDL_strlcpy = function(dst: SDL_String; const src: SDL_String; maxlen: SDL_Size_t): SDL_Size_t cdecl;
    TSDL_utf8strlcpy = function(dst: SDL_String; const src: SDL_String; dst_bytes: SDL_Size_t): SDL_Size_t cdecl;
    TSDL_strlcat = function(dst: SDL_String; const src: SDL_String; maxlen: SDL_Size_t): SDL_Size_t cdecl;
    TSDL_strdup = function(const str: SDL_String): SDL_String cdecl;
    TSDL_strrev = function(str: SDL_String): SDL_String cdecl;
    TSDL_strupr = function(str: SDL_String): SDL_String cdecl;
    TSDL_strlwr = function(str: SDL_String): SDL_String cdecl;
    TSDL_strchr = function(const str: SDL_String; c: SDL_SInt32): SDL_String cdecl;
    TSDL_strrchr = function(const str: SDL_String; c: SDL_SInt32): SDL_String cdecl;
    TSDL_strstr = function(const haystack: SDL_String; const needle: SDL_String): SDL_String cdecl;
    TSDL_itoa = function(value: SDL_SInt32; str: SDL_String; radix: SDL_SInt32): SDL_String cdecl;
    TSDL_uitoa = function(value: SDL_UInt32; str: SDL_String; radix: SDL_SInt32): SDL_String cdecl;
    TSDL_ltoa = function(value: SDL_SInt32; str: SDL_String; radix: SDL_SInt32): SDL_String cdecl;
    TSDL_ultoa = function(value: SDL_UInt32; str: SDL_String; radix: SDL_SInt32): SDL_String cdecl;
    TSDL_lltoa = function(value: SDL_SInt64; str: SDL_String; radix: SDL_SInt32): SDL_String cdecl;
    TSDL_ulltoa = function(value: SDL_UInt64; str: SDL_String; radix: SDL_SInt32): SDL_String cdecl;
    TSDL_atoi = function(const str: SDL_String): SDL_SInt32 cdecl;
    TSDL_atof = function(const str: SDL_String): SDL_Double cdecl;
    TSDL_strtol = function(const str: SDL_String; endp: PSDL_String; base: SDL_SInt32): SDL_SInt32 cdecl;
    TSDL_strtoul = function(const str: SDL_String; endp: PSDL_String; base: SDL_SInt32): SDL_UInt32 cdecl;
    TSDL_strtoll = function(const str: SDL_String; endp: PSDL_String; base: SDL_SInt32): SDL_SInt64 cdecl;
    TSDL_strtoull = function(const str: SDL_String; endp: PSDL_String; base: SDL_SInt32): SDL_UInt64 cdecl;
    TSDL_strtod = function(const str: SDL_String; endp: PSDL_String): SDL_Double cdecl;
    TSDL_strcmp = function(const str1: SDL_String; const str2: SDL_String): SDL_SInt32 cdecl;
    TSDL_strncmp = function(const str1: SDL_String; const str2: SDL_String; maxlen: SDL_Size_t): SDL_SInt32 cdecl;
    TSDL_strcasecmp = function(const str1: SDL_String; const str2: SDL_String): SDL_SInt32 cdecl;
    TSDL_strncasecmp = function(const str1: SDL_String; const str2: SDL_String; len: SDL_Size_t): SDL_SInt32 cdecl;
    TSDL_acos = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_asin = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_atan = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_atan2 = function(x: SDL_Double; y: SDL_Double): SDL_Double cdecl;
    TSDL_ceil = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_copysign = function(x: SDL_Double; y: SDL_Double): SDL_Double cdecl;
    TSDL_cos = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_cosf = function(x: SDL_Float): SDL_Double cdecl;
    TSDL_fabs = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_floor = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_log = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_pow = function(x: SDL_Double; y: SDL_Double): SDL_Double cdecl;
    TSDL_scalbn = function(x: SDL_Double; n: SDL_SInt32): SDL_Double cdecl;
    TSDL_sin = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_sinf = function(x: SDL_Float): SDL_Double cdecl;
    TSDL_sqrt = function(x: SDL_Double): SDL_Double cdecl;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    TSDL_sqrtf = function(x: SDL_Float): SDL_Float cdecl;
    TSDL_tan = function(x: SDL_Double): SDL_Double cdecl;
    TSDL_tanf = function(x: SDL_Float): SDL_Float cdecl;
    {$ENDIF}
    TSDL_sscanf = function(const text: SDL_String; const fmt: SDL_String):SDL_SInt32 cdecl; varargs;
    TSDL_snprintf = function(text: SDL_String; maxlen: SDL_Size_t; const fmt: SDL_String):SDL_SInt32 cdecl; varargs;
  public
    SDL_malloc: TSDL_malloc;
    SDL_calloc: TSDL_calloc;
    SDL_realloc: TSDL_realloc;
    SDL_free: TSDL_free;
    SDL_setenv: TSDL_setenv;
    SDL_getenv: TSDL_getenv;
    SDL_qsort: TSDL_qsort;
    SDL_abs: TSDL_abs;
    SDL_isdigit: TSDL_isdigit;
    SDL_isspace: TSDL_isspace;
    SDL_toupper: TSDL_toupper;
    SDL_tolower: TSDL_tolower;
    SDL_memset: TSDL_memset;
    SDL_memcpy: TSDL_memcpy;
    SDL_memmove: TSDL_memmove;
    SDL_memcmp: TSDL_memcmp;
    SDL_wcslen: TSDL_wcslen;
    SDL_wcslcpy: TSDL_wcslcpy;
    SDL_wcslcat: TSDL_wcslcat;
    SDL_strlen: TSDL_strlen;
    SDL_strlcpy: TSDL_strlcpy;
    SDL_utf8strlcpy: TSDL_utf8strlcpy;
    SDL_strlcat: TSDL_strlcat;
    SDL_strdup: TSDL_strdup;
    SDL_strrev: TSDL_strrev;
    SDL_strupr: TSDL_strupr;
    SDL_strlwr: TSDL_strlwr;
    SDL_strchr: TSDL_strchr;
    SDL_strrchr: TSDL_strrchr;
    SDL_strstr: TSDL_strstr;
    SDL_itoa: TSDL_itoa;
    SDL_uitoa: TSDL_uitoa;
    SDL_ltoa: TSDL_ltoa;
    SDL_ultoa: TSDL_ultoa;
    SDL_lltoa: TSDL_lltoa;
    SDL_ulltoa: TSDL_ulltoa;
    SDL_atoi: TSDL_atoi;
    SDL_atof: TSDL_atof;
    SDL_strtol: TSDL_strtol;
    SDL_strtoul: TSDL_strtoul;
    SDL_strtoll: TSDL_strtoll;
    SDL_strtoull: TSDL_strtoull;
    SDL_strtod: TSDL_strtod;
    SDL_strcmp: TSDL_strcmp;
    SDL_strncmp: TSDL_strncmp;
    SDL_strcasecmp: TSDL_strcasecmp;
    SDL_strncasecmp: TSDL_strncasecmp;
    SDL_acos: TSDL_acos;
    SDL_asin: TSDL_asin;
    SDL_atan: TSDL_atan;
    SDL_atan2: TSDL_atan2;
    SDL_ceil: TSDL_ceil;
    SDL_copysign: TSDL_copysign;
    SDL_cos: TSDL_cos;
    SDL_cosf: TSDL_cosf;
    SDL_fabs: TSDL_fabs;
    SDL_floor: TSDL_floor;
    SDL_log: TSDL_log;
    SDL_pow: TSDL_pow;
    SDL_scalbn: TSDL_scalbn;
    SDL_sin: TSDL_sin;
    SDL_sinf: TSDL_sinf;
    SDL_sqrt: TSDL_sqrt;
    {$IFNDEF DISABLE_SDL2_2_0_4}
    SDL_sqrtf: TSDL_sqrtf;
    SDL_tan: TSDL_tan;
    SDL_tanf: TSDL_tanf;
    {$ENDIF}
    SDL_sscanf: TSDL_sscanf;
    SDL_snprintf: TSDL_snprintf;
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (** /brief Helper method to convert char to SD_WideChar in platform independant way *)
    function SDL_CharToWideChar(const Ch: Char): SDL_WideChar;
  end;

implementation

{ TSDLStrIncApi }

procedure TSDLStrIncApi.DoInit;
begin

end;

procedure TSDLStrIncApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_malloc', @SDL_malloc);
  List.Add('SDL_calloc', @SDL_calloc);
  List.Add('SDL_realloc', @SDL_realloc);
  List.Add('SDL_free', @SDL_free);
  List.Add('SDL_setenv', @SDL_setenv);
  List.Add('SDL_getenv', @SDL_getenv);
  List.Add('SDL_qsort', @SDL_qsort);
  List.Add('SDL_abs', @SDL_abs);
  List.Add('SDL_isdigit', @SDL_isdigit);
  List.Add('SDL_isspace', @SDL_isspace);
  List.Add('SDL_toupper', @SDL_toupper);
  List.Add('SDL_tolower', @SDL_tolower);
  List.Add('SDL_memset', @SDL_memset);
  List.Add('SDL_memcpy', @SDL_memcpy);
  List.Add('SDL_memmove', @SDL_memmove);
  List.Add('SDL_memcmp', @SDL_memcmp);
  List.Add('SDL_wcslen', @SDL_wcslen);
  List.Add('SDL_wcslcpy', @SDL_wcslcpy);
  List.Add('SDL_wcslcat', @SDL_wcslcat);
  List.Add('SDL_strlen', @SDL_strlen);
  List.Add('SDL_strlcpy', @SDL_strlcpy);
  List.Add('SDL_utf8strlcpy', @SDL_utf8strlcpy);
  List.Add('SDL_strlcat', @SDL_strlcat);
  List.Add('SDL_strdup', @SDL_strdup);
  List.Add('SDL_strrev', @SDL_strrev);
  List.Add('SDL_strupr', @SDL_strupr);
  List.Add('SDL_strlwr', @SDL_strlwr);
  List.Add('SDL_strchr', @SDL_strchr);
  List.Add('SDL_strrchr', @SDL_strrchr);
  List.Add('SDL_strstr', @SDL_strstr);
  List.Add('SDL_itoa', @SDL_itoa);
  List.Add('SDL_uitoa', @SDL_uitoa);
  List.Add('SDL_ltoa', @SDL_ltoa);
  List.Add('SDL_ultoa', @SDL_ultoa);
  List.Add('SDL_lltoa', @SDL_lltoa);
  List.Add('SDL_ulltoa', @SDL_ulltoa);
  List.Add('SDL_atoi', @SDL_atoi);
  List.Add('SDL_atof', @SDL_atof);
  List.Add('SDL_strtol', @SDL_strtol);
  List.Add('SDL_strtoul', @SDL_strtoul);
  List.Add('SDL_strtoll', @SDL_strtoll);
  List.Add('SDL_strtoull', @SDL_strtoull);
  List.Add('SDL_strtod', @SDL_strtod);
  List.Add('SDL_strcmp', @SDL_strcmp);
  List.Add('SDL_strncmp', @SDL_strncmp);
  List.Add('SDL_strcasecmp', @SDL_strcasecmp);
  List.Add('SDL_strncasecmp', @SDL_strncasecmp);
  List.Add('SDL_acos', @SDL_acos);
  List.Add('SDL_asin', @SDL_asin);
  List.Add('SDL_atan', @SDL_atan);
  List.Add('SDL_atan2', @SDL_atan2);
  List.Add('SDL_ceil', @SDL_ceil);
  List.Add('SDL_copysign', @SDL_copysign);
  List.Add('SDL_cos', @SDL_cos);
  List.Add('SDL_cosf', @SDL_cosf);
  List.Add('SDL_fabs', @SDL_fabs);
  List.Add('SDL_floor', @SDL_floor);
  List.Add('SDL_log', @SDL_log);
  List.Add('SDL_pow', @SDL_pow);
  List.Add('SDL_scalbn', @SDL_scalbn);
  List.Add('SDL_sin', @SDL_sin);
  List.Add('SDL_sinf', @SDL_sinf);
  List.Add('SDL_sqrt', @SDL_sqrt);
  {$IFNDEF DISABLE_SDL2_2_0_4}
  List.Add('SDL_sqrtf', @SDL_sqrtf, False);
  List.Add('SDL_tan', @SDL_tan, False);
  List.Add('SDL_tanf', @SDL_tanf, False);
  {$ENDIF}
  List.Add('SDL_sscanf', @SDL_sscanf);
  List.Add('SDL_snprintf', @SDL_snprintf);
end;

function TSDLStrIncApi.SDL_CharToWideChar(const Ch: Char): SDL_WideChar;
begin
  {$IFDEF WINDOWS}
  Result := Ch;
  {$ELSE}
  Result := Ord(Ch);
  {$ENDIF}
end;

end.

