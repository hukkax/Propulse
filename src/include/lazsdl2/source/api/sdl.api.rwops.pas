unit SDL.Api.RWops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL.Api.Types, DLibrary;

type

  { TSDLRwopsApi }

  TSDLRwopsApi = class(TSubLibrary)
  private type
    TSDL_RWFromFile = function(const _file: SDL_String; const mode: SDL_String): PSDL_RWops; cdecl;
    TSDL_RWFromMem = function(mem: SDL_Pointer; size: SDL_SInt32): PSDL_RWops; cdecl;
    TSDL_RWFromConstMem = function(const mem: Pointer; size: SDL_SInt32): PSDL_RWops; cdecl;
    TSDL_ReadU8 = function(src: PSDL_RWops): SDL_UInt8 cdecl;
    TSDL_ReadLE16 = function(src: PSDL_RWops): SDL_UInt16 cdecl;
    TSDL_ReadBE16 = function(src: PSDL_RWops): SDL_UInt16 cdecl;
    TSDL_ReadLE32 = function(src: PSDL_RWops): SDL_UInt32 cdecl;
    TSDL_ReadBE32 = function(src: PSDL_RWops): SDL_UInt32 cdecl;
    TSDL_ReadLE64 = function(src: PSDL_RWops): SDL_UInt64 cdecl;
    TSDL_ReadBE64 = function(src: PSDL_RWops): SDL_UInt64 cdecl;
    TSDL_WriteU8 = function(dst: PSDL_RWops; value: SDL_UInt8): SDL_Size_t cdecl;
    TSDL_WriteLE16 = function(dst: PSDL_RWops; value: SDL_UInt16): SDL_Size_t cdecl;
    TSDL_WriteBE16 = function(dst: PSDL_RWops; value: SDL_UInt16): SDL_Size_t cdecl;
    TSDL_WriteLE32 = function(dst: PSDL_RWops; value: SDL_UInt32): SDL_Size_t cdecl;
    TSDL_WriteBE32 = function(dst: PSDL_RWops; value: SDL_UInt32): SDL_Size_t cdecl;
    TSDL_WriteLE64 = function(dst: PSDL_RWops; value: SDL_UInt64): SDL_Size_t cdecl;
    TSDL_WriteBE64 = function(dst: PSDL_RWops; value: SDL_UInt64): SDL_Size_t cdecl;
  public
    (**
     *  \name RWFrom functions
     *
     *  Functions to create SDL_RWops structures from various data streams.
     */
    /* @{ *)
    (**
     * /breaf Load RWops from file
     *)
    SDL_RWFromFile: TSDL_RWFromFile;
    (**
     * /breaf Load RWops from memory
     *)
    SDL_RWFromMem: TSDL_RWFromMem;
    (**
     * /breaf Load RWops from read only memory
     *)
    SDL_RWFromConstMem: TSDL_RWFromConstMem;
    (** @} *//* RWFrom functions *)
    (**
     *  \name Read endian functions
     *
     *  Read an item of the specified endianness and return in native format.
     */
    /* @{ *)
    SDL_ReadU8: TSDL_ReadU8;
    SDL_ReadLE16: TSDL_ReadLE16;
    SDL_ReadBE16: TSDL_ReadBE16;
    SDL_ReadLE32: TSDL_ReadLE32;
    SDL_ReadBE32: TSDL_ReadBE32;
    SDL_ReadLE64: TSDL_ReadLE64;
    SDL_ReadBE64: TSDL_ReadBE64;
    (* @} *//* Read endian functions *)
    (**
     *  \name Write endian functions
     *
     *  Write an item of native format to the specified endianness.
     */
    /* @{ *)
    SDL_WriteU8: TSDL_WriteU8;
    SDL_WriteLE16: TSDL_WriteLE16;
    SDL_WriteBE16: TSDL_WriteBE16;
    SDL_WriteLE32: TSDL_WriteLE32;
    SDL_WriteBE32: TSDL_WriteBE32;
    SDL_WriteLE64: TSDL_WriteLE64;
    SDL_WriteBE64: TSDL_WriteBE64;
    (* @} *//* Write endian functions *)
  protected
    procedure DoInit; override;
    procedure GetRequiredMethods(const List: TMethodList); override;
  public
    (**
     *  \name Read/write macros
     *
     *  Macros to easily read and write from an SDL_RWops structure.
     */
    /* @{ *)
    (**
     * /breaf Get RWops size
     *)
    function SDL_RWsize(ctx: PSDL_RWops): SDL_SInt64; inline;
    (**
     * /breaf Set RWops position
     *)
    function SDL_RWseek(ctx: PSDL_RWops; offset: SDL_SInt64; whence: SDL_Rwops_seek): SDL_SInt64; inline;
    (**
     * /breaf Get RWops current position
     *)
    function SDL_RWtell(ctx: PSDL_RWops): SDL_SInt64; inline;
    (**
     * /breaf Read data from RWops
     *)
    function SDL_RWread(ctx: PSDL_RWops; ptr: SDL_Pointer; size: SDL_Size_t; n: SDL_Size_t): SDL_Size_t; inline;
    (**
     * /breaf Write data to RWops
     *)
    function SDL_RWwrite(ctx: PSDL_RWops; ptr: SDL_Pointer; size: SDL_Size_t; n: SDL_Size_t): SDL_Size_t; inline;
    (**
     * /breaf Free RWops
     *)
    function SDL_RWclose(ctx: PSDL_RWops): SDL_SInt32; inline;
    (* @} *//* Read/write macros *)
  end;

implementation

{ TSDLRwopsApi }

procedure TSDLRwopsApi.DoInit;
begin

end;

procedure TSDLRwopsApi.GetRequiredMethods(const List: TMethodList);
begin
  inherited GetRequiredMethods(List);

  List.Add('SDL_RWFromFile', @SDL_RWFromFile);
  List.Add('SDL_RWFromMem', @SDL_RWFromMem);
  List.Add('SDL_RWFromConstMem', @SDL_RWFromConstMem);
  List.Add('SDL_ReadU8', @SDL_ReadU8);
  List.Add('SDL_ReadLE16', @SDL_ReadLE16);
  List.Add('SDL_ReadBE16', @SDL_ReadBE16);
  List.Add('SDL_ReadLE32', @SDL_ReadLE32);
  List.Add('SDL_ReadBE32', @SDL_ReadBE32);
  List.Add('SDL_ReadLE64', @SDL_ReadLE64);
  List.Add('SDL_ReadBE64', @SDL_ReadBE64);
  List.Add('SDL_WriteU8', @SDL_WriteU8);
  List.Add('SDL_WriteLE16', @SDL_WriteLE16);
  List.Add('SDL_WriteBE16', @SDL_WriteBE16);
  List.Add('SDL_WriteLE32', @SDL_WriteLE32);
  List.Add('SDL_WriteBE32', @SDL_WriteBE32);
  List.Add('SDL_WriteLE64', @SDL_WriteLE64);
  List.Add('SDL_WriteBE64', @SDL_WriteBE64);
end;

function TSDLRwopsApi.SDL_RWsize(ctx: PSDL_RWops): SDL_SInt64;
begin
  Result := ctx^.size(ctx)
end;

function TSDLRwopsApi.SDL_RWseek(ctx: PSDL_RWops; offset: SDL_SInt64; whence: SDL_Rwops_seek): SDL_SInt64;
begin
  Result := ctx^.seek(ctx, offset, whence)
end;

function TSDLRwopsApi.SDL_RWtell(ctx: PSDL_RWops): SDL_SInt64;
begin
  Result := ctx^.seek(ctx, 0, RW_SEEK_CUR)
end;

function TSDLRwopsApi.SDL_RWread(ctx: PSDL_RWops; ptr: SDL_Pointer; size: SDL_Size_t; n: SDL_Size_t): SDL_Size_t;
begin
  Result := ctx^.read(ctx, ptr, size, n)
end;

function TSDLRwopsApi.SDL_RWwrite(ctx: PSDL_RWops; ptr: SDL_Pointer; size: SDL_Size_t; n: SDL_Size_t): SDL_Size_t;
begin
  Result := ctx^.write(ctx, ptr, size, n)
end;

function TSDLRwopsApi.SDL_RWclose(ctx: PSDL_RWops): SDL_SInt32;
begin
  Result := ctx^.close(ctx)
end;

end.

