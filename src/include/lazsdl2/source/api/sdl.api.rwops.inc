

type

  PSDL_RWops = ^SDL_RWops;

  (** RWops Types *)
  SDL_Rwops_Type = (
    SDL_RWOPS_UNKNOWN	  = 0,	(** Unknown stream type *)
    SDL_RWOPS_WINFILE	  = 1,	(** Win32 file *)
    SDL_RWOPS_STDFILE	  = 2,	(** Stdio file *)
    SDL_RWOPS_JNIFILE	  = 3,	(** Android asset *)
    SDL_RWOPS_MEMORY      = 4,	(** Memory stream *)
    SDL_RWOPS_MEMORY_RO   = 5	(** Read-Only memory stream *)
  );

  SDL_Rwops_seek = (
    RW_SEEK_SET = 0,       (** Seek from the beginning of data *)
    RW_SEEK_CUR = 1,       (** Seek relative to current read point *)
    RW_SEEK_END = 2        (** Seek relative to the end of data *)
  );

  (**
   *  Return the size of the file in this rwops, or -1 if unknown
   *)
  TSDL_Rwops_Size = function(context: PSDL_RWops): SDL_SInt64; {$IFNDEF GPC} cdecl; {$ENDIF}

  (**
   *  Seek to offset relative to whence, one of stdio's whence values:
   *  RW_SEEK_SET, RW_SEEK_CUR, RW_SEEK_END
   *
   *  the final offset in the data stream, or -1 on error.
   *)
  TSDL_Rwops_Seek = function(context: PSDL_RWops; offset: SDL_SInt64; whence: SDL_Rwops_seek): SDL_SInt64; {$IFNDEF GPC} cdecl; {$ENDIF}

  (**
   *  Read up to maxnum objects each of size size from the data
   *  stream to the area pointed at by ptr.
   *
   *  the number of objects read, or 0 at error or end of file.
   *)

   TSDL_Rwops_Read = function(context: PSDL_RWops; ptr: Pointer; size: SDL_Size_t; maxnum: SDL_Size_t): SDL_Size_t; {$IFNDEF GPC} cdecl; {$ENDIF}

  (**
   *  Write exactly num objects each of size size from the area
   *  pointed at by ptr to data stream.
   *
   *  the number of objects written, or 0 at error or end of file.
   *)

   TSDL_Rwops_Write = function(context: PSDL_RWops; const ptr: Pointer; size: SDL_Size_t; num: SDL_Size_t): SDL_Size_t; {$IFNDEF GPC} cdecl; {$ENDIF}

  (**
   *  Close and free an allocated SDL_RWops structure.
   *
   *  0 if successful or -1 on write error when flushing data.
   *)

  TSDL_Rwops_Close =  function(context: PSDL_RWops): SDL_SInt32; {$IFNDEF GPC} cdecl; {$ENDIF}

  SDL_File = ^file;

  (**
   * This is the read/write operation structure -- very basic.
   *)
  SDL_RWops = packed record
    size: TSDL_Rwops_Size;
    seek: TSDL_Rwops_Seek;
    read: TSDL_Rwops_Read;
    write: TSDL_Rwops_Write;
    close: TSDL_Rwops_Close;
    _type: SDL_Rwops_Type;
    hidden: record
      case Integer of
        0: (stdio: record
              autoclose: SDL_Bool;
              fp: SDL_Pointer; //Pointer to C FILE structure
            end;
            );
        1: (mem: record
              base: PSDL_UInt8;
              here: PSDL_UInt8;
              stop: PSDL_UInt8;
            end;
            );
        2: (unknown: record
              data1: SDL_Data;
              data2: SDL_Data;
            end;
           );
        {$IFDEF ANDROID}
        3: (androidio: record
              fileNameRef: Pointer;
              inputStreamRef: Pointer;
              readableByteChannelRef: Pointer;
              readMethod: Pointer;
              assetFileDescriptorRef: Pointer;
              position: SDL_SInt32;
              size: SDL_SInt32;
              offset: SDL_SInt32;
              fd: SDL_SInt32;
            end;
            );
        {$ENDIF}
        {$IFDEF WINDOWS}
        3: (windowsio: record
              append: SDL_Bool;
              h: SDL_Pointer;
              buffer: record
                data: SDL_Pointer;
                size: SDL_Size_t;
                left: SDL_Size_t;
              end;
            end;
           );
        {$ENDIF}
    end;
  end;

