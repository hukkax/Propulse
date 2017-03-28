(* SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
 *
 * Delphi header conversion by hukka 2016 - *** unfinished! ***
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at
 * your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

unit soxr;

interface

{$IFDEF WINDOWS}
{$DEFINE ENABLE_SOXR}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

const
	{$IFDEF WINDOWS}
	SOXR_DLL = 'libsoxr.dll';
	{$ELSE}
	SOXR_DLL = 'libsoxr.so';
	{$ENDIF}

	SOXR_TPDF             = 0;	// Applicable only if otype is INT16.
	SOXR_NO_DITHER        = 8;	// Disable the above.

	{ soxr_quality_spec }
	SOXR_ROLLOFF_SMALL    = 0;	// <= 0.01 dB
	SOXR_ROLLOFF_MEDIUM   = 1;	// <= 0.35 dB
	SOXR_ROLLOFF_NONE     = 2;	// For Chebyshev bandwidth.

	SOXR_MAINTAIN_3DB_PT  = 4;	// Reserved for internal use.
	SOXR_HI_PREC_CLOCK    = 8;	// Increase 'irrational' ratio accuracy.
	SOXR_DOUBLE_PRECISION = 16;	// Use D.P. calcs even if precision <= 20.
	SOXR_VR               = 32;	// Variable-rate resampling.

	{ soxr_runtime_spec }		// For 'irrational' ratios only:
	SOXR_COEF_INTERP_AUTO = 0; 	// Auto select coef. interpolation.
	SOXR_COEF_INTERP_LOW  = 2; 	// Man. select: less CPU, more memory.
	SOXR_COEF_INTERP_HIGH = 3; 	// Man. select: more CPU, less memory.

	SOXR_STRICT_BUFFERING = 4; 	// Reserved for future use.
	SOXR_NOSMALLINTOPT    = 8; 	// For test purposes only.

	{ soxr_quality_spec }
	SOXR_16_BITQ          = 3;
	SOXR_20_BITQ          = 4;
	SOXR_24_BITQ          = 5;
	SOXR_28_BITQ          = 6;
	SOXR_32_BITQ          = 7;
											// The 5 standard qualities found in SoX:
	SOXR_QQ               = 0;				// 'Quick' cubic interpolation.
	SOXR_LQ               = 1;				// 'Low' 16-bit with larger rolloff.
	SOXR_MQ               = 2;				// 'Medium' 16-bit with medium rolloff.
	SOXR_HQ               = SOXR_20_BITQ;	// 'High quality'.
	SOXR_VHQ              = SOXR_28_BITQ;	// 'Very high quality'.
											// Libsamplerate equivalent qualities:
	SOXR_LSR0Q            = 8; 				// 'Best sinc'.
	SOXR_LSR1Q            = 9;				// 'Medium sinc'.
	SOXR_LSR2Q            = 10;				// 'Fast sinc'.

	SOXR_LINEAR_PHASE       = $00;
	SOXR_INTERMEDIATE_PHASE = $10;
	SOXR_MINIMUM_PHASE      = $30;
	SOXR_STEEP_FILTER       = $40;
	SOXR_ALLOW_ALIASING     = $80; 			// Reserved for future use.

const
	SOXRQuality: array[0..4] of Integer =
		(SOXR_QQ, SOXR_LQ, SOXR_MQ, SOXR_HQ, SOXR_VHQ);

(* ---------------------------- API conventions --------------------------------

Buffer lengths (and occupancies) are expressed as the number of contained
samples per channel.

Parameter names for buffer lengths have the suffix `len'.

A single-character `i' or 'o' is often used in names to give context as
input or output (e.g. ilen, olen).                                            *)

(* --------------------------- Type declarations ---------------------------- *)

type
	size_t = Cardinal;
	Psize_t = ^size_t;

	_soxr = record end;					// A resampler for 1 or more channels.
	soxr_t = ^_soxr;

(* -------------------------- API type definitions -------------------------- *)

	(* Datatypes supported for I/O to/from the resampler: *)
	soxr_datatype_t = (
		(* Internal; do not use: *)
		SOXR_FLOAT32, SOXR_FLOAT64, SOXR_INT32, SOXR_INT16, SOXR_SPLIT = 4,

		(* Use for interleaved channels: *)
		SOXR_FLOAT32_I = SOXR_FLOAT32, SOXR_FLOAT64_I, SOXR_INT32_I, SOXR_INT16_I,

		(* Use for split channels: *)
		SOXR_FLOAT32_S = SOXR_SPLIT, SOXR_FLOAT64_S, SOXR_INT32_S, SOXR_INT16_S
	);

	//#define soxr_datatype_size(x)  (* Returns `sizeof' a soxr_datatype_t sample. *)\
	//  ((unsigned char *)"\4\10\4\2")[(x)&3]

	soxr_io_spec_t = record                                         (* Typically *)
	  itype: soxr_datatype_t;	(* Input datatype.                SOXR_FLOAT32_I *)
	  otype: soxr_datatype_t;	(* Output datatype.               SOXR_FLOAT32_I *)
	  scale: Double;			(* Linear gain to apply during resampling.  1    *)
	  e: Pointer;				(* Reserved for internal use                0    *)
	  flags: Cardinal;			(* Per the following #defines.              0    *)
	end;
	Psoxr_io_spec = ^soxr_io_spec_t;

	soxr_quality_spec_t = record                                     (* Typically *)
		precision: double;		(* Conversion precision (in bits).           20   *)
		phase_response: double;	(* 0=minimum, ... 50=linear, ... 100=maximum 50   *)
		passband_end: double;	(* 0dB pt. bandwidth to preserve; nyquist=1  0.913*)
		stopband_begin: double;	(* Aliasing/imaging control; > passband_end   1   *)
		e: Pointer;				(* Reserved for internal use.                 0   *)
		flags: Cardinal;		(* Per the following #defines.                0   *)
	end;
	Psoxr_quality_spec = ^soxr_quality_spec_t;

	soxr_runtime_spec_t = record                                     (* Typically *)
		log2_min_dft_size: Cardinal;	(* For DFT efficiency. [8,15]          10 *)
		log2_large_dft_size: Cardinal;	(* For DFT efficiency. [16,20]         17 *)
		coef_size_kbytes: Cardinal;		(* For SOXR_COEF_INTERP_AUTO (below). 400 *)
		num_threads: Cardinal;			(* If built so. 0 means `automatic'.    1 *)
		e: Pointer;						(* Reserved for internal use.           0 *)
		flags: Cardinal;				(* Per the following #defines.          0 *)
	end;
	Psoxr_runtime_spec = ^soxr_runtime_spec_t;

	soxr_error_t = PAnsiChar;			// 0:no-error; non-0:error.

	soxr_buf_t  = Pointer; 				// 1 buffer of channel-interleaved samples.
	soxr_cbuf_t = Pointer; 				// Ditto; read-only.

	soxr_bufs_t = ^soxr_buf_t; 			// Or, a separate buffer for each ch.
	soxr_cbufs_t = ^soxr_cbuf_t;		// Ditto; read-only.

	soxr_in_t = Pointer; 				// Either a soxr_cbuf_t or soxr_cbufs_t,
										// depending on itype in soxr_io_spec_t.
	soxr_out_t = Pointer; 				// Either a soxr_buf_t or soxr_bufs_t,
										// depending on otype in soxr_io_spec_t.

(* --------------------------- API main functions --------------------------- *)

	{$IFDEF ENABLE_SOXR}

	//#define soxr_strerror(e)               (* Soxr counterpart to strerror. *)
	//    ((e)?(e):"no error")

    (* Query library version: "libsoxr-x.y.z" *)
	function soxr_version(): PAnsiChar
		cdecl; external SOXR_DLL;

	(* Create a stream resampler *)
	function soxr_create(
		input_rate:   Double;				// Input sample-rate.
		output_rate:  Double;				// Output sample-rate.
		num_channels: Cardinal;				// Number of channels to be used.
		(* All following arguments are optional (may be set to NULL). *)
		err:     Pointer; //soxr_error_t;			// To report any error during creation.
		iospec:  Pointer; //Psoxr_io_spec;			// To specify non-default I/O formats.
		quality: Pointer; //Psoxr_quality_spec;		// To specify non-default resampling quality.
		runtime: Pointer  //Psoxr_runtime_spec		// To specify non-default runtime resources.
	): soxr_t
	cdecl; external SOXR_DLL;

	(*	Default io_spec      is per soxr_io_spec(SOXR_FLOAT32_I, SOXR_FLOAT32_I)
		Default quality_spec is per soxr_quality_spec(SOXR_HQ, 0)
		Default runtime_spec is per soxr_runtime_spec(1)						*)

	(* If not using an app-supplied input function, after creating a stream
	 * resampler, repeatedly call:
	 *)
	function soxr_process(
		resampler: soxr_t;		// As returned by soxr_create.
		// Input (to be resampled):
		_in: soxr_in_t;			// Input buffer(s); may be NULL (see below).
		ilen: size_t;			// Input buf. length (samples per channel).
		idone: Psize_t;			// To return actual # samples used (<= ilen).
		// Output (resampled):
		_out: soxr_out_t;		// Output buffer(s).
		olen: size_t;			// Output buf. length (samples per channel).
		odone: Psize_t			// To return actual # samples out (<= olen).
	): soxr_error_t
	cdecl; external SOXR_DLL;

	(*	Note that no special meaning is associated with ilen or olen equal to
		zero.  End-of-input (i.e. no data is available nor shall be available)
		may be indicated by setting 'in' to NULL. 								*)

	(* If using an app-supplied input function, it must look and behave like this:*)

{size_t (* data_len *)
  (* soxr_input_fn_t)(         (* Supply data to be resampled. *)
	void * input_fn_state,     (* As given to soxr_set_input_fn (below). *)
	soxr_in_t * data,          (* Returned data; see below. N.B. ptr to ptr(s)*)
	size_t requested_len);     (* Samples per channel, >= returned data_len.

  data_len  *data     Indicates    Meaning
   ------- -------   ------------  -------------------------
	 !=0     !=0       Success     *data contains data to be
								   input to the resampler.
	  0    !=0 (or   End-of-input  No data is available nor
		   not set)                shall be available.
	  0       0        Failure     An error occurred whilst trying to
								   source data to be input to the resampler.  *)}

(* and be registered with a previously created stream resampler using: *)

{	function soxr_set_input_fn(	(* Set (or reset) an input function.*)
	soxr_t resampler,            (* As returned by soxr_create. *)
	soxr_input_fn_t,             (* Function to supply data to be resampled.*)
	input_fn_state: Pointer;       (* If needed by the input function. *)
	max_ilen: size_t            (* Maximum value for input fn. requested_len.*)
	): soxr_error_t;}

(* then repeatedly call: *)

{size_t (*odone*) soxr_output((* Resample and output a block of data.*)
	soxr_t resampler,            (* As returned by soxr_create. *)
	soxr_out_t data,             (* App-supplied buffer(s) for resampled data.*)
	size_t olen);                (* Amount of data to output; >= odone. *)}

	(* Common stream resampler operations: *)

	function  soxr_error(err: soxr_t): soxr_error_t		// Query error status.
		cdecl; external SOXR_DLL;

	function  soxr_num_clips(clipctr: soxr_t): Psize_t	// Query int. clip counter (for R/W).
		cdecl; external SOXR_DLL;

	function  soxr_delay(delay: soxr_t): Double			// Query current delay in output samples.
		cdecl; external SOXR_DLL;

	function  soxr_engine(p: soxr_t): PAnsiChar			// Query resampling engine name.
		cdecl; external SOXR_DLL;

	function  soxr_clear(v: soxr_t): soxr_error_t		// Ready for fresh signal, same config.
		cdecl; external SOXR_DLL;

	procedure soxr_delete(v: soxr_t)					// Free resources.
		cdecl; external SOXR_DLL;

	(* 'Short-cut', single call to resample a (probably short) signal held entirely
	 * in memory.  See soxr_create and soxr_process above for parameter details.
	 * Note that unlike soxr_create however, the default quality spec. for
	 * soxr_oneshot is per soxr_quality_spec(SOXR_LQ, 0).
	 *)
	function soxr_oneshot(
		input_rate: double;
		output_rate: double;
		num_channels: Cardinal;
		input: soxr_in_t;
		ilen: size_t;
		idone: Psize_t;
		output: soxr_out_t;
		olen: size_t;
		odone: Psize_t;
		iospec:  Psoxr_io_spec;
		quality: Psoxr_quality_spec;
		runtime: Psoxr_runtime_spec
	): soxr_error_t
	cdecl; external SOXR_DLL;

	(* For variable-rate resampling. See example # 5 for how to create a
	 * variable-rate resampler and how to use this function. *)

	function soxr_set_io_ratio(
		n: soxr_t;
		io_ratio: double;
		slew_len: size_t
	): soxr_error_t
	cdecl; external SOXR_DLL;

(* -------------------------- API type constructors ------------------------- *)

	(* These functions allow setting of the most commonly-used structure
	 * parameters, with other parameters being given default values.  The default
	 * values may then be overridden, directly in the structure, if needed.  *)

	function soxr_quality_spec(
		recipe: Cardinal;	(* Per the #defines immediately below. *)
		flags:  Cardinal	(* As soxr_quality_spec_t.flags. *)
	): soxr_quality_spec_t
	cdecl; external SOXR_DLL;

	function soxr_runtime_spec(num_threads: Cardinal): soxr_runtime_spec_t
		cdecl; external SOXR_DLL;

	function soxr_io_spec(itype, otype: soxr_datatype_t): soxr_io_spec_t
		cdecl; external SOXR_DLL;

(* --------------------------- Advanced use only ---------------------------- *)

	(* For new designs, the following functions/usage will probably not be needed.
	 * They might be useful when adding soxr into an existing design where values
	 * for the resampling-rate and/or number-of-channels parameters to soxr_create
	 * are not available when that function will be called.  In such cases, the
	 * relevant soxr_create parameter(s) can be given as 0, then one or both of the
	 * following (as appropriate) later invoked (but prior to calling soxr_process
	 * or soxr_output):
	 *
	 * soxr_set_error(soxr, soxr_set_io_ratio(soxr, io_ratio, 0));
	 * soxr_set_error(soxr, soxr_set_num_channels(soxr, num_channels));
	 *)

	function soxr_set_error(t: soxr_t; err: soxr_error_t): soxr_error_t
		cdecl; external SOXR_DLL;

	function soxr_set_num_channels(t: soxr_t; c: Cardinal): soxr_error_t
		cdecl; external SOXR_DLL;

implementation

    {$ELSE}

    // dummy functions because I couldn't figure out how to
    // use soxr lib in linux

	function soxr_version: PAnsiChar;
	function soxr_create(
		input_rate:   Double;				// Input sample-rate.
		output_rate:  Double;				// Output sample-rate.
		num_channels: Cardinal;				// Number of channels to be used.
		(* All following arguments are optional (may be set to NULL). *)
		err:     Pointer; //soxr_error_t;			// To report any error during creation.
		iospec:  Pointer; //Psoxr_io_spec;			// To specify non-default I/O formats.
		quality: Pointer; //Psoxr_quality_spec;		// To specify non-default resampling quality.
		runtime: Pointer  //Psoxr_runtime_spec		// To specify non-default runtime resources.
	): soxr_t;
	function soxr_process(
		resampler: soxr_t;		// As returned by soxr_create.
		// Input (to be resampled):
		_in: soxr_in_t;			// Input buffer(s); may be NULL (see below).
		ilen: size_t;			// Input buf. length (samples per channel).
		idone: Psize_t;			// To return actual # samples used (<= ilen).
		// Output (resampled):
		_out: soxr_out_t;		// Output buffer(s).
		olen: size_t;			// Output buf. length (samples per channel).
		odone: Psize_t			// To return actual # samples out (<= olen).
	): soxr_error_t;
	function  soxr_error(err: soxr_t): soxr_error_t;
	function  soxr_num_clips(clipctr: soxr_t): Psize_t;
	function  soxr_delay(delay: soxr_t): Double;
	function  soxr_engine(p: soxr_t): PAnsiChar;
	function  soxr_clear(v: soxr_t): soxr_error_t;
	procedure soxr_delete(v: soxr_t);
	function soxr_oneshot(
		input_rate: double;
		output_rate: double;
		num_channels: Cardinal;
		input: soxr_in_t;
		ilen: size_t;
		idone: Psize_t;
		output: soxr_out_t;
		olen: size_t;
		odone: Psize_t;
		iospec:  Psoxr_io_spec;
		quality: Psoxr_quality_spec;
		runtime: Psoxr_runtime_spec
	): soxr_error_t;
	function soxr_set_io_ratio(
		n: soxr_t;
		io_ratio: double;
		slew_len: size_t
	): soxr_error_t;
	function soxr_quality_spec(
		recipe: Cardinal;	(* Per the #defines immediately below. *)
		flags:  Cardinal	(* As soxr_quality_spec_t.flags. *)
	): soxr_quality_spec_t;
	function soxr_runtime_spec(num_threads: Cardinal): soxr_runtime_spec_t;
	function soxr_io_spec(itype, otype: soxr_datatype_t): soxr_io_spec_t;
	function soxr_set_error(t: soxr_t; err: soxr_error_t): soxr_error_t;
	function soxr_set_num_channels(t: soxr_t; c: Cardinal): soxr_error_t;

implementation

	function soxr_version: PAnsiChar;
	begin
		Result := PAnsiChar('');
	end;

	(* Create a stream resampler *)
	function soxr_create(
		input_rate:   Double;				// Input sample-rate.
		output_rate:  Double;				// Output sample-rate.
		num_channels: Cardinal;				// Number of channels to be used.
		(* All following arguments are optional (may be set to NULL). *)
		err:     Pointer; //soxr_error_t;			// To report any error during creation.
		iospec:  Pointer; //Psoxr_io_spec;			// To specify non-default I/O formats.
		quality: Pointer; //Psoxr_quality_spec;		// To specify non-default resampling quality.
		runtime: Pointer  //Psoxr_runtime_spec		// To specify non-default runtime resources.
	): soxr_t;
    begin
	end;

	function soxr_process(
		resampler: soxr_t;		// As returned by soxr_create.
		// Input (to be resampled):
		_in: soxr_in_t;			// Input buffer(s); may be NULL (see below).
		ilen: size_t;			// Input buf. length (samples per channel).
		idone: Psize_t;			// To return actual # samples used (<= ilen).
		// Output (resampled):
		_out: soxr_out_t;		// Output buffer(s).
		olen: size_t;			// Output buf. length (samples per channel).
		odone: Psize_t			// To return actual # samples out (<= olen).
	): soxr_error_t;
    begin
	end;

	function  soxr_error(err: soxr_t): soxr_error_t;
    begin
	end;

	function  soxr_num_clips(clipctr: soxr_t): Psize_t;
    begin
	end;

	function  soxr_delay(delay: soxr_t): Double;
    begin
	end;

	function  soxr_engine(p: soxr_t): PAnsiChar;
    begin
	end;

	function  soxr_clear(v: soxr_t): soxr_error_t;
    begin
	end;

	procedure soxr_delete(v: soxr_t);
    begin
	end;

	function soxr_oneshot(
		input_rate: double;
		output_rate: double;
		num_channels: Cardinal;
		input: soxr_in_t;
		ilen: size_t;
		idone: Psize_t;
		output: soxr_out_t;
		olen: size_t;
		odone: Psize_t;
		iospec:  Psoxr_io_spec;
		quality: Psoxr_quality_spec;
		runtime: Psoxr_runtime_spec
	): soxr_error_t;
    begin
	end;

	function soxr_set_io_ratio(
		n: soxr_t;
		io_ratio: double;
		slew_len: size_t
	): soxr_error_t;
    begin
	end;

	function soxr_quality_spec(
		recipe: Cardinal;	(* Per the #defines immediately below. *)
		flags:  Cardinal	(* As soxr_quality_spec_t.flags. *)
	): soxr_quality_spec_t;
    begin
	end;

	function soxr_runtime_spec(num_threads: Cardinal): soxr_runtime_spec_t;
    begin
	end;

	function soxr_io_spec(itype, otype: soxr_datatype_t): soxr_io_spec_t;
    begin
	end;

	function soxr_set_error(t: soxr_t; err: soxr_error_t): soxr_error_t;
    begin
	end;

	function soxr_set_num_channels(t: soxr_t; c: Cardinal): soxr_error_t;
    begin
	end;

    {$ENDIF}

end.

