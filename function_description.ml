(** C function bindings for QuickJS utilities

    This module defines the FFI bindings to QuickJS's internal libraries:
    - libregexp: Regular expression engine
    - libunicode: Unicode character utilities
    - dtoa: Number ↔ String conversion (js_dtoa, js_atod)
    - cutils: Integer to string conversion (itoa family)

    Some functions are bound through wrappers in [shims.c], either because
    ctypes cannot express the exact C signature (const qualifiers), or to add
    behavior the raw library delegates to its embedder (stack-overflow guard,
    execution deadline, allocation). *)

[@@@ocamlformat "disable"] (* We want to keep the comments aligned with the C code *)

module Functions (F : Ctypes.FOREIGN) = struct

  let ( @-> ) = F.( @-> )

  (* =========================================================================
     libregexp.c - Regular Expression Engine

     QuickJS's ES2023-compliant regex engine with Unicode support.
     ========================================================================= *)

  (** Compile a regular expression pattern into bytecode (via shim that arms
      the C-stack guard). The returned buffer must be released with
      [lre_bytecode_free]. Returns NULL on error with message in error_msg. *)
  let lre_compile =
    F.foreign "lre_compile_shim"
      (Ctypes.ptr Ctypes.int          (* [out] int *plen - bytecode length *)
      @-> Ctypes.ptr Ctypes.char      (* [out] char *error_msg *)
      @-> Ctypes.int                  (* int error_msg_size *)
      @-> Ctypes.ocaml_string         (* const char *buf - pattern *)
      @-> Ctypes.size_t               (* size_t buf_len *)
      @-> Ctypes.int                  (* int re_flags *)
      @-> Ctypes.ptr Ctypes.void      (* void *opaque *)
      @-> F.returning (Ctypes.ptr Ctypes.uint8_t))

  (** Execute a compiled regex against input string (via shim that arms the
      C-stack guard and threads an optional deadline).
      Returns: 1 = match, 0 = no match,
      -1 = LRE_RET_MEMORY_ERROR, -2 = LRE_RET_TIMEOUT *)
  let lre_exec =
    F.foreign "lre_exec_shim"
      (Ctypes.ptr (Ctypes.ptr Ctypes.uint8_t)  (* [out] uint8_t **capture *)
      @-> Ctypes.ptr Ctypes.uint8_t   (* const uint8_t *bc_buf - bytecode *)
      @-> Ctypes.ptr Ctypes.uint8_t   (* const uint8_t *cbuf - input *)
      @-> Ctypes.int                  (* int cindex - start index *)
      @-> Ctypes.int                  (* int clen - input length *)
      @-> Ctypes.int                  (* int cbuf_type: 0=8bit, 1=16bit *)
      @-> Ctypes.ptr_opt Ctypes.double (* double *deadline (NULL = none) *)
      @-> F.returning Ctypes.int)

  (** Free a bytecode buffer returned by [lre_compile] *)
  let lre_bytecode_free =
    F.foreign "lre_bytecode_free"
      (Ctypes.ptr Ctypes.uint8_t      (* uint8_t *bc_buf *)
      @-> F.returning Ctypes.void)

  (** Monotonic clock in milliseconds; basis for [lre_exec] deadlines *)
  let lre_now_ms =
    F.foreign "lre_now_ms" (Ctypes.void @-> F.returning Ctypes.double)

  (** Get number of capture groups (including group 0 for full match) *)
  let lre_get_capture_count =
    F.foreign "lre_get_capture_count"
      (Ctypes.ptr Ctypes.uint8_t      (* const uint8_t *bc_buf *)
      @-> F.returning Ctypes.int)

  (** Get pointer to null-terminated group names (via shim) *)
  let lre_get_groupnames =
    F.foreign "lre_get_groupnames_shim"
      (Ctypes.ptr Ctypes.uint8_t      (* const uint8_t *bc_buf *)
      @-> F.returning (Ctypes.ptr_opt Ctypes.char))

  (** Get flags from compiled bytecode *)
  let lre_get_flags =
    F.foreign "lre_get_flags"
      (Ctypes.ptr Ctypes.uint8_t      (* const uint8_t *bc_buf *)
      @-> F.returning Ctypes.int)

  (* =========================================================================
     libunicode.c - Unicode Character Utilities

     Unicode character classification and case conversion.
     ========================================================================= *)

  (* --- Character Classification --- *)

  (** Check if character has uppercase/lowercase variants (Cased property) *)
  let lre_is_cased =
    F.foreign "lre_is_cased"
      (Ctypes.uint32_t @-> F.returning Ctypes.bool)

  (** Check if character is ignored during case mapping (Case_Ignorable) *)
  let lre_is_case_ignorable =
    F.foreign "lre_is_case_ignorable"
      (Ctypes.uint32_t @-> F.returning Ctypes.bool)

  (** Check if character can start an identifier (ID_Start) *)
  let lre_is_id_start =
    F.foreign "lre_is_id_start"
      (Ctypes.uint32_t @-> F.returning Ctypes.bool)

  (** Check if character can continue an identifier (ID_Continue) *)
  let lre_is_id_continue =
    F.foreign "lre_is_id_continue"
      (Ctypes.uint32_t @-> F.returning Ctypes.bool)

  (** Check if character is whitespace per ECMA-262 (WhiteSpace or
      LineTerminator; matches regexp \s, String.prototype.trim and the
      whitespace skipped by parseInt/parseFloat) *)
  let lre_is_space =
    F.foreign "lre_is_space"
      (Ctypes.int @-> F.returning Ctypes.bool)

  (* --- Case Conversion --- *)

  (** Convert character case.
      conv_type: 0 = uppercase, 1 = lowercase, 2 = case folding
      Returns number of output codepoints (1-3) *)
  let lre_case_conv =
    F.foreign "lre_case_conv"
      (Ctypes.ptr Ctypes.uint32_t     (* [out] uint32_t *res - output buffer *)
      @-> Ctypes.uint32_t             (* uint32_t c - input codepoint *)
      @-> Ctypes.int                  (* int conv_type *)
      @-> F.returning Ctypes.int)

  (** Canonicalize character for case-insensitive regex matching.
      is_unicode: true = full Unicode folding, false = ASCII only *)
  let lre_canonicalize =
    F.foreign "lre_canonicalize"
      (Ctypes.uint32_t                (* uint32_t c *)
      @-> Ctypes.bool                 (* bool is_unicode *)
      @-> F.returning Ctypes.int)

  (* --- Normalization --- *)

  (** Normalize Unicode string (via C shim that handles allocation).
      n_type: 0 = NFC, 1 = NFD, 2 = NFKC, 3 = NFKD
      Returns length of output, or -1 on error *)
  let unicode_normalize_shim =
    F.foreign "unicode_normalize_shim"
      (Ctypes.ptr Ctypes.uint32_t     (* const uint32_t *src *)
      @-> Ctypes.int                  (* int src_len *)
      @-> Ctypes.int                  (* int n_type *)
      @-> Ctypes.ptr (Ctypes.ptr Ctypes.uint32_t)  (* [out] uint32_t **pdst *)
      @-> F.returning Ctypes.int)

  (** Free buffer allocated by unicode_normalize_shim *)
  let unicode_normalize_free =
    F.foreign "unicode_normalize_free"
      (Ctypes.ptr Ctypes.uint32_t @-> F.returning Ctypes.void)

  (* =========================================================================
     dtoa.c - Number ↔ String Conversion

     JavaScript-compatible floating point parsing and formatting.
     dtoa = Double TO Ascii, atod = Ascii TO Double
     ========================================================================= *)

  (* --- dtoa: Double → String --- *)

  (** Calculate maximum buffer size needed for js_dtoa *)
  let js_dtoa_max_len =
    F.foreign "js_dtoa_max_len"
      (Ctypes.double                  (* double d *)
      @-> Ctypes.int                  (* int radix *)
      @-> Ctypes.int                  (* int n_digits *)
      @-> Ctypes.int                  (* int flags *)
      @-> F.returning Ctypes.int)

  (** Convert double to string with JS semantics.
      Flags: JS_DTOA_FORMAT_* | JS_DTOA_EXP_* | JS_DTOA_MINUS_ZERO
      Returns actual string length *)
  let js_dtoa =
    F.foreign "js_dtoa"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.double               (* double d *)
      @-> Ctypes.int                  (* int radix (2-36) *)
      @-> Ctypes.int                  (* int n_digits *)
      @-> Ctypes.int                  (* int flags *)
      @-> Ctypes.ptr Ctypes.void      (* JSDTOATempMem *tmp_mem *)
      @-> F.returning Ctypes.int)

  (* --- atod: String → Double --- *)

  (** Parse string to double with JS semantics (via shim).
      Flags: JS_ATOD_INT_ONLY | JS_ATOD_ACCEPT_BIN_OCT | etc.
      Sets *poffset to the number of bytes consumed (0 when parsing failed).
      Returns NaN when nothing could be parsed. *)
  let js_atod =
    F.foreign "js_atod_shim"
      (Ctypes.string                  (* const char *str *)
      @-> Ctypes.ptr Ctypes.int       (* [out] int *poffset *)
      @-> Ctypes.int                  (* int radix (0=auto, 2-36) *)
      @-> Ctypes.int                  (* int flags *)
      @-> Ctypes.ptr Ctypes.void      (* JSATODTempMem *tmp_mem *)
      @-> F.returning Ctypes.double)

  (* =========================================================================
     cutils.c / dtoa.c - Integer to String Conversion

     Fast integer-to-string functions (itoa family). These do not
     NUL-terminate; they return the number of bytes written.
     ========================================================================= *)

  (** Convert unsigned 32-bit integer to decimal string *)
  let u32toa =
    F.foreign "u32toa"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.uint32_t             (* uint32_t n *)
      @-> F.returning Ctypes.size_t)

  (** Convert signed 32-bit integer to decimal string *)
  let i32toa =
    F.foreign "i32toa"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.int32_t              (* int32_t n *)
      @-> F.returning Ctypes.size_t)

  (** Convert unsigned 64-bit integer to decimal string *)
  let u64toa =
    F.foreign "u64toa"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.uint64_t             (* uint64_t n *)
      @-> F.returning Ctypes.size_t)

  (** Convert signed 64-bit integer to decimal string *)
  let i64toa =
    F.foreign "i64toa"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.int64_t              (* int64_t n *)
      @-> F.returning Ctypes.size_t)

  (** Convert unsigned 64-bit integer to string in given radix (2-36) *)
  let u64toa_radix =
    F.foreign "u64toa_radix"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.uint64_t             (* uint64_t n *)
      @-> Ctypes.uint                 (* unsigned int radix *)
      @-> F.returning Ctypes.size_t)

  (** Convert signed 64-bit integer to string in given radix (2-36) *)
  let i64toa_radix =
    F.foreign "i64toa_radix"
      (Ctypes.ptr Ctypes.char         (* [out] char *buf *)
      @-> Ctypes.int64_t              (* int64_t n *)
      @-> Ctypes.int                  (* int radix *)
      @-> F.returning Ctypes.size_t)
end
