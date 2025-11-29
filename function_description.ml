module Functions (F : Ctypes.FOREIGN) = struct
  let ( @-> ) = F.( @-> )

  (* RegExp *)

  let lre_compile =
    F.foreign "lre_compile"
      (* int *plen *)
      (Ctypes.ptr Ctypes.int
     (* char *error_msg *)
     @-> Ctypes.ptr Ctypes.char
      (* int error_msg_size *)
      @-> Ctypes.int
      (* const char *buf *)
      @-> Ctypes.ocaml_string
      (* size_t buf_len *)
      @-> Ctypes.size_t
      (* int re_flags *)
      @-> Ctypes.int
      (* void *opaque *)
      @-> Ctypes.ptr Ctypes.void
      @-> F.returning (Ctypes.ptr Ctypes.uint8_t))

  let lre_exec =
    F.foreign "lre_exec"
      (* uint8_t **capture *)
      (Ctypes.ptr (Ctypes.ptr Ctypes.uint8_t)
      (* const uint8_t *bc_buf *)
      @-> Ctypes.ptr Ctypes.uint8_t
      (* const uint8_t *cbuf *)
      @-> Ctypes.ptr Ctypes.uint8_t
      (* int cindex *)
      @-> Ctypes.int
      (* int clen *)
      @-> Ctypes.int
      (* int cbuf_type *)
      @-> Ctypes.int
      (* void *opaque *)
      @-> Ctypes.ptr Ctypes.void
      @-> F.returning Ctypes.int)

  let lre_get_capture_count =
    F.foreign "lre_get_capture_count"
      (* const uint8_t *bc_buf *)
      (Ctypes.ptr Ctypes.uint8_t @-> F.returning Ctypes.int)

  let lre_get_groupnames =
    F.foreign "lre_get_groupnames_shim"
      (* const uint8_t *bc_buf *)
      (Ctypes.ptr Ctypes.uint8_t @-> F.returning (Ctypes.ptr_opt Ctypes.char))

  let lre_get_flags =
    F.foreign "lre_get_flags"
      (* const uint8_t *bc_buf *)
      (Ctypes.ptr Ctypes.uint8_t @-> F.returning Ctypes.int)

  (* Unicode - Character Classification *)

  let lre_is_cased =
    F.foreign "lre_is_cased"
      (* uint32_t c *)
      (Ctypes.uint32_t @-> F.returning Ctypes.int)

  let lre_is_case_ignorable =
    F.foreign "lre_is_case_ignorable"
      (* uint32_t c *)
      (Ctypes.uint32_t @-> F.returning Ctypes.int)

  let lre_is_id_start =
    F.foreign "lre_is_id_start"
      (* uint32_t c *)
      (Ctypes.uint32_t @-> F.returning Ctypes.int)

  let lre_is_id_continue =
    F.foreign "lre_is_id_continue"
      (* uint32_t c *)
      (Ctypes.uint32_t @-> F.returning Ctypes.int)

  let lre_is_space_non_ascii =
    F.foreign "lre_is_space_non_ascii"
      (* uint32_t c *)
      (Ctypes.uint32_t @-> F.returning Ctypes.int)

  (* Unicode - Case Conversion *)

  let lre_case_conv =
    F.foreign "lre_case_conv"
      (* uint32_t *res *)
      (Ctypes.ptr Ctypes.uint32_t
     (* uint32_t c *)
     @-> Ctypes.uint32_t
      (* int conv_type: 0=upper, 1=lower, 2=case_folding *)
      @-> Ctypes.int
      @-> F.returning Ctypes.int)

  let lre_canonicalize =
    F.foreign "lre_canonicalize"
      (* uint32_t c *)
      (Ctypes.uint32_t
     (* int is_unicode *)
     @-> Ctypes.int
      @-> F.returning Ctypes.int)

  (* Unicode - Normalization (via C shim) *)

  let unicode_normalize_shim =
    F.foreign "unicode_normalize_shim"
      (* const uint32_t *src *)
      (Ctypes.ptr Ctypes.uint32_t
     (* int src_len *)
     @-> Ctypes.int
      (* int n_type: 0=NFC, 1=NFD, 2=NFKC, 3=NFKD *)
      @-> Ctypes.int
      (* uint32_t **pdst - output buffer pointer *)
      @-> Ctypes.ptr (Ctypes.ptr Ctypes.uint32_t)
      (* returns length or -1 on error *)
      @-> F.returning Ctypes.int)

  let unicode_normalize_free =
    F.foreign "unicode_normalize_free"
      (* uint32_t *ptr *)
      (Ctypes.ptr Ctypes.uint32_t @-> F.returning Ctypes.void)

  (* dtoa - Double to String conversion *)

  let js_dtoa_max_len =
    F.foreign "js_dtoa_max_len"
      (* double d *)
      (Ctypes.double
     (* int radix *)
     @-> Ctypes.int
      (* int n_digits *)
      @-> Ctypes.int
      (* int flags *)
      @-> Ctypes.int
      @-> F.returning Ctypes.int)

  let js_dtoa =
    F.foreign "js_dtoa"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* double d *)
     @-> Ctypes.double
      (* int radix *)
      @-> Ctypes.int
      (* int n_digits *)
      @-> Ctypes.int
      (* int flags *)
      @-> Ctypes.int
      (* JSDTOATempMem *tmp_mem *)
      @-> Ctypes.ptr Ctypes.void
      @-> F.returning Ctypes.int)

  (* atod - String to Double conversion *)

  let js_atod =
    F.foreign "js_atod_shim"
      (* const char *str *)
      (Ctypes.string
      (* char **pnext *)
      @-> Ctypes.ptr (Ctypes.ptr Ctypes.char)
      (* int radix *)
      @-> Ctypes.int
      (* int flags *)
      @-> Ctypes.int
      (* JSATODTempMem *tmp_mem *)
      @-> Ctypes.ptr Ctypes.void
      @-> F.returning Ctypes.double)

  (* Integer to String conversion *)

  let u32toa =
    F.foreign "u32toa"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* uint32_t n *)
     @-> Ctypes.uint32_t
      @-> F.returning Ctypes.size_t)

  let i32toa =
    F.foreign "i32toa"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* int32_t n *)
     @-> Ctypes.int32_t
      @-> F.returning Ctypes.size_t)

  let u64toa =
    F.foreign "u64toa"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* uint64_t n *)
     @-> Ctypes.uint64_t
      @-> F.returning Ctypes.size_t)

  let i64toa =
    F.foreign "i64toa"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* int64_t n *)
     @-> Ctypes.int64_t
      @-> F.returning Ctypes.size_t)

  let u64toa_radix =
    F.foreign "u64toa_radix"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* uint64_t n *)
     @-> Ctypes.uint64_t
      (* unsigned int radix *)
      @-> Ctypes.uint
      @-> F.returning Ctypes.size_t)

  let i64toa_radix =
    F.foreign "i64toa_radix"
      (* char *buf *)
      (Ctypes.ptr Ctypes.char
     (* int64_t n *)
     @-> Ctypes.int64_t
      (* int radix *)
      @-> Ctypes.int
      @-> F.returning Ctypes.size_t)
end
