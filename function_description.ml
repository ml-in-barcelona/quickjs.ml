module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

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

  let lre_get_flags =
    F.foreign "lre_get_flags"
      (* const uint8_t *bc_buf *)
      (Ctypes.ptr Ctypes.uint8_t @-> F.returning Ctypes.int)
end
