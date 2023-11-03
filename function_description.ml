module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  (* uint8_t *lre_compile(
     int *plen,
     char *error_msg,
     int error_msg_size,
     const char *buf,
     size_t buf_len,
     int re_flags,
     void *opaque) *)
  let lre_compile =
    foreign
      "lre_compile"
      (Ctypes.ptr Ctypes.int
       @-> Ctypes.ptr Ctypes.char
       @-> Ctypes.int
       @-> Ctypes.string
       @-> Ctypes.size_t
       @-> Ctypes.int
       @-> Ctypes.ptr Ctypes.void
       @-> F.returning (Ctypes.ptr Ctypes.uint8_t))
  ;;

  (* int lre_exec(
     uint8_t **capture,
     const uint8_t *bc_buf,
     const uint8_t *cbuf,
     int cindex,
     int clen,
     int cbuf_type,
     void *opaque) *)
  let lre_exec =
    foreign
      "lre_exec"
      (Ctypes.ptr (Ctypes.ptr Ctypes.char)
       @-> Ctypes.ptr Ctypes.uint8_t
       @-> Ctypes.string
       @-> Ctypes.int
       @-> Ctypes.int
       @-> Ctypes.int
       @-> Ctypes.ptr Ctypes.void
       @-> F.returning Ctypes.int)
  ;;
end
