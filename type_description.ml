(** Ctypes type description functor - required by the ctypes build system.

    This module is part of the ctypes functor pattern. While the bindings to
    QuickJS don't define any custom ctypes TYPE declarations (we use standard
    types like int, char ptr, uint32_t, etc.), this file must exist for the
    ctypes build infrastructure to work correctly.

    See dune's (type_description ...) stanza and the ctypes documentation. *)

module Types (T : Ctypes.TYPE) = struct
  (* No custom type declarations needed - using standard ctypes *)
end
