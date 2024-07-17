type t = {
  bc : Unsigned.uint8 Ctypes_static.ptr;
  source : string;
  flags : int;
  mutable lastIndex : int;
}

type matchResult = {
  captures : string array;
  input : string;
  index : int;
  groups : string list; [@warning "-69"] (* groups is unused, but will be *)
}

type result = (matchResult, string) Stdlib.result

(* #define LRE_FLAG_GLOBAL (1 << 0) *)
let lre_flag_global = 0b01

(* #define LRE_FLAG_IGNORECASE (1 << 1) *)
let lre_flag_ignorecase = 0b10

(* #define LRE_FLAG_MULTILINE (1 << 2) *)
let lre_flag_multiline = 0b100

(* #define LRE_FLAG_DOTALL (1 << 3) *)
let lre_flag_dotall = 0b1000

(* #define LRE_FLAG_UNICODE (1 << 4) *)
let lre_flag_unicode = 0b10000

(* #define LRE_FLAG_STICKY (1 << 5) *)
let lre_flag_sticky = 0b100000
let has_flag flags flag = flags land flag != 0
let global regexp = has_flag regexp.flags lre_flag_global
let ignorecase regexp = has_flag regexp.flags lre_flag_ignorecase
let multiline regexp = has_flag regexp.flags lre_flag_multiline
let dotall regexp = has_flag regexp.flags lre_flag_dotall
let sticky regexp = has_flag regexp.flags lre_flag_sticky
let unicode regexp = has_flag regexp.flags lre_flag_unicode

let parse_flags flags =
  let rec parse_flags' flags acc =
    match flags with
    | [] -> acc
    | 'g' :: rest -> parse_flags' rest (acc lor lre_flag_global)
    | 'i' :: rest -> parse_flags' rest (acc lor lre_flag_ignorecase)
    | 'm' :: rest -> parse_flags' rest (acc lor lre_flag_multiline)
    | 's' :: rest -> parse_flags' rest (acc lor lre_flag_dotall)
    | 'u' :: rest -> parse_flags' rest (acc lor lre_flag_unicode)
    | 'y' :: rest -> parse_flags' rest (acc lor lre_flag_sticky)
    | _ :: rest -> parse_flags' rest acc
  in
  parse_flags' (String.to_seq flags |> List.of_seq) 0

let flags_to_string flags =
  let rec flags_to_string' flags acc =
    match flags with
    | 0 -> acc
    | _ when has_flag flags lre_flag_global ->
        flags_to_string' (flags land lre_flag_global lxor flags) (acc ^ "g")
    | _ when has_flag flags lre_flag_ignorecase ->
        flags_to_string' (flags land lre_flag_ignorecase lxor flags) (acc ^ "i")
    | _ when has_flag flags lre_flag_multiline ->
        flags_to_string' (flags land lre_flag_multiline lxor flags) (acc ^ "m")
    | _ when has_flag flags lre_flag_dotall ->
        flags_to_string' (flags land lre_flag_dotall lxor flags) (acc ^ "s")
    | _ when has_flag flags lre_flag_unicode ->
        flags_to_string' (flags land lre_flag_unicode lxor flags) (acc ^ "u")
    | _ when has_flag flags lre_flag_sticky ->
        flags_to_string' (flags land lre_flag_sticky lxor flags) (acc ^ "y")
    | _ -> acc
  in
  flags_to_string' flags ""

let strlen ptr =
  let rec aux ptr len =
    let c = Ctypes.( !@ ) ptr in
    if c = char_of_int 0 then len else aux (Ctypes.( +@ ) ptr 1) (len + 1)
  in
  aux ptr 0

let string_from_ptr ptr = Ctypes.string_from_ptr ~length:(strlen ptr) ptr

let compile ~flags re =
  let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
  let size_of_error_msg = 64 in
  let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
  let input = Ctypes.ocaml_string_start re in
  let input_length = String.length re |> Unsigned.Size_t.of_int in
  let flags = parse_flags flags in
  let compiled_byte_code =
    Bindings.C.Functions.lre_compile compiled_byte_code_len error_msg
      size_of_error_msg input input_length flags Ctypes.null
  in
  match Ctypes.is_null compiled_byte_code with
  | false -> Ok { bc = compiled_byte_code; flags; lastIndex = 0; source = re }
  | true ->
      let length = strlen error_msg in
      let error = Ctypes.string_from_ptr ~length error_msg in
      Error
        ( (match error with
          | "unexpected end" -> `Unexpected_end
          | "malformed unicode char" -> `Malformed_unicode_char
          | "nothing to repeat" -> `Nothing_to_repeat
          | "invalid escape sequence in regular expression" ->
              `Invalid_escape_sequence
          | unknown -> `Unknown unknown),
          error )

let index result = match result with Ok result -> result.index | Error _ -> 0
let lastIndex regexp = regexp.lastIndex
let source regexp = regexp.source
let input result = match result with Ok result -> result.input | Error _ -> ""
let setLastIndex regexp lastIndex = regexp.lastIndex <- lastIndex

let captures result =
  match result with Ok result -> result.captures | Error _ -> [||]

let flags regexp = flags_to_string regexp.flags

(* exec is not a binding to lre_exec but an implementation of `js_regexp_exec` *)
let exec regexp input =
  let capture_count = Bindings.C.Functions.lre_get_capture_count regexp.bc in
  let capture_size = capture_count * 2 in
  let capture = Ctypes.CArray.make (Ctypes.ptr Ctypes.uint8_t) capture_size in
  let start_capture = Ctypes.CArray.start capture in
  let matching_length = String.length input in
  let bufp =
    Ctypes.CArray.of_list Ctypes.char (input |> String.to_seq |> List.of_seq)
  in
  let buffer =
    Ctypes.coerce (Ctypes.ptr Ctypes.char)
      (Ctypes.ptr Ctypes.uint8_t)
      (Ctypes.CArray.start bufp)
  in

  let lastIndex =
    (* if ((re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0) {
           last_index = 0;
       } *)
    match global regexp || sticky regexp with
    | true -> regexp.lastIndex
    | false -> 0
  in

  (* TODO: Support `str->is_wide_char`. Possible solution: (install uutf)
       open Uchar

     let is_wide_char (c : char) : bool =
       let uchar = Uchar.of_char c in
       match uchar with
       | `Uchar uchar -> Uucp.Break.tty_width (Uucp.Break.tty_properties uchar) = 2
       | _ -> false *)
  let shift = 0 in

  (* Return 1 if match, 0 if not match or -1 if error. cindex is the
     starting position of the match and must be such as 0 <= cindex <=
     clen. *)
  let exec_result =
    Bindings.C.Functions.lre_exec start_capture regexp.bc buffer lastIndex
      matching_length shift Ctypes.null
  in

  match exec_result with
  | 1 ->
      let substrings = Array.make capture_count "" in
      let i = ref 0 in
      let index = ref 0 in
      let groups = ref [] in
      let group_name_ptr =
        ref (Bindings.C.Functions.lre_get_groupnames regexp.bc)
      in
      while !i < capture_size - 1 do
        let start_ptr = Ctypes.CArray.get capture !i in
        let end_ptr = Ctypes.CArray.get capture (!i + 1) in
        let start_index = Ctypes.ptr_diff buffer start_ptr in
        let length = Ctypes.ptr_diff start_ptr end_ptr in
        (* JS_DefinePropertyValue(ctx, obj, JS_ATOM_index, JS_NewInt32(ctx, (capture[0] - str_buf) >> shift), prop_flags) *)
        index := start_index;
        let substring =
          match String.sub input start_index length with
          | sub -> sub
          (* goto fail which foes JS_FreeValue str_val means there's a null in the result *)
          | exception _ -> ""
        in
        (* Store the captured substring *)
        substrings.(!i / 2) <- substring;
        (* Update the lastIndex *)
        regexp.lastIndex <- start_index + length;

        match !group_name_ptr with
        | None -> ()
        | Some pointer ->
            (*
              if (group_name_ptr && i > 0) {
                if (\*group_name_ptr) {
                    if (JS_DefinePropertyValueStr(ctx, groups, group_name_ptr, JS_DupValue(ctx, val), prop_flags) < 0) {
                        JS_FreeValue(ctx, val);
                        goto fail;
                    }
                }
            *)
            if !i > 0 then (
              (* store the group name *)
              let current_group_name = string_from_ptr pointer in
              groups := current_group_name :: !groups;
              (* group_name_ptr += strlen(group_name_ptr) + 1; *)
              let next_group_name_ptr =
                Ctypes.( +@ ) pointer (strlen pointer + 1)
              in
              if Ctypes.is_null next_group_name_ptr then group_name_ptr := None
              else group_name_ptr := Some next_group_name_ptr);

            (* Incement the index *)
            i := !i + 2
      done;
      Ok { captures = substrings; input; index = !index; groups = !groups }
  | 0 ->
      (* When there's no matches left, lastIndex goes back to 0 *)
      (match sticky regexp || global regexp with
      | true -> regexp.lastIndex <- 0
      | false -> ());
      Ok { captures = [||]; input; index = 0; groups = [] }
  | _ (* -1 *) -> Error "Error"

let test regexp input =
  let result = exec regexp input in
  match result with
  | Ok result -> Array.length result.captures > 0
  | Error _ -> false
