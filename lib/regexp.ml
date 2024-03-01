type flags = int

type regex = {
  bc : Unsigned.uint8 Ctypes_static.ptr;
  flags : flags;
  mutable lastIndex : int;
}

type result = { captures : string array }

let parse_flags flags =
  let rec parse_flags' flags acc =
    match flags with
    | [] -> acc
    | 'g' :: rest ->
        (* #define LRE_FLAG_GLOBAL (1 << 0) *)
        parse_flags' rest (acc lor 0b01)
    | 'i' :: rest ->
        (* #define LRE_FLAG_IGNORECASE (1 << 1) *)
        parse_flags' rest (acc lor 0b10)
    | 'm' :: rest ->
        (* #define LRE_FLAG_MULTILINE (1 << 2) *)
        parse_flags' rest (acc lor 0b100)
    | 's' :: rest ->
        (*    #define LRE_FLAG_DOTALL (1 << 3) *)
        parse_flags' rest (acc lor 0b100)
    | 'u' :: rest ->
        (* #define LRE_FLAG_UNICODE (1 << 4) *)
        parse_flags' rest (acc lor 0b10000)
    | 'y' :: rest ->
        (*    #define LRE_FLAG_STICKY (1 << 5) *)
        parse_flags' rest (acc lor 0b100000)
    | _ :: rest -> parse_flags' rest acc
  in
  parse_flags' (String.to_seq flags |> List.of_seq) 0

let compile re flags =
  let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
  let size_of_error_msg = 64 in
  let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
  let input = Ctypes.ocaml_string_start re in
  let input_length = String.length re |> Unsigned.Size_t.of_int in
  let flags = parse_flags flags in
  let compiled_byte_code =
    Libregexp.C.Functions.lre_compile compiled_byte_code_len error_msg
      size_of_error_msg input input_length flags Ctypes.null
  in
  match Ctypes.is_null compiled_byte_code with
  | true ->
      let error = Ctypes.string_from_ptr ~length:64 error_msg in
      print_endline error;
      raise (Invalid_argument "Compilation failed")
  | false -> { bc = compiled_byte_code; flags = 0; lastIndex = 0 }

(* exec is not a binding to lre_exec but an implementation of `js_regexp_exec` *)
let exec regexp input =
  let { bc; _ } = regexp in
  let capture_count = Libregexp.C.Functions.lre_get_capture_count bc in
  let capture_size = capture_count * 2 in
  let capture = Ctypes.CArray.make (Ctypes.ptr Ctypes.uint8_t) capture_size in
  let start_capture = Ctypes.CArray.start capture in
  let matching_length = String.length input in
  let _matching = Ctypes.ocaml_string_start input in
  let bufp =
    Ctypes.CArray.of_list Ctypes.char (input |> String.to_seq |> List.of_seq)
  in
  let buffer =
    Ctypes.coerce (Ctypes.ptr Ctypes.char)
      (Ctypes.ptr Ctypes.uint8_t)
      (Ctypes.CArray.start bufp)
  in
  (* if ((re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0) {
         last_index = 0;
     } *)
  (* Printf.printf "\nmatching_length %d\n" matching_length; *)
  let index = 0 in
  (* Return 1 if match, 0 if not match or -1 if error. cindex is the
     starting position of the match and must be such as 0 <= cindex <=
     clen. *)
  (* if (last_index > str->len) {
         ret = 2;
     } else {
         ret = lre_exec(capture, re_bytecode,
                        str_buf, last_index, str->len,
                        shift, ctx);
     } *)
  let exec_result =
    Libregexp.C.Functions.lre_exec start_capture bc buffer index matching_length
      0 Ctypes.null
  in
  (* Printf.printf "\ncapture_count %d\n" capture_count; *)
  match exec_result with
  | 1 ->
      let substrings = Array.make capture_count "" in
      (* maybe not 0 from the start, previous? *)
      let i = ref 0 in
      while !i < capture_size - 1 do
        let start_ptr = Ctypes.CArray.get capture !i in
        let end_ptr = Ctypes.CArray.get capture (!i + 1) in
        let start_index = Ctypes.ptr_diff buffer start_ptr in
        let length = Ctypes.ptr_diff start_ptr end_ptr in
        (* print_endline (Printf.sprintf "start_index: %d" start_index); *)
        (* print_endline (Printf.sprintf "len: %d" length); *)
        let substring = String.sub input start_index length in
        substrings.(!i / 2) <- substring;
        (* Update the lastIndex *)
        regexp.lastIndex <- start_index + length;
        (* Check if lre_get_groupnames are enabled and
           if (group_name_ptr && i > 0) {
                 if (*group_name_ptr) {
                     if (JS_DefinePropertyValueStr(ctx, groups, group_name_ptr,
                                                   JS_DupValue(ctx, val),
                                                   prop_flags) < 0) {
                         JS_FreeValue(ctx, val);
                         goto fail;
                     }
                 }
                 group_name_ptr += strlen(group_name_ptr) + 1;
             }
            *) *)
        i := !i + 2
      done;
      (* mutable lastIndex : int *)
      { captures = substrings }
  | 0 ->
      (* Printf.sprintf "nothing found" |> print_endline; *)
      { captures = [||] }
  | _ (* -1 *) -> raise (Invalid_argument "Error")
