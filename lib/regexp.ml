type flags = {
  global : bool;
  ignore_case : bool;
  multiline : bool;
  sticky : bool;
  unicode : bool;
  unicode_sets : bool;
  dot_all : bool;
}

(* #define LRE_FLAG_GLOBAL     (1 << 0)
   #define LRE_FLAG_IGNORECASE (1 << 1)
   #define LRE_FLAG_MULTILINE  (1 << 2)
   #define LRE_FLAG_DOTALL     (1 << 3)
   #define LRE_FLAG_UNICODE    (1 << 4)
   #define LRE_FLAG_STICKY     (1 << 5)
   #define LRE_FLAG_INDICES    (1 << 6) /* Unused by libregexp, just recorded. */
   #define LRE_FLAG_NAMED_GROUPS (1 << 7) /* named groups are present in the regexp */ *)

type regex = { bc : Unsigned.uint8 Ctypes_static.ptr; flags : flags }
type result

let parse_flags flags =
  let rec parse_flags' flags acc =
    match flags with
    | [] -> acc
    | 'g' :: rest -> parse_flags' rest { acc with global = true }
    | 'i' :: rest -> parse_flags' rest { acc with ignore_case = true }
    | 'm' :: rest -> parse_flags' rest { acc with multiline = true }
    | 's' :: rest -> parse_flags' rest { acc with dot_all = true }
    | 'u' :: rest -> parse_flags' rest { acc with unicode = true }
    | 'y' :: rest -> parse_flags' rest { acc with sticky = true }
    | _ :: rest -> parse_flags' rest acc
  in
  parse_flags'
    (String.to_seq flags |> List.of_seq)
    {
      global = false;
      ignore_case = false;
      multiline = false;
      sticky = false;
      unicode = false;
      unicode_sets = false;
      dot_all = false;
    }

let compile re =
  let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
  let size_of_error_msg = 64 in
  let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
  let input = Ctypes.ocaml_string_start re in
  let input_length = String.length re |> Unsigned.Size_t.of_int in
  let flags = 0 in
  let compiled_byte_code =
    Libregexp.C.Functions.lre_compile compiled_byte_code_len error_msg
      size_of_error_msg input input_length flags Ctypes.null
  in
  match Ctypes.is_null compiled_byte_code with
  | true ->
      let error = Ctypes.string_from_ptr ~length:64 error_msg in
      print_endline error;
      raise (Invalid_argument "Compilation failed")
  | false ->
      {
        bc = compiled_byte_code;
        flags =
          {
            global = false;
            ignore_case = false;
            multiline = false;
            sticky = false;
            unicode = false;
            unicode_sets = false;
            dot_all = false;
          };
      }

(* exec is not a binding to lre_exec but an implementation of `js_regexp_exec` *)
let exec input regexp =
  let { bc; _ } = regexp in
  let capture_count = Libregexp.C.Functions.lre_get_capture_count bc in
  let capture = Ctypes.CArray.make Ctypes.uint8_t capture_count in
  let start = Ctypes.CArray.start capture in
  let start_capture = Ctypes.allocate (Ctypes.ptr Ctypes.uint8_t) start in
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
  Printf.printf "\nmatching_length %d\n" matching_length;
  let index = 0 in
  (* Return 1 if match, 0 if not match or -1 if error. cindex is the
     starting position of the match and must be such as 0 <= cindex <=
     clen. *)
  let exec_result =
    Libregexp.C.Functions.lre_exec start_capture bc buffer index matching_length
      0 Ctypes.null
  in
  Printf.printf "\ncapture_count %d\n" capture_count;
  match exec_result with
  | 1 ->
      capture
      |> Ctypes.CArray.iter (fun i ->
             Printf.sprintf "capture: %d" (Unsigned.UInt8.to_int i)
             |> print_endline);
      (* printd_intCtypes.CArray.length capture *)
      [||]
  | 0 ->
      Printf.sprintf "nothing found" |> print_endline;
      [||]
  | _ (* -1 *) -> raise (Invalid_argument "Error")

let make regex input =
  let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
  let size_of_error_msg = 64 in
  let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
  let regexp_input = Ctypes.ocaml_string_start regex in
  let regexp_length = String.length regex |> Unsigned.Size_t.of_int in
  let flags = 0 in
  let compiled_byte_code =
    Libregexp.C.Functions.lre_compile compiled_byte_code_len error_msg
      size_of_error_msg regexp_input regexp_length flags Ctypes.null
  in
  if Ctypes.is_null compiled_byte_code then (
    let error = Ctypes.string_from_ptr ~length:64 error_msg in
    print_endline error;
    [||])
  else
    let capture_count =
      Libregexp.C.Functions.lre_get_capture_count compiled_byte_code
    in
    Printf.printf "\ncapture_count %d\n" capture_count;
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
    Printf.printf "\nmatching_length %d\n" matching_length;
    let index = 0 in
    let exec_result =
      Libregexp.C.Functions.lre_exec start_capture compiled_byte_code buffer
        index matching_length 0 Ctypes.null
    in
    (* Return 1 if match, 0 if not match or -1 if error. cindex is the
       starting position of the match and must be such as 0 <= cindex <=
       clen. *)
    match exec_result with
    | 1 ->
        print_endline (Printf.sprintf "capture_size: %d" capture_size);
        (* capture
           |> Ctypes.CArray.iter (fun p ->
                  let i = Ctypes.( !@ ) p in
                  (* let start_ptr = Ctypes.CArray.get capture i in *)
                  (* let end_ptr = Ctypes.CArray.get capture (i + 1) in *)
                  let start_index = Unsigned.UInt8.to_int start_ptr in
                  let end_index = Unsigned.UInt8.to_int end_ptr in
                  let substring =
                    String.sub matching start_index (end_index - start_index)
                  in
                  substrings.(i / 2) <- substring;
                  Printf.sprintf "capture: %d" (Unsigned.UInt8.to_int i)
                  |> print_endline); *)
        (* Don't make an array of 0 *)
        let substrings = Array.make capture_count "" in

        let i = ref 0 in
        while !i < capture_size - 1 do
          let start_ptr = Ctypes.CArray.get capture !i in
          let end_ptr = Ctypes.CArray.get capture (!i + 1) in
          let start_index = Ctypes.ptr_diff buffer start_ptr in
          let length = Ctypes.ptr_diff start_ptr end_ptr in
          print_endline (Printf.sprintf "start_index: %d" start_index);
          print_endline (Printf.sprintf "len: %d" length);

          let substring = String.sub input start_index length in
          substrings.(!i / 2) <- substring;

          (*  *)
          i := !i + 2
        done;
        substrings
    | 0 ->
        Printf.sprintf "nothing found" |> print_endline;
        [||]
    | _ (* -1 *) -> raise (Invalid_argument "Error")
