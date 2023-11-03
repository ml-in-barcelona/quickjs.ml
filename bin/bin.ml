open Ctypes

let () =
  let plen = allocate int 0 in
  let error_msg = allocate_n ~count:64 char in
  let input = "asdf" in
  let input_length = String.length input |> Unsigned.Size_t.of_int in
  let flags = 0 in
  let bc =
    Libregexp.C.Functions.lre_compile plen error_msg 64 input input_length flags null
  in
  match is_null bc with
  | true -> print_endline "Error"
  | false ->
    let capture = CArray.make char 410 in
    let start = CArray.start capture in
    let start_capture = allocate (ptr char) start in
    let matching = "asdf" in
    let matching_length = String.length matching in
    let matched =
      Libregexp.C.Functions.lre_exec start_capture bc matching 0 matching_length 0 null
    in
    print_endline ("The result of the regexp is: " ^ Int.to_string matched)
;;
