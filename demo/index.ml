open Quickjs

let print_output (output : string array) =
  let len = Array.length output in
  Printf.printf "\noutput\n";
  for i = 0 to len - 1 do
    Printf.printf "%s\n" output.(i)
  done

let () =
  let re = RegExp.compile "\\d" "g" in
  let result = RegExp.exec re "a2b3c4d5e6f7g8h9i" in
  print_output (RegExp.captures result);
  let result = RegExp.exec re "a2b3c4d5e6f7g8h9i" in
  print_output (RegExp.captures result)
