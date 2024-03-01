let print_output (output : string array) =
  let len = Array.length output in
  Printf.printf "\noutput\n";
  for i = 0 to len - 1 do
    Printf.printf "%s\n" output.(i)
  done

let () =
  let output = RegExp.make "\\d" "ab9" in
  (* ["8", "1", "3", "1", "2", "3"] *)
  print_output output
