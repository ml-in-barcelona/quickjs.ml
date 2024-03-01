let print_output (output : string array) =
  let len = Array.length output in
  Printf.printf "\noutput\n";
  for i = 0 to len - 1 do
    Printf.printf "%s\n" output.(i)
  done

let () =
  let output = RegExp.make "\\d" "m" "1ab9" in
  (* ["1", "9"] *)
  print_output output
