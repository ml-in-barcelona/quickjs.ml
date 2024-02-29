let print_output (output : (int * int) array) =
  let len = Array.length output in
  Printf.printf "\noutput\n";
  for i = 0 to len - 1 do
    let start, end_ = output.(i) in
    Printf.printf "%d %d\n" start end_
  done

let () =
  let output = RegExp.compile "\\d" |> RegExp.exec "abc123xyz123xyz" in
  print_output output
