open Quickjs

let print_output (output : string array) =
  let len = Array.length output in
  Printf.printf "\noutput\n";
  for i = 0 to len - 1 do
    Printf.printf "%s\n" output.(i)
  done

let () =
  let re =
    RegExp.compile "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})" ~flags:"g"
  in
  match re with
  | Ok re ->
      let result = RegExp.exec re "Today's date is 2024-07-17" in
      print_output (RegExp.captures result)
  | Error (_, error) -> Printf.printf "Error: %s" error
