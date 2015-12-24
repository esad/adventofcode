open Core.Std

(* Just a dumb scan for all numbers in the input *)
let numbers str  =
  let re = Str.regexp "\\(-?[0-9]+\\)" in
  let rec aux pos matches =
    match Str.search_forward re str pos with
      | exception Not_found -> matches
      | _  -> 
        let result = (Str.matched_group 0 str) |> int_of_string in
        aux (Str.match_end()) (result :: matches)
  in
    aux 0 []

let () = 
  In_channel.input_all stdin
  |> numbers
  |> List.sum (module Int) ~f:ident
  |> print_int