open Core.Std

let matchcount re str =
  let rec matchcount' re str count pos = 
    match Str.search_forward re str pos with
    | exception Not_found -> count
    | _ as pos' -> matchcount' re str (count+1) (pos'+1)
  in
  matchcount' re str 0 0

let is_nice word = 
  let rule1 = Str.regexp "[aeiou]" in
  let rule2 = Str.regexp "\\(.\\)\\1" in
  let rule3 = Str.regexp "\\(ab\\|cd\\|pq\\|xy\\)" in
  matchcount rule1 word >= 3 &&
  matchcount rule2 word >= 1 &&
  matchcount rule3 word = 0

let () =
  In_channel.fold_lines stdin ~init:0 ~f:(fun count line ->
    match is_nice line with
    | true -> count + 1
    | false -> count
  )
  |> print_int
  