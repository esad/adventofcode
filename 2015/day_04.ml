open Core.Std

let input = "yzbqklnj"

let md5 prefix x =
  string_of_int x
  |> (^) prefix
  |>  Fn.compose Digest.to_hex Digest.string 

let rec mine x prefix =
  let result = md5 input x in
  if String.is_prefix ~prefix:prefix result then
    x
  else
    mine (x+1) prefix

let () =
  let p1 = mine 0 "00000" in
  let p2 = mine 0 "000000" in
  Printf.printf "Part 1: %d Part 2: %d\n" p1 p2
