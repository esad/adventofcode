open Core.Std

let unescape str =
  Str.global_replace (Str.regexp "\\\\\\\\") "*" str
  |> Str.global_replace (Str.regexp "\\\\\"") "\""
  |> Str.global_replace (Str.regexp "\\\\x..") "*"
  |> Str.global_replace (Str.regexp "^\"") ""
  |> Str.global_replace (Str.regexp "\"$") ""

let () = 
  let mapn f (a,b) = (f a, f b) in
  In_channel.fold_lines stdin ~init:0 ~f:(fun count line ->
    mapn String.length (line, unescape line)
    |> Tuple2.uncurry (-)
    |> (+) count
  )
  |> print_int
