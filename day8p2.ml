open Core.Std

let () = 
  let mapn f (a,b) = (f a, f b) in
  In_channel.fold_lines stdin ~init:0 ~f:(fun count line ->
    mapn String.length (String.escaped line, line)
    |> Tuple2.uncurry (-)
    |> (+) 2 (* quotes *)
    |> (+) count
  )
  |> print_int
