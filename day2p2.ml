open Core.Std

let () =
  In_channel.fold_lines stdin ~init:0 ~f:(fun total line ->
    let (l, w, h) = Scanf.sscanf line "%dx%dx%d" (fun l w h -> (l, w, h)) in
    let sides = [l; w; h] in
    let volume = l * w * h in
    let min2_sides = List.take (List.sort ~cmp:compare sides) 2 in
    total + volume + (List.fold ~init:0 ~f:(+) min2_sides) * 2
  )
  |> print_int