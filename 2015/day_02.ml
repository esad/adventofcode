open Core.Std

let () =
  In_channel.fold_lines stdin ~init:0 ~f:(fun total line ->
    let (l, w, h) = Scanf.sscanf line "%dx%dx%d" (fun l w h -> (l, w, h)) in
    let sides = [l * w; w * h; h * l] in
    let min_side = Option.value ~default:0 (List.min_elt ~cmp:compare sides) in
    total + min_side + (List.fold ~init:0 ~f:(+) sides) * 2
  )
  |> print_int