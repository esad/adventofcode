open Core.Std

module CoordSet = Set.Make (struct
  type t = (int * int) with sexp, compare
end)

let () =
  In_channel.input_all stdin
  |> String.to_list
  |> List.fold ~init:(CoordSet.of_list [(0,0)],(0,0)) ~f:(fun (visited, (x,y)) cmd ->
    let next = match cmd with
    | '^' -> (x,y-1)
    | 'v' -> (x,y+1)
    | '<' -> (x-1, y)
    | '>' -> (x+1, y)
    | _ -> (x,y)
    in
    (CoordSet.add visited next, next)
  )
  |> fst |> CoordSet.length
  |> print_int