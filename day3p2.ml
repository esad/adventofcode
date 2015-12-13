open Core.Std

module CoordSet = Set.Make (struct
  type t = (int * int) with sexp, compare
end)

let visits commands =
  commands
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
  |> fst

let () =
  let map_tuple f (a,b) = (f a, f b) in
  let split_list list = List.fold_right ~init:([],[]) ~f:(fun x (l,r) -> x::r, l) list in
  In_channel.input_all stdin
  |> String.to_list
  |> split_list
  |> map_tuple visits
  |> Tuple2.uncurry CoordSet.union
  |> CoordSet.length
  |> print_int
