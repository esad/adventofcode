open Core.Std

type coord = int * int
type range = coord * coord
type command = Toggle of range | On of range | Off of range

exception InvalidCommand of string

let parse line =
  Scanf.sscanf line "%[a-z ] %d,%d through %d,%d" (fun cmd ax ay bx by -> 
    let range = ((ax,ay), (bx,by)) in
    match String.strip cmd with
    | "turn on" -> On range
    | "turn off" -> Off range
    | "toggle" -> Toggle range
    | _ as cmd' -> raise (InvalidCommand cmd')
  )

let execute field cmd = 
  let always v _ = v in
  let sum_range range f =
    let ((ax, ay), (bx, by)) = range in
    List.cartesian_product (List.range ax (bx+1)) (List.range ay (by+1)) |> List.fold ~init:0 ~f:(fun count coord -> count + f coord)
  in
  let int_of_bool b = if b then 1 else 0
  in
  let update = match cmd with
    | On _ -> always true
    | Off _ -> always false
    | Toggle _ -> (fun state -> not state)
  in match cmd with
    | On range | Off range | Toggle range -> sum_range range (fun (x,y) ->
      let before = field.(x).(y) in
      let after = update before in
      let () = field.(x).(y) <- after in
      int_of_bool after - int_of_bool before
    )

let () = 
  let field = Array.make_matrix ~dimx:1000 ~dimy:1000 false in
  In_channel.fold_lines stdin ~init:0 ~f:(fun count line ->
    parse line
    |> execute field
    |> (+) count
  )
  |> print_int
