open Core.Std

(* Naive and input specific solution: Input seems to have <10 cities and each city is connected to each other,
   so let's just generate permutations of all cities and see which route is the cheapest (it works!). *)

module StringSet = Set.Make(String)
module ConnectionMap = Map.Make(struct type t = string * string with sexp, compare let compare = compare end)

let rec permutations lst = 
  let rec interleave x lst = 
    match lst with
    | [] -> [[x]]
    | hd::tl -> (x::lst) :: (List.map ~f:(fun y -> hd::y) (interleave x tl))
  in
  match lst with
  | hd::tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))
  | _ -> [lst]

let rec walk cities connections =
  match cities with
  | x::(y::_ as rest) -> ConnectionMap.find_exn connections (x,y) + walk rest connections
  | _ -> 0

let () = 
  let (cities, connections) = In_channel.fold_lines stdin ~init:(StringSet.empty,ConnectionMap.empty) ~f:(fun (cities,connections) line ->
    Scanf.sscanf line "%s to %s = %d" (fun src dest d -> 
      ( Set.add (Set.add cities src) dest
      , ConnectionMap.add ~key:(src,dest) ~data:d connections |> ConnectionMap.add ~key:(dest,src) ~data:d
      )
    )
  ) in
  Set.to_list cities 
  |> permutations 
  |> List.fold ~init:None ~f:(fun best p ->
    let cost = walk p connections in
    match best with 
    | None -> Some cost
    | Some cost' -> Some (min cost cost')
  )
  |> Option.value ~default:0
  |> print_int
  