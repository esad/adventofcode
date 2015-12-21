open Core.Std

type wire_input = 
  | Value of string
  | And of string * string 
  | Or of string * string 
  | LShift of string * int
  | RShift of string * int
  | Not of string

let parse line =
  try Scanf.sscanf line "%s -> %s" (fun i o -> (Value i, o))
  with _ ->
  try Scanf.sscanf line "%s AND %s -> %s" (fun i1 i2 o -> (And (i1,i2), o))
  with _ ->
  try Scanf.sscanf line "%s OR %s -> %s" (fun i1 i2 o -> (Or (i1,i2), o))
  with _ ->
  try Scanf.sscanf line "%s LSHIFT %d -> %s" (fun i n o -> (LShift (i,n), o))
  with _ ->
  try Scanf.sscanf line "%s RSHIFT %d -> %s" (fun i n o -> (RShift (i,n), o))
  with _ ->
  Scanf.sscanf line "NOT %s -> %s" (fun i o -> (Not i, o))

let rec eval wiring wire_name =
  try int_of_string wire_name 
  with _ ->
    let (input, value) = Hashtbl.find_exn wiring wire_name in
    match value with
    | Some i -> i
    | None ->
      let result = match input with
      | Value v -> eval wiring v
      | And (i1, i2) -> (land) (eval wiring i1) (eval wiring i2)
      | Or (i1, i2) -> (lor) (eval wiring i1) (eval wiring i2)
      | LShift (i,n) -> (lsl) (eval wiring i) n
      | RShift (i,n) -> (lsr) (eval wiring i) n
      | Not i -> lnot (eval wiring i)
      in 
      let () = Hashtbl.set wiring ~key:wire_name ~data:(input,Some result)
      in result

let () = 
  let wiring = String.Table.create () in
  let () = In_channel.iter_lines stdin ~f:(fun line ->
    let (input,key) = parse line in
    Hashtbl.set wiring ~key ~data:(input,None)
  ) in
  eval wiring "a"
  |> print_int