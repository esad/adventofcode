open Core.Std

(* Looks like we now have to actually parse the JSON. *)
let rec numbers = function
  | `List x -> List.concat_map ~f:numbers x
  | `Int x -> [x]
  | `Assoc kvs -> 
    kvs 
    |> List.map ~f:snd 
    |> (fun values -> if List.mem values (`String "red") then [] else numbers (`List values))
  | _ -> []

let () = 
  In_channel.input_all stdin
  |> Yojson.Basic.from_string
  |> numbers
  |> List.sum (module Int) ~f:ident
  |> print_int