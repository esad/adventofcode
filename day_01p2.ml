open Core.Std

let () =
  In_channel.input_all stdin
  |> String.to_list
  |> List.fold ~init:(1,0,None) ~f:(fun (step, floor, basement) paren ->
    match (floor, paren, basement) with
    | (0, ')', None) -> (step+1, -1, Some step)
    | (_, '(', _) -> (step+1, floor + 1, basement)
    | (_, ')', _) -> (step+1, floor - 1, basement)
    | _ -> (step, floor, basement)
  )
  |> (fun (_, floor, basement) ->
    Printf.printf "Basement at step: %d End floor: %d\n" (Option.value ~default:(-1) basement) floor
  )