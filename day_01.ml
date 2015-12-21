open Core.Std

let () =
  In_channel.input_all stdin
  |> String.to_list
  |> List.fold ~init:0 ~f:(fun floor paren ->
    match paren with
    | '(' -> floor + 1
    | ')' -> floor - 1
    | _ -> floor
  )
  |> print_int