open Core.Std

let rle str =
  str |> String.to_list |> List.fold ~init:[] ~f:(fun results c ->
    match results with
    | (p,count) :: xs when p = c -> (p,count+1) :: xs
    | xs -> (c,1) :: xs
  )
  |> List.rev

let expand str =
  str
  |> rle
  |> List.concat_map ~f:(fun (c,count) -> [string_of_int count; String.of_char c])
  |> String.concat

assert (expand "1211" = "111221")
assert (expand "111221" = "312211")
assert (expand "21" = "1211")

let () =
  let rec iterate f n x =
    if n = 0 then x
    else iterate f (n - 1) (f x) in
  let input = "1321131112"
  in
    iterate expand 50 input
    |> String.length
    |> print_int
