open Core.Std

let increment word =
  let rec aux = function
    | [] -> []
    | 'z' :: xs -> 'a' :: (aux xs)
    | x :: xs -> (Char.to_int x + 1 |> Char.of_int_exn) :: xs
  in
    word |> List.rev |> aux |> List.rev

let check word =
  let check1 =
    List.group ~break:(fun a b -> Char.to_int a + 1 <> Char.to_int b) word
    |> List.count ~f:(fun x -> List.length x >= 3) > 0
  in
  let check2 =
    List.exists ~f:(fun x -> x = 'l' || x = 'o' || x = 'i') word |> (not)
  in
  let check3 =
    List.group ~break:(<>) word |> List.filter ~f:(fun x -> List.length x = 2)
    |> List.dedup 
    |> List.length >= 2
  in
  check1 && check2 && check3
  
let () = 
  let input = "hepxxyzz" in
  let rec find_from word =
    if check word then
      word
    else
      find_from (increment word)
  in
  input
  |> String.to_list
  |> increment
  |> find_from
  |> String.of_char_list
  |> print_endline
