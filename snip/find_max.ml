let rec find_max f = function
  | [] -> None
  | x :: xs ->
    if f x then
      begin match find_max f xs with
        | None -> Some (x, xs)
        | o -> o
      end
    else
      None
