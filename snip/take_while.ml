let rec take_while f = function
  | [] -> []
  | x :: xs ->
    if f x then x :: take_while f xs
    else []
