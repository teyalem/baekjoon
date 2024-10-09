let rec split_while f = function
  | [] -> [], []
  | x :: xs ->
    if f x then
      let ps, xs = split_while f xs in
      x::ps, xs
    else
      [], x::xs
