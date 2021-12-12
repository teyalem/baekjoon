let f n = n mod 15746
let tile n =
  let rec aux a b i =
    if i = n then b
    else aux b (f (a+b)) (i+1)
  in
  aux 1 1 1
let () = read_int () |> tile |> print_int
