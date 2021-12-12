open List
let p = fold_left Int.mul 1
let c n r =
  let nom = init r ((+) @@ n-r+1) |> p
  and den = init r ((+) 1) |> p in
  nom / den
let () =
  Scanf.scanf "%d %d" c |> print_int
