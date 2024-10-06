let pow a b m =
  let rec aux i p acc =
    if i = 0 then acc
    else
      aux (i/2) ((p*p) mod m)
        (if i mod 2 = 0 then acc
         else (p*acc) mod m)
  in
  aux b a 1

let () =
  Scanf.scanf "%d %d %d" (fun a b m ->
      pow a b m |> print_int)
