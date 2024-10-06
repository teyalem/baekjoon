let find a b =
  let rec aux i n =
    if n < a then -1
    else if n = a then i + 1
    else if n mod 10 = 1 then
      aux (i+1) (n/10)
    else if n mod 2 = 1 then -1
    else
      aux (i+1) (n/2)
  in
  aux 0 b

let () =
  Scanf.scanf "%d %d" @@ fun a b ->
  find a b |> print_int
