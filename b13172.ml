let pow a b m =
  let rec aux i p acc =
    if i = 0 then acc
    else
      aux (i/2) ((p*p) mod m)
        (if i mod 2 = 0 then acc
         else (p*acc) mod m)
  in
  aux b a 1

let modular = 1_000_000_007

let invmod n =
  pow n (modular-2) modular

let rec gcd n m =
  if n mod m = 0 then m else gcd m (n mod m)

let frac n m =
  let g = gcd n m in
  n/g, m/g

let expect (n, s) =
  frac s n

let add (a, b) (c, d) =
  let f n = n mod modular in
  let nom = f (f (a*d) + f (c*b))
  and den = f (b*d) in
  nom, den

let solve ds =
  let nom, den =
    List.map expect ds
    |> List.fold_left add (0, 1)
    |> (fun (d, n) -> frac d n)
  in
  if den = 1 then nom
  else (nom * invmod den) mod modular

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let ds = List.init n (fun _ ->
      Scanf.scanf " %d %d" @@ fun n s -> n, s)
  in
  solve ds |> print_int
