open Array

let zero = make 10 0
let zn = make 10 1

let zd n = init 10 (fun i ->
    if 0 <= i && i <= n then 1 else 0)

let only d = init 10 (fun i ->
    if i = d then 1 else 0)

let ( +$ ) a b = map2 Int.add a b
let ( *$ ) m arr = map (Int.mul m) arr

let ds n =
  let rec aux o =
    if o > n then zero
    else
      let d = (n/o) mod 10 in
      let n, m = n/(10*o), n mod o in
      aux (10*o) +$
      (n*o) *$ zn +$
      o *$ zd (d-1) +$
      (m+1) *$ only d +$
      ~-o *$ zd 0
  in
  aux 1

let () =
  read_int ()
  |> ds
  |> iteri (fun i n ->
      print_int n;
      if i < 9 then print_char ' ')
