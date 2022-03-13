open List
open Scanf

let print (n, d) =
  Printf.printf "%d/%d\n" n d

let rec gcd n m =
  if n mod m = 0 then m else gcd m (n mod m)

let div k n =
  let m = gcd (max k n) (min k n) in
  k/m, n/m

let () =
  let n = scanf " %d" Fun.id in
  match init n (fun _ -> scanf " %d" Fun.id) with
  | k::ns -> map (div k) ns |> iter print
  | _ -> assert false
