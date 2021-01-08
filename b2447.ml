(* print stars in fractal shape
 * input: n where n = k^3 and 1 <= k < 8
 * output: unit
 *)
open List

type block = string list

let join: block -> block -> block =
  map2 (fun a b -> String.concat "" [a; b])

let rec combine: block list -> block = function
  | [] -> []
  | [x] -> x
  | x::xs ->
    let p = combine xs in
    join x p

let map_combine (xs: block list list): block
  = map combine xs |> flatten

let rec empty: int -> block = function
  | 3 -> [ "   "; "   "; "   " ]
  | n -> 
    let e = empty (n/3)
    in map_combine [[e; e; e]; [e; e; e]; [e; e; e;]]

let rec stars: int -> block = function
  | 3 -> [ "***"; "* *"; "***" ]
  | n ->
    let p = stars (n/3)
    and e = empty (n/3)
    in map_combine [[p; p; p]; [p; e; p]; [p; p; p]]

let rec print_block: block -> unit = function
  | [] -> ()
  | x::xs ->
    print_endline x;
    print_block xs

let _ =
  let n = read_int () in
  let b = stars n in
  print_block b
