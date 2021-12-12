let m = 1_000_000_000

let nstair arr = function
  | 0 -> arr.(1)
  | 9 -> arr.(8)
  | i -> (arr.(i-1) + arr.(i+1)) mod m

let numstair n =
  let rec aux i arr =
    if i = n then arr
    else aux (i+1) @@ Array.init 10 (nstair arr)
  in
  aux 1 @@ Array.init 10 (function 0 -> 0 | _ -> 1)

let () =
  read_int ()
  |> numstair
  |> Array.fold_left (fun p n -> (p+n) mod m) 0
  |> print_int
