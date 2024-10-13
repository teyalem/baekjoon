let row mat i = mat.(i)
let col mat i = Array.(init (length mat) (fun j -> mat.(j).(i)))

let dotmod a b m =
  Array.map2 (fun a b -> (a*b) mod m) a b
  |> Array.fold_left (fun a b -> (a+b) mod m) 0

let mulmod a b m =
  let dim1 = Array.length a
  and dim2 = Array.length b.(0) in
  Array.init dim1 (fun y ->
      Array.init dim2 (fun x ->
          dotmod (row a y) (col b x) m))

let u a =
  let dim1 = Array.length a
  and dim2 = Array.length a.(0) in
  Array.init dim1 (fun i ->
      Array.init dim2 (fun j ->
          if i=j then 1 else 0))

let pow a b m =
  let rec aux i p acc =
    if i = 0 then acc
    else
      aux (i/2) (mulmod p p m)
        (if i mod 2 = 0 then acc
         else (mulmod p acc m))
  in
  aux b a (u a)

let () =
  Scanf.scanf "%d %d" @@ fun n b ->
  let mat =
    Array.init n (fun _ ->
        Array.init n (fun _ ->
            Scanf.scanf " %d" Fun.id))
  in
  pow mat b 1000
  |> Array.iter (fun row ->
      Array.iter (Printf.printf "%d ") row;
      Printf.printf "\n")
