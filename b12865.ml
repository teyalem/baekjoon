open Scanf

let knapsack k items =
  let l = Array.length items in
  let a = Array.make_matrix (k+1) l ~-1 in
  let rec aux k i =
    if i = l then 0 (* no items *)
    else if k <= 0 then 0 (* no strength *)
    else if a.(k).(i) <> -1 then a.(k).(i) (* memoized *)
    else
      let w, v = items.(i) in
      let wv = v + aux (k-w) (i+1) (* with item i *)
      and wov = aux k (i+1) in (* without item i *)
      let mv = if k-w < 0 then wov else max wv wov in
      begin a.(k).(i) <- mv; mv end (* memo and return *)
  in
  aux k 0

let zip a b = a, b

let () =
  let n, k = scanf " %d %d" zip in
  let items = Array.init n (fun _ -> scanf " %d %d" zip) in
  knapsack k items |> print_int
