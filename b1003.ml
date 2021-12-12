let countfib n =
  let rec aux (a0, a1) (b0, b1) i =
    if i = n
    then b0, b1
    else aux (b0, b1) (a0+b0, a1+b1) (i+1)
  in
  if n = 0 then 1, 0 else aux (1, 0) (0, 1) 1

let () =
  let t = read_int () in
  for _ = 1 to t do
    let z, o = read_int () |> countfib in
    Printf.printf "%d %d\n" z o
  done
