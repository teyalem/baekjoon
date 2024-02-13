let is_pal str =
  let l = String.length str in
  let rec aux i j =
    if i >= j then 1, i+1
    else if str.[i] <> str.[j] then 0, i+1
    else aux (i+1) (j-1)
  in
  aux 0 (l-1)

let () =
  let n = read_int () in
  for _ = 1 to n do
    let i, k = is_pal @@ read_line () in
    Printf.printf "%d %d\n" i k
  done
