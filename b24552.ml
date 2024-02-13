let solve str =
  let rec aux i c n =
    if i >= String.length str then n
    else
      let c = c + match str.[i] with '(' -> 1 | ')' -> -1 | _ -> 0 in
      let n = n + if i > 1 && (c = 1 || c = -1) then 1 else 0 in
      aux (i+1) c n
  in
  aux 0 0 0

let () =
  let str = read_line () in
  solve str |> print_int
