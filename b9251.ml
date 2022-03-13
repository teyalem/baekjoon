let solve a b =
  let la = String.length a and lb = String.length b in
  let m = Array.make_matrix la lb ~-1 in
  let rec aux i j =
    if i < la && j < lb then
      if m.(i).(j) <> ~-1 then m.(i).(j)
      else
        let r =
          if a.[i] = b.[j]
          then 1 + aux (i+1) (j+1)
          else max (aux (i+1) j) (aux i (j+1))
        in
        begin m.(i).(j) <- r; r end
    else
      0
  in
  aux 0 0

let () =
  let a = read_line () in
  let b = read_line () in
  solve a b |> print_int
