let padovan n =
  let rec aux a b c i =
    if i = n then c
    else aux b c (a+b) (i+1)
  in
  aux 1 0 0 2
let () =
  for _ = 1 to read_int () do
    read_int () + 4 |> padovan |> Printf.printf "%d\n"
  done
