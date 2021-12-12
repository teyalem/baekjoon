let set_early h m =
  if m < 45
  then (if h = 0 then 23 else h-1), m+15
  else h, m-45

let () =
  let h, m = Scanf.scanf "%d %d" set_early in
  Printf.printf "%d %d" h m
