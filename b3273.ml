let solve x arr len =
  let rec aux c l r =
    if l >= r then c
    else
      let n = arr.(l) + arr.(r) in
      if n < x then aux c (l+1) r
      else if n > x then aux c l (r-1)
      else (* n = x *) aux (c+1) (l+1) (r-1)
  in
  aux 0 0 (len-1)

let () =
  let n = Scanf.scanf " %d" Fun.id in
  let arr = Array.init n (fun _ -> Scanf.scanf " %d" Fun.id) in
  let x = Scanf.scanf " %d" Fun.id in
  Array.sort Int.compare arr;
  solve x arr n |> print_int
