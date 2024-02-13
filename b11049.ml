let ( *$ ) ((m, n), c1) ((_, k), c2) =
  (m, k), m*n*k + c1 + c2

let solve arr len =
  let m = Array.make_matrix len len ((0, 0), ~-1) in
  let get (l, h) = m.(l).(h)
  and set (l, h) v = m.(l).(h) <- v in

  for l = 0 to len - 1 do
    set (l, l) arr.(l)
  done;

  for i = 2 to len do
    for l = 0 to len - 1 - i do
      List.init i (fun j -> l, l+j+1, l+i-1)
    done
  done;

  get (0, len-1)

let () =
  let len = Scanf.scanf " %d" Fun.id in
  let arr = Array.init len
      (fun _ -> Scanf.scanf " %d %d" (fun x y -> x, y))
  in
  solve arr len |> print_int
