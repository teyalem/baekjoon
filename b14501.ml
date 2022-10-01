open Array

let solve n arr =
  let f = make n 0 in
  for i = n - 1 downto 0 do
    let t, p = arr.(i) in
    if i + t > n then
      f.(i) <- 0
    else
      let m = fold_left max 0 @@ sub f (i + t) (n - i - t) in
      f.(i) <- p + m
  done;
  fold_left max 0 f

let () =
  let n = read_int () in
  let arr = init n (fun _ -> Scanf.scanf " %d %d" (fun t p -> t, p)) in
  solve n arr |> print_int
