let solve n map =
  let dp =
    Array.init 3 (fun _ ->
        Array.make_matrix n n 0)
  in
  let get t r c =
    try dp.(t).(r).(c) with _ -> 0
  in
  let inc t r c n =
    try
      if map.(r).(c) = 0
      && (t <> 2 || (map.(r-1).(c) = 0
                     && map.(r).(c-1) = 0))
      then
        dp.(t).(r).(c) <- get t r c + n
    with _ -> ()
  in

  inc 0 0 1 1;
  for r = 0 to n-1 do
    for c = 0 to n-1 do
      inc 0  r    (c+1) @@ get 0 r c;
      inc 2 (r+1) (c+1) @@ get 0 r c;

      inc 1 (r+1)  c    @@ get 1 r c;
      inc 2 (r+1) (c+1) @@ get 1 r c;

      inc 0  r    (c+1) @@ get 2 r c;
      inc 1 (r+1)  c    @@ get 2 r c;
      inc 2 (r+1) (c+1) @@ get 2 r c;
    done
  done;

  List.init 3 (fun i -> get i (n-1) (n-1))
  |> List.fold_left Int.add 0

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let map = Array.init n (fun _ ->
      Array.init n (fun _ -> read ()))
  in
  solve n map |> print_int
