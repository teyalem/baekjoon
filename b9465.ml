let solve n arr =
  (* 0 up 1 down 2 nothing *)
  let dp = Array.make_matrix n 3 min_int in
  let get i j = arr.(i+j*n) in
  for i = 0 to 1 do
    dp.(0).(i) <- get 0 i
  done;
  dp.(0).(2) <- 0;

  for i = 1 to n-1 do
    dp.(i).(0) <- get i 0 + max dp.(i-1).(1) dp.(i-1).(2);
    dp.(i).(1) <- get i 1 + max dp.(i-1).(0) dp.(i-1).(2);
    dp.(i).(2) <- max dp.(i-1).(0)
      @@ max dp.(i-1).(1) dp.(i-1).(2);
  done;

  Array.fold_left max min_int dp.(n-1)

let read () = Scanf.scanf " %d" Fun.id

let () =
  let t = read () in
  for _ = 1 to t do
    let n = read () in
    let arr = Array.init (2*n) (fun _ -> read ()) in
    solve n arr
    |> Printf.printf "%d\n"
  done
