open Array

let dp = [ 0, 1; 1, 0; 0, -1; -1, 0 ]

let () =
  let m, n = Scanf.scanf " %d %d" (fun m n -> m, n) in
  let mat = init m (fun _ ->
      init n (fun _ -> Scanf.scanf " %d" Fun.id))
  and ways = make_matrix m n ~-1 in
  ways.(m-1).(n-1) <- 1;

  let is_in (y, x) = 0 <= y && y < m && 0 <= x && x < n in
  let lower (y1, x1) (y2, x2) = mat.(y1).(x1) < mat.(y2).(x2) in
  let rec walk (y, x) =
    if ways.(y).(x) > -1 then (* visited path *)
      ways.(y).(x)
    else begin
      let nw =
        List.map (fun (dy, dx) -> y+dy, x+dx) dp
        |> List.filter (fun p -> is_in p && lower p (y, x))
        |> List.map walk
        |> List.fold_left Int.add 0
      in
      ways.(y).(x) <- nw; (* memo *)
      nw
    end
  in
  print_int @@ walk (0, 0)
