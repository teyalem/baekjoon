let sum = List.fold_left Int.add 0

let inf = max_int/2

let solve tree n =
  let dp = Array.make_matrix (n+1) 2 inf in

  let children p i =
    List.filter (fun n -> n <> p)
      tree.(i)
  in

  let rec aux c p i =
    if dp.(i).(c) < inf then dp.(i).(c)
    else begin
      let r =
        if c = 0 then
          sum @@ List.map (aux 1 i) @@ children p i
        else
          1 + (sum @@ List.map (fun n -> min (aux 0 i n) (aux 1 i n)) @@ children p i)
      in
      dp.(i).(c) <- r;
      r
    end
  in

  min (aux 0 0 1) (aux 1 0 1)

let () =
  Scanf.scanf "%d" @@ fun n ->
  let t = Array.make (n+1) [] in
  for _ = 2 to n do
    Scanf.scanf " %d %d" @@ fun u v ->
    t.(u) <- v :: t.(u);
    t.(v) <- u :: t.(v)
  done;
  solve t n |> print_int
