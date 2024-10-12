module M = Map.Make(Int)

let solve graph items n m =
  let dist = Array.make_matrix (n+1) (n+1) (max_int/2) in

  List.iter (fun (s, e, w) ->
      dist.(s).(e) <- w;
      dist.(e).(s) <- w)
    graph;

  for v = 1 to n do
    dist.(v).(v) <- 0
  done;

  for k = 1 to n do
    for i = 1 to n do
      for j = 1 to n do
        dist.(i).(j) <- min dist.(i).(j) (dist.(i).(k) + dist.(k).(j));
      done
    done
  done;

  List.init n (fun i ->
      let i = i+1 in
      let ni = ref 0 in
      for j = 1 to n do
        if dist.(i).(j) <= m then
          ni := !ni + items.(j-1)
      done;
      !ni)
  |> List.fold_left max min_int

open Scanf

let () =
  scanf " %d %d %d" @@ fun n m r ->
  let items = Array.init n (fun _ ->
      scanf " %d" Fun.id)
  in
  let graph = List.init r (fun _ ->
      scanf " %d %d %d" (fun a b l -> a, b, l))
  in
  solve graph items n m |> print_int
